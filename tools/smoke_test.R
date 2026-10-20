# End-to-end test of the package against the OpenHEXA demo server (https://app.demo.openhexa.org).
#
# The unit tests mock the API; this script calls the demo server for real, tier by tier, and reports what works. It
# always targets the demo server, never production, whatever the HEXA_* environment variables say. Usage:
#
#   OPENHEXA_DEMO_TOKEN=... OPENHEXA_DEMO_WORKSPACE=my-workspace make smoke
#
# The token is a demo API token, from your account settings on the demo server. Without it, an interactive
# session prompts for it. Without a workspace, the first one you have access to is used. Options:
#   OPENHEXA_SMOKE_WRITE=true      also upload a file, create a dataset and a version, publish (then delete) a web app
#                                  and create a notebook, all prefixed with "smoke-test-". Datasets and files can't be
#                                  deleted from R: remove them from the web interface.
#   OPENHEXA_SMOKE_PIPELINE=code   also run this pipeline, with its default parameters, and wait for it.

if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  library(openhexa)
}

demo_url <- "https://api.demo.openhexa.org"

# Don't pick up a production configuration from the environment.
Sys.unsetenv(c("HEXA_SERVER_URL", "HEXA_TOKEN", "HEXA_WORKSPACE"))

write <- identical(tolower(Sys.getenv("OPENHEXA_SMOKE_WRITE")), "true")
pipeline <- Sys.getenv("OPENHEXA_SMOKE_PIPELINE")
suffix <- format(Sys.time(), "%Y%m%d-%H%M%S")

results <- list()

# Runs one step, prints its result, and keeps going on failure.
step <- function(label, expr) {
  out <- tryCatch(
    {
      value <- force(expr)
      cli::cli_alert_success(label)
      list(ok = TRUE, value = value)
    },
    error = function(e) {
      cli::cli_alert_danger("{label}: {conditionMessage(e)}")
      list(ok = FALSE, value = NULL)
    }
  )
  results[[label]] <<- out$ok
  invisible(out$value)
}

skip <- function(label, why) {
  cli::cli_alert_info("{label}: skipped ({why})")
}

first <- function(x, column) if (NROW(x) > 0) x[[column]][[1]]

# Tier 0: authenticate and explore ------------------------------------------------------------------------------------

cli::cli_h1("Tier 0: authenticate and explore")

token <- Sys.getenv("OPENHEXA_DEMO_TOKEN")
if (token == "" && !interactive()) cli::cli_abort("Set {.envvar OPENHEXA_DEMO_TOKEN} to a demo API token.")
user <- step("login()", login(token = if (token != "") token, server_url = demo_url, verify = TRUE))
if (is.null(user)) quit(status = 1)
step("get_user()", get_user())
workspaces <- step("list_workspaces()", list_workspaces())
workspace <- Sys.getenv("OPENHEXA_DEMO_WORKSPACE")
if (workspace == "") workspace <- first(workspaces, "slug")
if (is.null(workspace)) cli::cli_abort("No workspace available on the demo server.")
step(sprintf("set_workspace(\"%s\")", workspace), set_workspace(workspace))
step("get_workspace()", get_workspace())

datasets <- step("list_datasets()", list_datasets())
files <- step("list_files()", list_files())
queries <- step("list_saved_queries()", list_saved_queries())
step("list_webapps()", list_webapps())
pipelines <- step("list_pipelines()", list_pipelines())
connections <- step("list_connections()", list_connections())

# Tier 1: get data ----------------------------------------------------------------------------------------------------

cli::cli_h1("Tier 1: get data")

dataset <- first(datasets, "slug")
if (is.null(dataset)) {
  skip("Datasets", "no dataset in the workspace")
} else {
  step(sprintf("get_dataset(\"%s\")", dataset), get_dataset(dataset))
  step("list_dataset_versions()", list_dataset_versions(dataset))
  dataset_files <- step("list_dataset_files()", list_dataset_files(dataset))
  tabular <- grep("\\.(csv|tsv|parquet|xlsx?)$", dataset_files$filename, value = TRUE)
  if (length(tabular) == 0) {
    skip("get_dataset_file()", "no tabular file in the latest version")
  } else {
    data <- step(sprintf("get_dataset_file(\"%s\")", tabular[[1]]), get_dataset_file(dataset, tabular[[1]]))
    if (!is.null(data)) print(utils::head(data, 3))
  }
}

file <- if (!is.null(files)) first(files[files$type == "file", ], "path")
if (is.null(file)) {
  skip("download_file()", "no file at the root of the workspace")
} else {
  step(sprintf("download_file(\"%s\")", file), download_file(file, dest = tempfile(), overwrite = TRUE))
}

query <- first(queries, "slug")
if (is.null(query)) {
  skip("run_saved_query()", "no saved query in the workspace")
} else {
  rows <- step(sprintf("run_saved_query(\"%s\")", query), run_saved_query(query, max_rows = 10))
  if (!is.null(rows)) print(rows)
}

if (requireNamespace("RPostgres", quietly = TRUE)) {
  step("connect_database()", {
    con <- connect_database()
    tables <- DBI::dbListTables(con)
    DBI::dbDisconnect(con)
    tables
  })
} else {
  skip("connect_database()", "RPostgres is not installed")
}

# Tiers 2 and 3: write ------------------------------------------------------------------------------------------------

cli::cli_h1("Tiers 2 and 3: publish and write")

if (!write) {
  skip("Writes", "set OPENHEXA_SMOKE_WRITE=true to run them")
} else {
  local <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(district = c("A", "B"), cases = c(1, 2)), local, row.names = FALSE)
  remote <- sprintf("smoke-test-%s.csv", suffix)

  step("upload_file()", upload_file(local, remote))
  step("get_file() returns the uploaded data", {
    back <- get_file(remote)
    stopifnot(identical(back$cases, c(1, 2)))
  })

  created <- step("create_dataset()", create_dataset(sprintf("smoke-test-%s", suffix), description = "Smoke test"))
  if (!is.null(created)) {
    step("create_dataset_version()", create_dataset_version(created$slug, "v1"))
    step("add_dataset_file()", add_dataset_file(created$slug, local, "cases.csv"))
    step("get_dataset_data() returns the added data", {
      back <- get_dataset_data(created$slug)
      stopifnot(identical(back$cases, c(1, 2)))
    })
  }

  app_name <- sprintf("smoke-test-%s", suffix)
  app <- step("publish_webapp() creates", publish_webapp(app_name, "<h1>Smoke test</h1>"))
  if (!is.null(app)) {
    step("publish_webapp() updates", publish_webapp(app_name, "<h1>Smoke test, updated</h1>"))
    step("delete_webapp()", delete_webapp(app$slug))
  }

  step("create_notebook()", create_notebook(sprintf("smoke-test-%s.ipynb", suffix), c(
    markdown = "# Smoke test",
    "library(openhexa)",
    "list_datasets()"
  )))
}

# Tier 4: pipelines and connections -----------------------------------------------------------------------------------

cli::cli_h1("Tier 4: pipelines and connections")

code <- first(pipelines, "code")
if (is.null(code)) {
  skip("Pipelines", "no pipeline in the workspace")
} else {
  step(sprintf("get_pipeline(\"%s\")", code), get_pipeline(code))
  step("list_pipeline_runs()", list_pipeline_runs(code, n = 5))
}

if (pipeline == "") {
  skip("run_pipeline()", "set OPENHEXA_SMOKE_PIPELINE=<code> to run a pipeline")
} else {
  run <- step(sprintf("run_pipeline(\"%s\", wait = TRUE)", pipeline), run_pipeline(pipeline, wait = TRUE))
  if (!is.null(run)) cli::cli_text("Run status: {.val {run$status}}")
}

connection <- first(connections, "identifier")
if (is.null(connection)) {
  skip("get_connection()", "no connection in the workspace")
} else {
  step(sprintf("get_connection(\"%s\")", connection), get_connection(connection))
}

# Summary -------------------------------------------------------------------------------------------------------------

failed <- names(results)[!unlist(results)]
cli::cli_h1("Summary")
cli::cli_text("{sum(unlist(results))} step{?s} passed, {length(failed)} failed.")
if (length(failed) > 0) {
  cli::cli_bullets(stats::setNames(failed, rep("x", length(failed))))
  quit(status = 1)
}
