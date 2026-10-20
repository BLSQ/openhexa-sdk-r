# Every test runs against a fake OpenHEXA server, whatever the configuration of the machine: no request leaves R.
#
# The server knows nothing by default: each test declares the answers it needs, and gets the requests it sent.
#
#   sent <- server$answer(Workspace = list(workspace = test_workspace()))
#   get_workspace()
#   expect_equal(sent$calls[[1]]$variables$slug, "test-workspace")
#
# An answer is the `data` to return, or a function of the variables (and of the request) returning it. Files to
# download are declared with `server$serve()`. Both last until the end of the test.

# The fake server, with the URL of its web app. The .example domain is reserved (RFC 2606): it can't belong to anyone.
test_server_url <- "https://api.openhexa.example"
test_app_url <- "https://app.openhexa.example"
# Signed URLs of files point here. .test is reserved too.
test_storage_url <- "https://storage.test"

server <- local({
  answers <- list()
  files <- list()
  recorders <- list()

  until_end_of_test <- function(undo, env) withr::defer(undo(), envir = env)

  # Answers a request: GraphQL operations from the declared answers; uploads to the storage are accepted.
  respond <- function(req) {
    if (startsWith(req$url, test_storage_url)) {
      for (recorder in recorders) recorder$uploads[[length(recorder$uploads) + 1]] <- req
      return(httr2::response(201))
    }
    if (req$url != paste0(test_server_url, "/graphql/")) stop("Unexpected request to ", req$url)

    body <- req$body$data
    op <- sub("^(query|mutation) ", "", regmatches(body$query, regexpr("(query|mutation) [A-Za-z]+", body$query)))
    for (recorder in recorders) recorder$calls[[length(recorder$calls) + 1]] <- list(op = op, variables = body$variables)

    answer <- answers[[op]]
    if (is.null(answer)) stop("No answer for the operation ", op, ": declare it with server$answer().")
    data <- if (!is.function(answer)) {
      answer
    } else if (length(formals(answer)) > 1) {
      answer(body$variables, req)
    } else {
      answer(body$variables)
    }
    # Serialize NULL as null, like the API does (httr2::response_json() turns it into {}).
    json <- jsonlite::toJSON(list(data = data), auto_unbox = TRUE, null = "null", digits = NA)
    httr2::response(headers = list(`Content-Type` = "application/json"), body = charToRaw(json))
  }

  # Signed URL downloads write straight to disk, which mocked responses can't do: write the declared file instead.
  download <- function(url, dest) {
    if (is.null(files[[url]])) stop("No file at ", url, ": declare it with server$serve().")
    writeLines(files[[url]], dest)
    invisible(dest)
  }

  list(
    respond = respond,
    download = download,
    # Declares answers by operation name, and returns the requests sent from then on: `calls` holds the GraphQL
    # operations with their variables, `uploads` the file uploads.
    answer = function(..., .env = parent.frame()) {
      previous <- answers
      answers[names(list(...))] <<- list(...)
      until_end_of_test(function() answers <<- previous, .env)

      recorder <- new.env()
      recorder$calls <- list()
      recorder$uploads <- list()
      id <- format(recorder)
      recorders[[id]] <<- recorder
      until_end_of_test(function() recorders[[id]] <<- NULL, .env)
      invisible(recorder)
    },
    # Declares the content of the file at a signed URL.
    serve = function(url, content, .env = parent.frame()) {
      previous <- files
      files[[url]] <<- content
      until_end_of_test(function() files <<- previous, .env)
      invisible()
    }
  )
})

# The configuration of the machine, as OpenHEXA notebooks provide it through environment variables. Each helper lasts
# until the end of the test; NULL means "not provided".

# Provides the server, the token and the workspace (by default, those of the fake server), and starts logged out.
provide_credentials <- function(server_url = test_server_url, token = "test-token", workspace = "test-workspace",
                                verify_ssl = NULL, .env = parent.frame()) {
  withr::local_envvar(
    HEXA_SERVER_URL = server_url %||% NA, HEXA_TOKEN = token %||% NA, HEXA_WORKSPACE = workspace %||% NA,
    HEXA_VERIFY_SSL = if (is.null(verify_ssl)) NA else tolower(verify_ssl),
    .local_envir = .env
  )
  logout()
  withr::defer(logout(), envir = .env)
}

# Mounts the files of the workspace at `path`, as in notebooks.
provide_files <- function(path = NULL, tmp_path = NULL, .env = parent.frame()) {
  withr::local_envvar(WORKSPACE_FILES_PATH = path %||% NA, WORKSPACE_TMP_PATH = tmp_path %||% NA, .local_envir = .env)
}

# Provides the credentials of the workspace database, as in notebooks.
provide_database <- function(host = NULL, port = NULL, username = NULL, password = NULL, db_name = NULL,
                             .env = parent.frame()) {
  withr::local_envvar(
    WORKSPACE_DATABASE_HOST = host %||% NA, WORKSPACE_DATABASE_PORT = port %||% NA,
    WORKSPACE_DATABASE_USERNAME = username %||% NA, WORKSPACE_DATABASE_PASSWORD = password %||% NA,
    WORKSPACE_DATABASE_DB_NAME = db_name %||% NA,
    .local_envir = .env
  )
}

# Provides a connection of the workspace, as in notebooks: "my-dhis2" becomes MY_DHIS2=<type>, MY_DHIS2_<FIELD>=...
provide_connection <- function(slug, type, ..., .env = parent.frame()) {
  prefix <- toupper(gsub("[^A-Za-z0-9]", "_", slug))
  fields <- c(...)
  if (length(fields) > 0) names(fields) <- paste0(prefix, "_", toupper(names(fields)))
  withr::local_envvar(c(stats::setNames(type, prefix), fields), .local_envir = .env)
}

# Whatever the machine, tests run against the fake server, outside a notebook.
provide_credentials(.env = testthat::teardown_env())
provide_files(.env = testthat::teardown_env())
provide_database(.env = testthat::teardown_env())
httr2::local_mocked_responses(server$respond, env = testthat::teardown_env())
testthat::local_mocked_bindings(
  download_signed_url = server$download, .package = "openhexa", .env = testthat::teardown_env()
)

# Builders of the records that the API returns, for the answers of the tests. Arguments replace fields.

with_fields <- function(record, ...) {
  fields <- list(...)
  record[names(fields)] <- fields
  record
}

test_user <- function(...) {
  with_fields(list(id = "u1", email = "ada@example.org", displayName = "Ada"), ...)
}

test_workspace <- function(...) {
  with_fields(list(
    slug = "test-workspace", name = "Test workspace", description = "", configuration = list(),
    createdAt = "2024-01-01T00:00:00Z", updatedAt = NULL, countries = list()
  ), ...)
}

test_version <- function(...) {
  with_fields(list(id = "v2", name = "2024", changelog = NULL, createdAt = "2024-02-01T00:00:00Z"), ...)
}

test_dataset <- function(...) {
  with_fields(list(
    id = "ds1", slug = "population", name = "Population", description = "Census data",
    createdAt = "2024-01-01T00:00:00Z", updatedAt = "2024-02-01T00:00:00Z",
    workspace = list(slug = "test-workspace"), latestVersion = test_version()
  ), ...)
}

test_dataset_file <- function(filename = "population.csv", ...) {
  with_fields(list(
    id = "f1", filename = filename, uri = paste0("ds1/v2/", filename), contentType = "text/csv", size = 1024,
    createdAt = "2024-02-01T00:00:00Z"
  ), ...)
}

test_webapp <- function(...) {
  with_fields(list(
    id = "w1", slug = "dashboard", name = "Dashboard", description = NULL, url = "https://dashboard.test",
    type = "STATIC", isPublic = FALSE, allowedOperations = list("DATASETS_READ"),
    workspace = list(slug = "test-workspace"), files = list(list(path = "index.html", type = "file"))
  ), ...)
}

test_pipeline <- function(...) {
  with_fields(list(
    id = "p1", code = "extract", name = "Extract", description = NULL, type = "zipFile", schedule = NULL,
    webhookEnabled = FALSE, createdAt = "2024-01-01T00:00:00Z", updatedAt = NULL,
    currentVersion = list(
      id = "pv1", versionNumber = 3, versionName = "v3", createdAt = "2024-01-01T00:00:00Z",
      parameters = list(list(
        code = "year", name = "Year", type = "int", required = TRUE, multiple = FALSE, default = 2024,
        choices = NULL, help = NULL
      ))
    )
  ), ...)
}

test_run <- function(...) {
  with_fields(list(
    id = "r1", status = "success", executionDate = "2024-01-01T00:00:00Z", duration = 12, progress = 100,
    config = list(), triggerMode = "manual", pipeline = list(code = "extract"),
    version = list(versionNumber = 3, versionName = "v3"), user = list(displayName = "Ada"),
    messages = list(list(message = "Done", priority = "INFO", timestamp = NULL))
  ), ...)
}

# A page of a paginated list.
test_page <- function(...) list(totalPages = 1, items = list(...))

# The payload of a successful mutation.
test_success <- function(...) list(success = TRUE, errors = list(), ...)

# The payload of a failed mutation.
test_failure <- function(...) list(success = FALSE, errors = list(...))
