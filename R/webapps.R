#' List web apps
#'
#' @inheritParams get_workspace
#' @return A tibble with one row per web app of the workspace.
#' @export
list_webapps <- function(workspace = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  items <- paginate(function(page, per_page) {
    graphql_op("Webapps", list(workspaceSlug = workspace, page = page, perPage = per_page), client)$webapps
  })
  webapps <- records_to_tibble(items, list(
    slug = col_chr("slug"),
    name = col_chr("name"),
    description = col_chr("description"),
    type = col_chr("type"),
    public = col_lgl("isPublic"),
    url = col_chr("url"),
    id = col_chr("id")
  ))
  webapps$type <- tolower(webapps$type)
  webapps
}

#' Get a web app
#'
#' @param webapp The web app slug, or an `openhexa_webapp` object.
#' @inheritParams get_workspace
#' @return An `openhexa_webapp` object: a list with `id`, `slug`, `name`, `description`, `type`, `url`, `public`,
#'   `allowed_operations` and `files` (the paths of the files of a static web app).
#' @export
get_webapp <- function(webapp, workspace = NULL, client = NULL) {
  slug <- if (inherits(webapp, "openhexa_webapp")) webapp$slug else webapp
  workspace <- if (inherits(webapp, "openhexa_webapp")) webapp$workspace else resolve_workspace(workspace)
  data <- graphql_op("Webapp", list(workspaceSlug = workspace, slug = slug), client)$webapp
  if (is.null(data)) {
    cli::cli_abort(
      "Web app {.val {slug}} does not exist in workspace {.val {workspace}}.",
      class = c("openhexa_not_found_error", "openhexa_error")
    )
  }
  files <- Filter(function(f) tolower(f$type) == "file", data$files %||% list())
  structure(
    list(
      id = data$id,
      slug = data$slug,
      name = data$name,
      description = data$description,
      type = tolower(data$type),
      url = data$url,
      public = data$isPublic,
      allowed_operations = tolower(unlist(data$allowedOperations)),
      files = vapply(files, function(f) f$path, character(1)),
      workspace = data$workspace$slug
    ),
    class = "openhexa_webapp"
  )
}

#' @export
print.openhexa_webapp <- function(x, ...) {
  cli::cli_text("<openhexa_webapp> {.strong {x$name}} ({.val {x$slug}}, {x$type}{if (isTRUE(x$public)) ', public'})")
  cli::cli_text("URL: {.url {x$url}}")
  if (length(x$files) > 0) cli::cli_text("Files: {.file {x$files}}")
  invisible(x)
}

#' Publish a static web app
#'
#' Publishes HTML content as a web app of the workspace. If the workspace already has a web app with this name,
#' its content is replaced; otherwise a new web app is created.
#'
#' @param name The name of the web app.
#' @param content What to publish:
#'   * a string of HTML, published as `index.html`;
#'   * the path to an HTML file, published as `index.html`;
#'   * the path to a directory, whose files are all published (it must contain an `index.html`);
#'   * an HTML object from the htmltools package, or an htmlwidget (e.g. a plotly chart or a leaflet map), saved
#'     with its dependencies (requires htmltools).
#' @param description A description of the web app.
#' @param public If `TRUE`, anyone with the link can see the web app. `NULL` (the default) leaves an existing
#'   web app unchanged, and makes new ones private.
#' @param allowed_operations What the web app may do on behalf of its viewers through the OpenHEXA API, among
#'   `"pipelines_run"`, `"pipelines_read"`, `"files_read"`, `"files_write"`, `"datasets_read"`, `"datasets_write"`,
#'   `"user_read"` and `"database_read"`. `NULL` leaves an existing web app unchanged.
#' @param message A message describing the change, recorded in the history of the web app.
#' @inheritParams get_workspace
#' @return The web app, as an `openhexa_webapp` object. Its `url` is the address to share.
#' @export
#' @examples
#' \dontrun{
#' publish_webapp("Hello", "<h1>Hello from R</h1>")
#'
#' # An interactive chart
#' chart <- plotly::plot_ly(cases, x = ~month, y = ~cases, type = "bar")
#' app <- publish_webapp("Malaria cases", chart)
#' app$url
#' }
publish_webapp <- function(name, content, description = NULL, public = NULL, allowed_operations = NULL,
                               message = NULL, workspace = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  files <- webapp_files(content)
  operations <- if (!is.null(allowed_operations)) as.list(toupper(allowed_operations))

  existing <- list_webapps(workspace, client)
  existing <- existing[existing$name == name, ]
  if (nrow(existing) == 0) {
    input <- compact(list(
      name = name,
      description = description,
      isPublic = public,
      workspaceSlug = workspace,
      allowedOperations = operations,
      source = list(static = files)
    ))
    result <- graphql_op("CreateWebapp", list(input = input), client)$createWebapp
    check_mutation(result, webapp_errors)
    cli::cli_alert_success("Created web app {.strong {name}} ({.val {result$webapp$slug}}).")
  } else {
    webapp <- get_webapp(existing$slug[[1]], workspace, client)
    if (webapp$type != "static") {
      cli::cli_abort(
        "Web app {.val {name}} is a {webapp$type} web app: only static web apps can be published from R.",
        class = c("openhexa_input_error", "openhexa_error")
      )
    }
    paths <- vapply(files, function(f) f$path, character(1))
    input <- compact(list(
      id = webapp$id,
      description = description,
      isPublic = public,
      allowedOperations = operations,
      files = files,
      filesToDelete = as.list(setdiff(webapp$files, paths)),
      commitMessage = message
    ))
    if (length(input$filesToDelete) == 0) input$filesToDelete <- NULL
    result <- graphql_op("UpdateWebapp", list(input = input), client)$updateWebapp
    check_mutation(result, webapp_errors)
    cli::cli_alert_success("Updated web app {.strong {name}} ({.val {webapp$slug}}).")
  }
  get_webapp(result$webapp$slug, workspace, client)
}

#' Delete a web app
#'
#' @inheritParams get_webapp
#' @return `NULL`, invisibly.
#' @export
delete_webapp <- function(webapp, workspace = NULL, client = NULL) {
  webapp <- get_webapp(webapp, workspace, client)
  result <- graphql_op("DeleteWebapp", list(input = list(id = webapp$id)), client)$deleteWebapp
  check_mutation(result, webapp_errors)
  invisible(NULL)
}

webapp_errors <- c(
  WEBAPPS_NOT_CONFIGURED = "Static web apps are not enabled on this OpenHEXA server.",
  PERMISSION_DENIED = "You don't have permission to manage web apps in this workspace.",
  ALREADY_EXISTS = "A web app with this name already exists.",
  WEBAPP_NOT_FOUND = "This web app does not exist.",
  SAVE_FAILED = "The web app files could not be saved."
)

# The files of a static web app, as WebappFileInput objects.
webapp_files <- function(content) {
  if (inherits(content, c("shiny.tag", "shiny.tag.list", "html_dependency", "htmlwidget"))) {
    rlang::check_installed("htmltools", "to publish HTML objects.")
    dir <- tempfile("webapp")
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    dir.create(dir)
    htmltools::save_html(htmltools::tagList(content), file.path(dir, "index.html"), libdir = "lib")
    return(webapp_files(dir))
  }
  if (!rlang::is_string(content)) {
    cli::cli_abort(
      "{.arg content} must be a string of HTML, a file or directory path, or an HTML object.",
      class = c("openhexa_input_error", "openhexa_error")
    )
  }

  if (dir.exists(content)) {
    paths <- list.files(content, recursive = TRUE)
    if (!"index.html" %in% paths) {
      cli::cli_abort("{.file {content}} has no {.file index.html}.", class = c("openhexa_input_error", "openhexa_error"))
    }
    return(lapply(paths, function(path) webapp_file(path, file.path(content, path))))
  }
  if (file.exists(content)) {
    if (!tolower(tools::file_ext(content)) %in% c("html", "htm")) {
      cli::cli_abort(
        c("{.file {content}} is not an HTML file.", i = "To publish several files, pass the directory that holds them."),
        class = c("openhexa_input_error", "openhexa_error")
      )
    }
    return(list(webapp_file("index.html", content)))
  }
  if (!grepl("<", content, fixed = TRUE)) {
    cli::cli_abort(
      "{.arg content} is neither HTML nor an existing file or directory: {.val {content}}.",
      class = c("openhexa_input_error", "openhexa_error")
    )
  }
  list(list(path = "index.html", content = content, encoding = "TEXT"))
}

webapp_file <- function(path, local) {
  text_extensions <- c("html", "htm", "css", "js", "mjs", "json", "map", "svg", "txt", "md", "csv", "xml", "geojson")
  if (tolower(tools::file_ext(path)) %in% text_extensions) {
    content <- readChar(local, file.size(local), useBytes = TRUE)
    Encoding(content) <- "UTF-8"
    list(path = path, content = content, encoding = "TEXT")
  } else {
    list(path = path, content = jsonlite::base64_enc(readBin(local, "raw", file.size(local))), encoding = "BASE64")
  }
}
