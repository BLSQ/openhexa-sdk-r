#' List workspaces
#'
#' @param query Only return workspaces whose name matches this search term.
#' @inheritParams get_user
#' @return A tibble with one row per workspace you have access to.
#' @export
list_workspaces <- function(query = NULL, client = NULL) {
  items <- paginate(function(page, per_page) {
    graphql_op("Workspaces", list(query = query, page = page, perPage = per_page), client)$workspaces
  })
  records_to_tibble(items, list(
    slug = col_chr("slug"),
    name = col_chr("name"),
    description = col_chr("description"),
    created_at = col_dttm("createdAt"),
    updated_at = col_dttm("updatedAt")
  ))
}

#' Get a workspace
#'
#' @param workspace The workspace slug. Defaults to the workspace set with [login()] or the
#'   `HEXA_WORKSPACE` environment variable (always set inside OpenHEXA).
#' @inheritParams get_user
#' @return An `openhexa_workspace` object: a list with `slug`, `name`, `description`, `configuration` (a named
#'   list) and `countries` (a tibble).
#' @export
get_workspace <- function(workspace = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  data <- graphql_op("Workspace", list(slug = workspace), client)$workspace
  if (is.null(data)) abort_workspace_not_found(workspace)
  structure(
    list(
      slug = data$slug,
      name = data$name,
      description = data$description,
      configuration = data$configuration,
      countries = records_to_tibble(data$countries, list(
        code = col_chr("code"),
        alpha3 = col_chr("alpha3"),
        name = col_chr("name")
      )),
      created_at = parse_datetime(data$createdAt),
      updated_at = parse_datetime(data$updatedAt %||% NA_character_)
    ),
    class = "openhexa_workspace"
  )
}

#' @export
print.openhexa_workspace <- function(x, ...) {
  cli::cli_text("<openhexa_workspace> {.strong {x$name}} ({.val {x$slug}})")
  if (nrow(x$countries) > 0) cli::cli_text("Countries: {x$countries$name}")
  if (length(x$configuration) > 0) cli::cli_text("Configuration: {.field {names(x$configuration)}}")
  invisible(x)
}

#' Paths to the workspace filesystem
#'
#' Inside OpenHEXA (notebooks and pipelines), the workspace files are mounted on the local filesystem.
#' `workspace_path()` builds a path to a workspace file, and `workspace_tmp_path()` a path in the temporary
#' directory. Outside OpenHEXA, use [download_file()] and [upload_file()] instead.
#'
#' @param ... Path components, passed to [file.path()].
#' @return A path.
#' @export
#' @examples
#' workspace_path("data", "population.csv")
workspace_path <- function(...) {
  file.path(Sys.getenv("WORKSPACE_FILES_PATH", "/home/hexa/workspace"), ...)
}

#' @rdname workspace_path
#' @export
workspace_tmp_path <- function(...) {
  file.path(Sys.getenv("WORKSPACE_TMP_PATH", "/home/hexa/tmp"), ...)
}
