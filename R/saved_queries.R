#' List saved queries
#'
#' Saved queries are SQL queries on the workspace database, saved from the Data Studio. Run them with
#' [run_saved_query()].
#'
#' @param query Only return saved queries whose name matches this search term.
#' @inheritParams get_workspace
#' @return A tibble with one row per saved query you can see.
#' @export
list_saved_queries <- function(query = NULL, workspace = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  items <- paginate(function(page, per_page) {
    vars <- list(workspaceSlug = workspace, query = query, page = page, perPage = per_page)
    graphql_op("SavedQueries", vars, client)$workspace$savedQueries
  })
  if (is.null(items)) abort_workspace_not_found(workspace)

  queries <- records_to_tibble(items, list(
    slug = col_chr("slug"),
    name = col_chr("name"),
    description = col_chr("description"),
    visibility = col_chr("visibility"),
    created_by = col_chr("createdBy", "displayName"),
    created_at = col_dttm("createdAt"),
    updated_at = col_dttm("updatedAt"),
    id = col_chr("id")
  ))
  queries$visibility <- tolower(queries$visibility)
  queries
}

#' Run a saved query
#'
#' Runs a saved query against the workspace database and returns its result as a tibble. This works from
#' anywhere, without database credentials: only the permission to see the saved query is needed.
#'
#' Values are returned as JSON by the API: numbers, text and logicals keep their type, but dates and timestamps
#' come back as text.
#'
#' @param slug The slug of the saved query, as shown by [list_saved_queries()].
#' @param max_rows The maximum number of rows to return. The server caps it to its own limit; a warning is raised
#'   when the result is truncated. `NULL` uses the server default.
#' @inheritParams get_user
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{
#' cases <- run_saved_query("monthly-malaria-cases", max_rows = 10000)
#' }
run_saved_query <- function(slug, max_rows = NULL, client = NULL) {
  input <- compact(list(slug = slug, maxRows = if (!is.null(max_rows)) as.integer(max_rows)))
  result <- graphql_op("ExecuteSavedQuery", list(input = input), client)$executeSavedQuery
  check_mutation(result, c(
    SAVED_QUERY_NOT_FOUND = cli::format_inline("Saved query {.val {slug}} does not exist, or you can't see it."),
    PERMISSION_DENIED = "You don't have permission to query this workspace database.",
    QUERY_TIMEOUT = "The query took too long and was cancelled.",
    QUERY_ERROR = paste("The query failed:", result$errorMessage %||% "unknown error.")
  ))

  rows <- query_rows_to_tibble(unlist(result$columns), result$rows)
  if (isTRUE(result$truncated)) {
    cli::cli_warn(c(
      "The result of {.val {slug}} was truncated to {nrow(rows)} row{?s}.",
      i = "Increase {.arg max_rows}, or connect to the database with {.fn connect_database} for large results."
    ))
  }
  rows
}

# Rows come as a list of JSON objects keyed by column name.
query_rows_to_tibble <- function(columns, rows) {
  data <- lapply(rlang::set_names(columns), function(column) {
    simplify_values(lapply(rows, function(row) row[[column]]))
  })
  tibble::new_tibble(data, nrow = length(rows))
}

# Turn a list of JSON values into an atomic vector when they are all scalars, or keep a list column.
simplify_values <- function(values) {
  missing <- vapply(values, is.null, logical(1))
  if (all(missing)) {
    return(rep(NA, length(values)))
  }
  if (!all(vapply(values[!missing], function(v) is.atomic(v) && length(v) == 1, logical(1)))) {
    return(values)
  }
  values[missing] <- list(NA)
  unlist(values, use.names = FALSE)
}
