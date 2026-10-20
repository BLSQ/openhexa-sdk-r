#' Run a GraphQL query against the OpenHEXA API
#'
#' This is the escape hatch for everything the package does not cover with a dedicated function. See the
#' schema at `<server>/graphql/` for what is available.
#'
#' @param query A GraphQL document, as a string.
#' @param variables A named list of variables. `NULL` values are dropped.
#' @inheritParams get_user
#'
#' @return The `data` field of the response, as a nested list.
#' @export
#' @examples
#' \dontrun{
#' graphql_query(
#'   "query ($slug: String!) { workspace(slug: $slug) { name } }",
#'   list(slug = "my-workspace")
#' )
#' }
graphql_query <- function(query, variables = list(), client = NULL) {
  client <- client %||% openhexa_client()

  req <- httr2::request(paste0(client$server_url, "/graphql/")) |>
    httr2::req_auth_bearer_token(client$token) |>
    httr2::req_user_agent(user_agent()) |>
    httr2::req_body_json(list(query = query, variables = as_json_object(compact(variables)))) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(body = http_error_body) |>
    with_ssl_options()

  body <- httr2::resp_body_json(httr2::req_perform(req), simplifyVector = FALSE)
  if (length(body$errors) > 0) {
    messages <- vapply(body$errors, function(e) e$message %||% "Unknown error", character(1))
    cli::cli_abort(
      c("The OpenHEXA API returned an error.", rlang::set_names(messages, "x")),
      class = c("openhexa_graphql_error", "openhexa_error"),
      errors = body$errors
    )
  }
  body$data
}

# Run one of the operations shipped in inst/graphql/.
graphql_op <- function(name, variables = list(), client = NULL) {
  graphql_query(operation(name), variables, client)
}

operation <- function(name) {
  if (is.null(.openhexa$operations[[name]])) {
    path <- system.file("graphql", paste0(name, ".graphql"), package = "openhexa", mustWork = TRUE)
    .openhexa$operations[[name]] <- paste(readLines(path, warn = FALSE), collapse = "\n")
  }
  .openhexa$operations[[name]]
}

# Mutations report expected failures through `success` and `errors` instead of GraphQL errors.
# `messages` maps error codes to user-friendly messages.
check_mutation <- function(result, messages = character()) {
  if (isTRUE(result$success)) {
    return(invisible(result))
  }
  codes <- as.character(unlist(result$errors))
  known <- unname(messages[intersect(codes, names(messages))])
  cli::cli_abort(
    c(
      if (length(known) > 0) known[[1]] else "The OpenHEXA API refused the operation.",
      i = if (length(codes) > 0) "Error code{?s}: {.val {codes}}."
    ),
    class = c("openhexa_mutation_error", "openhexa_error"),
    codes = codes
  )
}

http_error_body <- function(resp) {
  if (httr2::resp_status(resp) == 401) {
    return("Check your API token.")
  }
  tryCatch(
    vapply(httr2::resp_body_json(resp)$errors, function(e) e$message, character(1)),
    error = function(e) character()
  )
}

with_ssl_options <- function(req) {
  if (verify_ssl()) req else httr2::req_options(req, ssl_verifypeer = 0L, ssl_verifyhost = 0L)
}

user_agent <- function() {
  paste0("openhexa-sdk-r/", utils::packageVersion("openhexa"))
}

compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

# An empty list must be sent as a JSON object ({}), not an array ([]).
as_json_object <- function(x) {
  if (length(x) == 0) structure(list(), names = character()) else x
}
