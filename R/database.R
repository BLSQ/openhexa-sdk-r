#' Connect to a PostgreSQL database
#'
#' Opens a [DBI][DBI::dbConnect] connection to the workspace database (the default), or to a PostgreSQL
#' connection of the workspace. Requires the DBI and RPostgres packages.
#'
#' @param connection `NULL` for the workspace database, or the identifier of a PostgreSQL connection
#'   (or an object returned by [get_postgresql_connection()]).
#' @param ... Passed to [DBI::dbConnect()].
#' @inheritParams get_workspace
#' @return A `DBIConnection`. Close it with [DBI::dbDisconnect()].
#' @export
#' @examples
#' \dontrun{
#' con <- connect_database()
#' DBI::dbListTables(con)
#' dplyr::tbl(con, "population") |> dplyr::filter(year == 2024) |> dplyr::collect()
#' DBI::dbDisconnect(con)
#' }
connect_database <- function(connection = NULL, ..., workspace = NULL, client = NULL) {
  rlang::check_installed(c("DBI", "RPostgres"), "to connect to databases.")
  credentials <- if (is.null(connection)) {
    get_database_credentials(workspace, client)
  } else if (inherits(connection, "openhexa_postgresql_connection")) {
    connection
  } else {
    get_postgresql_connection(connection, workspace, client)
  }

  DBI::dbConnect(
    RPostgres::Postgres(),
    host = credentials$host,
    port = credentials$port,
    dbname = credentials$database_name,
    user = credentials$username,
    password = credentials$password,
    ...
  )
}

#' Get the workspace database credentials
#'
#' Inside OpenHEXA, credentials are read from environment variables; elsewhere, they are fetched from the API,
#' which requires the permission to see them. Most of the time you want [connect_database()] instead.
#'
#' @inheritParams get_workspace
#' @return An `openhexa_postgresql_connection` object, see [get_connection()].
#' @export
get_database_credentials <- function(workspace = NULL, client = NULL) {
  if (nzchar(Sys.getenv("WORKSPACE_DATABASE_HOST"))) {
    fields <- list(
      host = Sys.getenv("WORKSPACE_DATABASE_HOST"),
      port = as.integer(Sys.getenv("WORKSPACE_DATABASE_PORT", "5432")),
      username = Sys.getenv("WORKSPACE_DATABASE_USERNAME"),
      password = Sys.getenv("WORKSPACE_DATABASE_PASSWORD"),
      database_name = Sys.getenv("WORKSPACE_DATABASE_DB_NAME")
    )
  } else {
    workspace <- resolve_workspace(workspace)
    data <- graphql_op("WorkspaceDatabaseCredentials", list(slug = workspace), client)$workspace
    if (is.null(data)) abort_workspace_not_found(workspace)
    credentials <- data$database$credentials
    if (is.null(credentials)) {
      cli::cli_abort(
        "You don't have permission to see the database credentials of workspace {.val {workspace}}.",
        class = c("openhexa_permission_error", "openhexa_error")
      )
    }
    fields <- list(
      host = credentials$host,
      port = as.integer(credentials$port),
      username = credentials$username,
      password = credentials$password,
      database_name = credentials$dbName
    )
  }

  structure(
    c(list(identifier = "workspace-database", type = "POSTGRESQL"), fields),
    secret_fields = "password",
    class = c("openhexa_postgresql_connection", "openhexa_connection")
  )
}
