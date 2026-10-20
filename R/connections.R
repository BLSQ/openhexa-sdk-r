#' List workspace connections
#'
#' Lists the connections of a workspace, without their values. Use [get_connection()] to get the values of one.
#'
#' @inheritParams get_workspace
#' @return A tibble with one row per connection. `fields` lists the field codes of each connection.
#' @export
list_connections <- function(workspace = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  data <- graphql_op("Connections", list(slug = workspace), client)$workspace
  if (is.null(data)) abort_workspace_not_found(workspace)
  connections <- records_to_tibble(data$connections, list(
    identifier = col_chr("slug"),
    name = col_chr("name"),
    type = col_chr("type"),
    description = col_chr("description"),
    fields = col_list("fields"),
    created_at = col_dttm("createdAt"),
    updated_at = col_dttm("updatedAt"),
    id = col_chr("id")
  ))
  connections$fields <- lapply(connections$fields, function(fields) {
    vapply(fields, function(f) f$code, character(1))
  })
  connections
}

#' Get a workspace connection
#'
#' Connections store the credentials of external systems (DHIS2, IASO, PostgreSQL, S3, GCS or custom ones) at
#' the workspace level. Inside OpenHEXA, connections are read from environment variables; elsewhere, they are
#' fetched from the API.
#'
#' The typed variants (`get_dhis2_connection()`, ...) also check that the connection is of the expected type.
#'
#' @param identifier The identifier of the connection, as shown in the workspace settings.
#' @inheritParams get_workspace
#' @return An `openhexa_connection` object: a list with `identifier`, `type`, and the connection fields:
#'   * DHIS2 and IASO: `url`, `username`, `password`
#'   * PostgreSQL: `host`, `port`, `username`, `password`, `database_name`
#'   * S3: `access_key_id`, `secret_access_key`, `bucket_name`
#'   * GCS: `service_account_key`, `bucket_name`
#'   * Custom: the fields defined for the connection
#'
#' Credentials are hidden when the connection is printed.
#' @export
#' @examples
#' \dontrun{
#' dhis2 <- get_dhis2_connection("dhis2-play")
#' dhis2$url
#' }
get_connection <- function(identifier, workspace = NULL, client = NULL) {
  connection <- connection_from_env(identifier) %||% connection_from_api(identifier, workspace, client)
  if (is.null(connection)) {
    cli::cli_abort(
      "Connection {.val {identifier}} does not exist.",
      class = c("openhexa_connection_not_found_error", "openhexa_not_found_error", "openhexa_error")
    )
  }

  fields <- connection$fields
  if (connection$type == "S3") fields <- rename(fields, c(access_key_secret = "secret_access_key"))
  if (connection$type == "POSTGRESQL") {
    fields <- rename(fields, c(db_name = "database_name"))
    fields$port <- as.integer(fields$port)
  }

  structure(
    c(list(identifier = identifier, type = connection$type), fields),
    secret_fields = connection$secret_fields,
    class = c(paste0("openhexa_", tolower(connection$type), "_connection"), "openhexa_connection")
  )
}

#' @rdname get_connection
#' @export
get_dhis2_connection <- function(identifier, workspace = NULL, client = NULL) {
  typed_connection(identifier, "DHIS2", workspace, client)
}

#' @rdname get_connection
#' @export
get_iaso_connection <- function(identifier, workspace = NULL, client = NULL) {
  typed_connection(identifier, "IASO", workspace, client)
}

#' @rdname get_connection
#' @export
get_postgresql_connection <- function(identifier, workspace = NULL, client = NULL) {
  typed_connection(identifier, "POSTGRESQL", workspace, client)
}

#' @rdname get_connection
#' @export
get_s3_connection <- function(identifier, workspace = NULL, client = NULL) {
  typed_connection(identifier, "S3", workspace, client)
}

#' @rdname get_connection
#' @export
get_gcs_connection <- function(identifier, workspace = NULL, client = NULL) {
  typed_connection(identifier, "GCS", workspace, client)
}

#' @rdname get_connection
#' @export
get_custom_connection <- function(identifier, workspace = NULL, client = NULL) {
  typed_connection(identifier, "CUSTOM", workspace, client)
}

#' @export
print.openhexa_connection <- function(x, ...) {
  secret <- union(attr(x, "secret_fields"), secret_connection_fields)
  fields <- setdiff(names(x), c("identifier", "type"))
  cli::cli_text("<openhexa_connection> {.val {x$identifier}} ({x$type})")
  for (field in fields) {
    value <- if (field %in% secret || is.null(x[[field]])) "****" else x[[field]]
    cli::cli_text("{.field {field}}: {value}")
  }
  invisible(x)
}

# The fields of each connection type, as named in the API and in environment variables.
connection_fields <- list(
  DHIS2 = c("url", "username", "password"),
  IASO = c("url", "username", "password"),
  POSTGRESQL = c("host", "port", "username", "password", "db_name"),
  S3 = c("access_key_id", "access_key_secret", "bucket_name"),
  GCS = c("service_account_key", "bucket_name")
)

secret_connection_fields <- c("password", "access_key_secret", "secret_access_key", "service_account_key")

typed_connection <- function(identifier, type, workspace, client) {
  connection <- get_connection(identifier, workspace, client)
  if (connection$type != type) {
    cli::cli_abort(
      "Connection {.val {identifier}} is a {connection$type} connection, not a {type} connection.",
      class = c("openhexa_connection_type_error", "openhexa_error")
    )
  }
  connection
}

# Inside OpenHEXA, a connection "my-dhis2" is exposed as MY_DHIS2=DHIS2, MY_DHIS2_URL=..., MY_DHIS2_USERNAME=...
connection_from_env <- function(identifier) {
  prefix <- env_prefix(identifier)
  type <- toupper(Sys.getenv(prefix))
  if (!type %in% c(names(connection_fields), "CUSTOM")) {
    return(NULL)
  }

  if (type == "CUSTOM") {
    env <- Sys.getenv()
    keys <- names(env)[startsWith(names(env), paste0(prefix, "_"))]
    fields <- as.list(rlang::set_names(unname(env[keys]), tolower(substring(keys, nchar(prefix) + 2))))
    # Custom fields can't be told apart from secrets in environment variables: hide them all.
    secret_fields <- names(fields)
  } else {
    names <- connection_fields[[type]]
    fields <- lapply(rlang::set_names(names), function(f) env_or_null(paste0(prefix, "_", toupper(f))))
    secret_fields <- intersect(names, secret_connection_fields)
  }
  list(type = type, fields = fields, secret_fields = secret_fields)
}

connection_from_api <- function(identifier, workspace, client) {
  workspace <- resolve_workspace(workspace)
  vars <- list(workspaceSlug = workspace, connectionSlug = tolower(identifier))
  data <- graphql_op("ConnectionBySlug", vars, client)$connectionBySlug
  if (is.null(data)) {
    return(NULL)
  }
  codes <- vapply(data$fields, function(f) f$code, character(1))
  secret <- vapply(data$fields, function(f) isTRUE(f$secret), logical(1))
  fields <- rlang::set_names(lapply(data$fields, function(f) f$value), codes)
  list(type = data$type, fields = fields, secret_fields = union(codes[secret], secret_connection_fields))
}

env_prefix <- function(identifier) {
  toupper(gsub("[^A-Za-z0-9]+", "_", identifier))
}

rename <- function(x, mapping) {
  idx <- match(names(mapping), names(x))
  names(x)[idx[!is.na(idx)]] <- mapping[!is.na(idx)]
  x
}
