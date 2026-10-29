#' Authenticate to OpenHEXA
#'
#' Stores the server URL, API token and (optionally) a default workspace for the rest of the R session. Every
#' value that is not provided falls back to the corresponding environment variable (`HEXA_SERVER_URL`,
#' `HEXA_TOKEN`, `HEXA_WORKSPACE`), so inside OpenHEXA notebooks you usually don't need to call this at all.
#'
#' In an interactive session, you are asked for the server and the token when they are neither provided nor
#' configured, so that the token doesn't end up in your scripts, and then to choose a workspace if none is set. In
#' RStudio, with the shiny package installed, this opens a login dialog; elsewhere, you are prompted in the console.
#' In RStudio, calling `login()` without arguments again reopens the dialog, filled in with the current server, token
#' and workspace, so that you can change them.
#'
#' @param token An OpenHEXA API token. Your tokens, one per workspace, are listed under "Access tokens" in your
#'   account settings.
#' @param server_url The URL of the OpenHEXA API, such as `"https://api.openhexa.org"`.
#' @param workspace The slug of the workspace to use by default.
#' @param verify If `TRUE` (the default), check the credentials by fetching the current user.
#'
#' @return The client, invisibly, or `NULL` if you cancel the login dialog.
#' @export
#' @examples
#' \dontrun{
#' login() # asks you to pick the server, then prompts for the token
#' login(server_url = "https://api.openhexa.org", workspace = "my-workspace") # prompts for the token
#' login(token = "...", server_url = "https://api.openhexa.org", workspace = "my-workspace")
#' }
login <- function(token = NULL, server_url = NULL, workspace = NULL, verify = TRUE) {
  # Empty strings, e.g. from Sys.getenv() on an unset variable, count as not provided.
  if (identical(token, "")) token <- NULL
  if (identical(server_url, "")) server_url <- NULL
  if (identical(workspace, "")) workspace <- NULL
  interactive <- rlang::is_interactive()
  need_server <- is.null(server_url %||% .openhexa$server_url %||% env_or_null("HEXA_SERVER_URL"))
  need_token <- is.null(token %||% .openhexa$token %||% env_or_null("HEXA_TOKEN"))
  # Choosing a workspace needs the API, so only when the credentials are verified.
  need_workspace <- verify && is.null(workspace %||% .openhexa$workspace %||% env_or_null("HEXA_WORKSPACE"))
  gadget <- interactive && use_login_gadget()
  relogin <- is.null(token) && is.null(server_url) && is.null(workspace)
  if (gadget && (relogin || need_server || need_token || need_workspace)) {
    answer <- login_gadget(server_url, token, ask_workspace = need_workspace || (relogin && verify), edit = relogin)
    # Cancelling is not an error: leave everything as it was.
    if (is.null(answer)) {
      cli::cli_alert_info("Login cancelled.")
      return(invisible(NULL))
    }
    server_url <- answer$server_url %||% server_url
    token <- answer$token %||% token
    workspace <- answer$workspace %||% workspace
  } else if (interactive) {
    if (need_server) server_url <- ask_server_url()
    if (need_token) token <- ask_token()
  }
  if (!is.null(token)) .openhexa$token <- token
  if (!is.null(server_url)) .openhexa$server_url <- server_url

  client <- openhexa_client()
  if (verify) {
    me <- get_user(client)
    cli::cli_alert_success("Authenticated as {.strong {me$display_name}} on {.url {client$server_url}}.")
    if (need_workspace && is.null(workspace) && interactive && !gadget) workspace <- choose_workspace(client)
  }
  if (!is.null(workspace)) {
    if (verify) set_workspace(workspace, client) else .openhexa$workspace <- workspace
  }
  invisible(client)
}

#' Set the current workspace
#'
#' Selects the workspace that every function uses when its `workspace` argument is not given, after checking that
#' it exists. This is the same as `login(workspace = ...)`.
#'
#' @param workspace The workspace slug, as shown by [list_workspaces()]. In an interactive session, leave it out to
#'   choose from a list: in RStudio, with the shiny package installed, in a dialog; elsewhere, in the console.
#' @inheritParams get_user
#' @return The workspace, as an `openhexa_workspace` object, invisibly, or `NULL` if you cancel the dialog.
#' @export
#' @examples
#' \dontrun{
#' list_workspaces()
#' set_workspace("malaria-burkina")
#' set_workspace() # choose from a list
#' list_datasets()
#' }
set_workspace <- function(workspace = NULL, client = NULL) {
  if (is.null(workspace) && rlang::is_interactive()) {
    workspace <- if (is.null(client) && use_login_gadget()) {
      answer <- login_gadget(ask_workspace = TRUE)
      if (is.null(answer)) {
        cli::cli_alert_info("No workspace selected.")
        return(invisible(NULL))
      }
      answer$workspace
    } else {
      choose_workspace(client %||% openhexa_client())
    }
  }
  if (is.null(workspace)) {
    cli::cli_abort(
      c("No workspace specified.", i = "Pass the slug of a workspace, as shown by {.fn list_workspaces}."),
      class = c("openhexa_input_error", "openhexa_error")
    )
  }
  ws <- get_workspace(workspace, client)
  .openhexa$workspace <- ws$slug
  cli::cli_alert_success("Using workspace {.strong {ws$name}} ({.val {ws$slug}}).")
  invisible(ws)
}

#' Forget the credentials set with `login()`
#'
#' Environment variables are left untouched.
#'
#' @return `NULL`, invisibly.
#' @export
logout <- function() {
  rm(list = intersect(c("token", "server_url", "workspace"), ls(.openhexa)), envir = .openhexa)
  invisible(NULL)
}

#' Create an OpenHEXA API client
#'
#' A client bundles a server URL and an API token. Every function of the package accepts a `client` argument;
#' when it is `NULL`, the client is built from the values set with [login()] or from the `HEXA_SERVER_URL`
#' and `HEXA_TOKEN` environment variables. You only need to build clients explicitly to talk to several
#' servers, or with several tokens, in the same session.
#'
#' To use a client, pass it as the `client` argument of any function of the package, such as [list_workspaces()],
#' [list_datasets()], [get_dataset_data()], [run_pipeline()] or [graphql_query()].
#'
#' A few things to keep in mind:
#'
#' * A client does not carry a workspace. The default workspace is the one set with [login()] or [set_workspace()]
#'   (or `HEXA_WORKSPACE`), whatever the client: with a client for another server, pass `workspace` explicitly.
#' * The values are read when the client is created. Calling [login()] or [logout()] afterwards doesn't change
#'   existing clients.
#' * Creating a client doesn't contact the server: check the token with [get_user()].
#' * Printing a client hides the token, except for its last four characters.
#'
#' @inheritParams login
#' @return An `openhexa_client` object: a list with `server_url` (normalized: `https://` added when missing,
#'   trailing `/` and `/graphql` removed) and `token`.
#' @seealso [login()] to set the default client for the session.
#' @export
#' @examples
#' \dontrun{
#' # The client used by default, from login() or the environment variables
#' client <- openhexa_client()
#' client
#' #> <openhexa_client> https://api.openhexa.org (token: ****a1b2)
#'
#' # A client for another server, with its own token, kept out of the script
#' demo <- openhexa_client("https://api.demo.openhexa.org", Sys.getenv("OPENHEXA_DEMO_TOKEN"))
#' get_user(client = demo) # check the token
#'
#' # Pass it to any function. The workspace is not part of the client: name it.
#' list_workspaces(client = demo)
#' list_datasets(workspace = "my-demo-workspace", client = demo)
#' data <- get_dataset_data("population", workspace = "my-demo-workspace", client = demo)
#' run_pipeline("my-pipeline", config = list(year = 2024), workspace = "my-demo-workspace", client = demo)
#' graphql_query("query { me { user { email } } }", client = demo)
#'
#' # Copy a file from one server to another
#' prod <- openhexa_client("https://api.openhexa.org", Sys.getenv("OPENHEXA_TOKEN"))
#' path <- download_file("data/report.csv", tempfile(fileext = ".csv"), workspace = "my-workspace", client = prod)
#' upload_file(path, "data/report.csv", workspace = "my-demo-workspace", client = demo)
#' }
openhexa_client <- function(server_url = NULL, token = NULL) {
  server_url <- server_url %||% .openhexa$server_url %||% env_or_null("HEXA_SERVER_URL")
  token <- token %||% .openhexa$token %||% env_or_null("HEXA_TOKEN")

  if (is.null(server_url)) {
    cli::cli_abort(
      c(
        "No OpenHEXA server configured.",
        i = "Call {.code login(server_url = ...)} or set the {.envvar HEXA_SERVER_URL} environment variable."
      ),
      class = c("openhexa_config_error", "openhexa_error")
    )
  }
  if (is.null(token)) {
    cli::cli_abort(
      c("No OpenHEXA API token configured.", i = "Call {.fn login} or set the {.envvar HEXA_TOKEN} environment variable."),
      class = c("openhexa_config_error", "openhexa_error")
    )
  }

  structure(list(server_url = normalize_server_url(server_url), token = token), class = "openhexa_client")
}

#' @export
print.openhexa_client <- function(x, ...) {
  cli::cli_text("<openhexa_client> {.url {x$server_url}} (token: {.field ****{substring(x$token, nchar(x$token) - 3)}})")
  invisible(x)
}

#' Get the current user
#'
#' @param client An [openhexa_client()]. Defaults to the client configured with [login()] or environment variables.
#' @return A list with `id`, `email` and `display_name`.
#' @export
get_user <- function(client = NULL) {
  user <- graphql_op("Me", client = client)$me$user
  if (is.null(user)) {
    cli::cli_abort("The API token is not valid.", class = c("openhexa_auth_error", "openhexa_error"))
  }
  list(id = user$id, email = user$email, display_name = user$displayName)
}

# Resolve the workspace slug to use: explicit argument, then login(), then HEXA_WORKSPACE.
resolve_workspace <- function(workspace = NULL) {
  workspace <- workspace %||% .openhexa$workspace %||% env_or_null("HEXA_WORKSPACE")
  if (is.null(workspace)) {
    cli::cli_abort(
      c(
        "No workspace selected.",
        i = "Call {.fn set_workspace} to choose one, pass {.arg workspace}, or set the {.envvar HEXA_WORKSPACE} environment variable."
      ),
      class = c("openhexa_config_error", "openhexa_error")
    )
  }
  workspace
}

normalize_server_url <- function(url) {
  url <- trimws(url)
  if (!grepl("^https?://", url)) url <- paste0("https://", url)
  url <- sub("/+$", "", url)
  sub("/graphql$", "", url)
}

verify_ssl <- function() {
  !tolower(Sys.getenv("HEXA_VERIFY_SSL", "true")) %in% c("0", "false")
}

# Servers offered by login() in interactive sessions, besides a custom API URL.
known_servers <- c(
  "OpenHEXA (api.openhexa.org)" = "https://api.openhexa.org",
  "OpenHEXA demo (api.demo.openhexa.org)" = "https://api.demo.openhexa.org"
)

ask_server_url <- function() {
  choice <- ask_menu(c(names(known_servers), "Custom API URL"), "Which OpenHEXA server?")
  if (choice == 0) {
    cli::cli_abort("No server selected.", class = c("openhexa_config_error", "openhexa_error"))
  }
  if (choice <= length(known_servers)) {
    return(unname(known_servers[[choice]]))
  }

  prompt <- "OpenHEXA API URL (e.g. https://api.my-openhexa.org): "
  url <- if (rlang::is_installed("rstudioapi") && rstudioapi::isAvailable()) {
    rstudioapi::showPrompt("OpenHEXA", prompt)
  } else {
    readline(prompt)
  }
  url <- trimws(url %||% "")
  if (!nzchar(url)) {
    cli::cli_abort("No server URL provided.", class = c("openhexa_config_error", "openhexa_error"))
  }
  url
}

# Asks to choose a workspace in the console. Returns its slug, or NULL if there is none or none was chosen.
choose_workspace <- function(client) {
  workspaces <- list_workspaces(client = client)
  if (nrow(workspaces) == 0) return(NULL)
  choice <- ask_menu(workspace_labels(workspaces), "Which workspace?")
  if (choice == 0) NULL else workspaces$slug[[choice]]
}

# How workspaces are shown in menus and dropdowns: their name and slug, as in the messages.
workspace_labels <- function(workspaces) {
  sprintf("%s (\"%s\")", workspaces$name, workspaces$slug)
}

ask_menu <- function(choices, title) {
  utils::menu(choices, title = title)
}

ask_token <- function() {
  prompt <- "OpenHEXA API token: "
  token <- if (rlang::is_installed("rstudioapi") && rstudioapi::isAvailable()) {
    rstudioapi::askForPassword(prompt)
  } else {
    readline(prompt)
  }
  if (is.null(token) || !nzchar(token)) {
    cli::cli_abort("No API token provided.", class = c("openhexa_config_error", "openhexa_error"))
  }
  token
}

env_or_null <- function(name) {
  value <- Sys.getenv(name)
  if (nzchar(value)) value else NULL
}
