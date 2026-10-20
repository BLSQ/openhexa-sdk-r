# A login dialog for RStudio, built as a Shiny gadget: a dropdown of servers and a token field, then a dropdown of
# workspaces. login() uses it in interactive RStudio sessions when shiny is installed, and falls back to console
# prompts otherwise. The dialog only collects the values: login() reports the result in the console.

use_login_gadget <- function() {
  rlang::is_installed("shiny") && rlang::is_installed("rstudioapi") && rstudioapi::isAvailable()
}

# Asks for what is not known yet: the server and token, then the workspace if `ask_workspace` is TRUE. Returns a list
# with `server_url`, `token` and `workspace`, each NULL when it was not asked for, or NULL when the user cancels.
login_gadget <- function(server_url = NULL, token = NULL, ask_workspace = TRUE) {
  server_url <- server_url %||% .openhexa$server_url %||% env_or_null("HEXA_SERVER_URL")
  token <- token %||% .openhexa$token %||% env_or_null("HEXA_TOKEN")
  need_server <- is.null(server_url)
  need_token <- is.null(token)
  result <- tryCatch(
    # Hide the messages of shiny ("Listening on http://127.0.0.1:..."): login() reports the result.
    suppressMessages(shiny::runGadget(
      login_gadget_ui(need_server, need_token),
      login_gadget_server(server_url, token, ask_workspace),
      viewer = shiny::dialogViewer("OpenHEXA", width = 460, height = 480),
      stopOnCancel = FALSE
    )),
    # Closing the window ends the gadget with an error.
    error = function(e) NULL,
    interrupt = function(e) NULL
  )
  # When it closes the dialog, RStudio interrupts R, which would silently stop login() halfway. Let that interrupt
  # arrive here instead.
  tryCatch(Sys.sleep(1), interrupt = function(e) NULL)
  result
}

login_gadget_ui <- function(need_server, need_token) {
  done <- shiny::actionButton("done", if (need_server || need_token) "Log in" else "Continue", class = "btn-primary")
  # Without credentials to enter, the dialog opens on the workspaces, which are loading.
  if (!need_server && !need_token) done <- shiny::tagAppendAttributes(done, disabled = "disabled")
  shiny::fluidPage(
    shiny::tags$style(shiny::HTML(login_gadget_css)),
    shiny::tags$script(shiny::HTML(login_gadget_js)),
    shiny::tags$h3(if (need_server || need_token) "Log in to OpenHEXA" else "Choose a workspace"),
    if (need_server) {
      shiny::tagList(
        shiny::selectInput("server", "Server", choices = c(known_servers, "Custom API URL" = "other"), width = "100%"),
        shiny::conditionalPanel(
          "input.server == 'other'",
          shiny::textInput("url", "API URL", placeholder = "https://api.my-openhexa.org", width = "100%")
        )
      )
    },
    if (need_token) {
      shiny::tagList(
        shiny::passwordInput("token", "API token", width = "100%"),
        shiny::uiOutput("token_help")
      )
    },
    # Shown until the server renders the workspaces, so that the dialog says they are loading from the start.
    shiny::tagAppendChild(
      shiny::uiOutput("workspace_ui"),
      if (!need_server && !need_token) workspaces_loading()
    ),
    shiny::div(style = "color: #c0392b; white-space: pre-line; min-height: 1.5em;", shiny::textOutput("error")),
    shiny::div(
      style = "text-align: right;",
      shiny::actionButton("cancel", "Cancel"),
      done
    )
  )
}

workspaces_loading <- function() {
  shiny::div(class = "openhexa-loading", shiny::span(class = "openhexa-spinner"), "Loading workspaces\u2026")
}

login_gadget_css <- "
.openhexa-loading { color: #777; margin-bottom: 15px; }
.openhexa-spinner {
  display: inline-block; width: 1em; height: 1em; margin-right: 0.5em; vertical-align: -0.15em;
  border: 2px solid #ccc; border-top-color: #555; border-radius: 50%; animation: openhexa-spin 0.8s linear infinite;
}
@keyframes openhexa-spin { to { transform: rotate(360deg); } }
"

# While the server works, the button is disabled and says so; the server enables it again when it is done.
login_gadget_js <- "
$(document).on('click', '#done', function() {
  $(this).prop('disabled', true).html('<span class=\"openhexa-spinner\"></span>Loading\u2026');
});
Shiny.addCustomMessageHandler('openhexa-ready', function(label) {
  $('#done').prop('disabled', false).text(label);
});
"

login_gadget_server <- function(server_url = NULL, token = NULL, ask_workspace = TRUE) {
  need_server <- is.null(server_url)
  need_token <- is.null(token)
  function(input, output, session) {
    error <- shiny::reactiveVal("")
    output$error <- shiny::renderText(error())
    # The verified client, and the workspaces to choose from once the credentials are verified.
    state <- shiny::reactiveValues(client = NULL, workspaces = NULL)

    # Link to the page that lists the tokens, on the selected server.
    output$token_help <- shiny::renderUI({
      url <- if (need_server) {
        if (identical(input$server, "other")) trimws(input$url %||% "") else input$server
      } else {
        server_url
      }
      if (!nzchar(url %||% "")) {
        return(shiny::helpText("Copy your token from the account settings in OpenHEXA."))
      }
      shiny::helpText(
        "Copy your token from your",
        shiny::tags$a(href = account_url(url), target = "_blank", "account settings"),
        "in OpenHEXA."
      )
    })

    loading <- shiny::reactiveVal(!need_server && !need_token)
    output$workspace_ui <- shiny::renderUI({
      if (loading()) return(workspaces_loading())
      if (is.null(state$workspaces)) return(NULL)
      choices <- stats::setNames(state$workspaces$slug, workspace_labels(state$workspaces))
      shiny::selectInput("workspace", "Workspace", choices = choices, width = "100%")
    })

    finish <- function(workspace = NULL) {
      # Save the result right away, so that it isn't lost if R is interrupted when the dialog closes.
      .openhexa$server_url <- state$client$server_url
      .openhexa$token <- state$client$token
      if (!is.null(workspace)) .openhexa$workspace <- workspace
      shiny::stopApp(list(
        server_url = if (need_server) state$client$server_url,
        token = if (need_token) state$client$token,
        workspace = workspace
      ))
    }

    # Check the credentials here, so that a typo can be fixed without reopening the dialog, then list the workspaces
    # to choose from. Finishes when there is nothing to choose.
    verify <- function(server_url, token) {
      client <- openhexa_client(server_url, token)
      user <- tryCatch(get_user(client), error = function(e) e)
      if (inherits(user, "error")) return(error(plain_message(user)))
      state$client <- client
      if (!ask_workspace) return(finish())
      workspaces <- tryCatch(list_workspaces(client = client), error = function(e) e)
      if (inherits(workspaces, "error")) return(error(plain_message(workspaces)))
      if (nrow(workspaces) == 0) return(finish())
      error("")
      state$workspaces <- workspaces
    }

    # Enables the button again, once the server is done.
    ready <- function() {
      label <- if (!is.null(shiny::isolate(state$workspaces)) || (!need_server && !need_token)) "Continue" else "Log in"
      session$sendCustomMessage("openhexa-ready", label)
    }

    # With the credentials already configured, go straight to the choice of the workspace. Load the workspaces once
    # the dialog is displayed, so that it shows that they are loading.
    if (!need_server && !need_token) {
      session$onFlushed(function() {
        shiny::isolate(verify(server_url, token))
        loading(FALSE)
        ready()
      })
    }

    submit <- function() {
      if (!is.null(state$workspaces)) return(finish(input$workspace))

      if (need_server) {
        server_url <- if (identical(input$server, "other")) trimws(input$url) else input$server
      }
      if (need_token) token <- trimws(input$token)
      if (need_server && !nzchar(server_url)) return(error("Enter the API URL."))
      if (need_token && !nzchar(token)) return(error("Enter your API token."))
      verify(server_url, token)
    }

    shiny::observeEvent(input$done, {
      submit()
      ready()
    })

    shiny::observeEvent(input$cancel, shiny::stopApp(NULL))
  }
}

# The message of a condition, without the console formatting (colors, bullets) of cli.
plain_message <- function(cnd) {
  cli::ansi_strip(conditionMessage(cnd))
}

# The account settings page of the web app, which lists the API tokens.
account_url <- function(server_url) {
  paste0(sub("^(https?://)api\\.", "\\1app.", normalize_server_url(server_url)), "/user/account/")
}
