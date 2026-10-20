# The API seen by the login dialog: the token "test-token" is valid, and gives access to two workspaces.
answer_login <- function(env = parent.frame()) {
  server$answer(
    Me = function(vars, req) {
      valid <- identical(httr2::req_get_headers(req, redacted = "reveal")$Authorization, "Bearer test-token")
      list(me = list(user = if (valid) test_user()))
    },
    Workspaces = list(workspaces = test_page(test_workspace(), test_workspace(slug = "other", name = "Other workspace"))),
    Workspace = list(workspace = test_workspace(slug = "other", name = "Other workspace")),
    .env = env
  )
}

# Catches the value the dialog returns when it closes.
capture_dialog_result <- function(env = parent.frame()) {
  withr::defer(logout(), envir = env)
  result <- new.env()
  testthat::local_mocked_bindings(stopApp = function(value = NULL) result$value <- value, .package = "shiny", .env = env)
  result
}

test_that("login() opens the login dialog in RStudio", {
  provide_credentials(server_url = NULL, token = NULL, workspace = NULL)
  rlang::local_interactive()
  local_mocked_bindings(
    use_login_gadget = function() TRUE,
    login_gadget = function(server_url, token, ask_workspace) {
      expect_false(ask_workspace)
      list(server_url = test_server_url, token = "dialog-token", workspace = NULL)
    }
  )
  login(verify = FALSE)
  client <- openhexa_client()
  expect_equal(client$server_url, test_server_url)
  expect_equal(client$token, "dialog-token")
})

test_that("login() reports the result of the dialog in the console", {
  provide_credentials(server_url = NULL, token = NULL, workspace = NULL)
  rlang::local_interactive()
  local_mocked_bindings(
    use_login_gadget = function() TRUE,
    login_gadget = function(server_url, token, ask_workspace) {
      list(server_url = test_server_url, token = "test-token", workspace = "other")
    }
  )
  answer_login()
  messages <- testthat::capture_messages(login())
  expect_match(messages[[1]], "Authenticated as Ada")
  expect_match(messages[[2]], 'Using workspace Other workspace ("other")', fixed = TRUE)
  expect_equal(resolve_workspace(), "other")
})

test_that("cancelling the dialog leaves everything as it was, without an error", {
  provide_credentials(server_url = NULL, token = NULL, workspace = NULL)
  rlang::local_interactive()
  local_mocked_bindings(use_login_gadget = function() TRUE, login_gadget = function(...) NULL)
  expect_message(out <- login(), "Login cancelled")
  expect_null(out)
  expect_error(openhexa_client(), class = "openhexa_config_error")

  expect_message(out <- set_workspace(), "No workspace selected")
  expect_null(out)
  expect_error(resolve_workspace(), class = "openhexa_config_error")
})

test_that("the login dialog validates the input, checks the credentials and lists the workspaces", {
  skip_if_not_installed("shiny")
  result <- capture_dialog_result()
  answer_login()

  shiny::testServer(login_gadget_server(), {
    session$setInputs(server = "other", url = "", token = "test-token", done = 1)
    expect_equal(output$error, "Enter the API URL.")

    session$setInputs(server = test_server_url, token = "", done = 2)
    expect_equal(output$error, "Enter your API token.")

    session$setInputs(token = "bad", done = 3)
    expect_equal(output$error, "The API token is not valid.")
    expect_null(result$value)

    session$setInputs(token = " test-token ", done = 4)
    expect_equal(output$error, "")
    expect_match(output$workspace_ui$html, 'Other workspace ("other")', fixed = TRUE)
    expect_null(result$value)

    session$setInputs(workspace = "other", done = 5)
    expect_equal(result$value, list(server_url = test_server_url, token = "test-token", workspace = "other"))
    # Saved before the dialog closes, in case R is interrupted.
    expect_equal(openhexa_client()$token, "test-token")
    expect_equal(resolve_workspace(), "other")
  })
})

test_that("the login dialog closes after the credentials when no workspace is needed", {
  skip_if_not_installed("shiny")
  result <- capture_dialog_result()
  answer_login()
  shiny::testServer(login_gadget_server(ask_workspace = FALSE), {
    session$setInputs(server = test_server_url, token = "test-token", done = 1)
    expect_equal(result$value, list(server_url = test_server_url, token = "test-token", workspace = NULL))
  })
})

test_that("the login dialog goes straight to the workspace when the credentials are configured", {
  skip_if_not_installed("shiny")
  result <- capture_dialog_result()
  answer_login()
  shiny::testServer(login_gadget_server(test_server_url, "test-token"), {
    expect_match(output$workspace_ui$html, "Loading workspaces")
    session$flushReact()
    session$flushReact()
    expect_match(output$workspace_ui$html, "Test workspace")
    session$setInputs(workspace = "test-workspace", done = 1)
    expect_equal(result$value, list(server_url = NULL, token = NULL, workspace = "test-workspace"))
  })
})

test_that("the login dialog only asks for what is missing", {
  skip_if_not_installed("shiny")
  html <- as.character(login_gadget_ui(need_server = FALSE, need_token = TRUE))
  expect_no_match(html, "Server")
  expect_match(html, "API token")
})

test_that("the login dialog links to the account settings of the selected server", {
  expect_equal(account_url(test_server_url), paste0(test_app_url, "/user/account/"))

  skip_if_not_installed("shiny")
  shiny::testServer(login_gadget_server(), {
    session$setInputs(server = test_server_url)
    expect_match(output$token_help$html, paste0(test_app_url, "/user/account/"), fixed = TRUE)
    session$setInputs(server = "other", url = "")
    expect_no_match(output$token_help$html, "href")
  })
})

test_that("error messages are shown without console formatting", {
  cnd <- tryCatch(cli::cli_abort(c("Failed.", x = "{.val x} is wrong")), error = identity)
  withr::local_options(cli.num_colors = 256)
  expect_no_match(plain_message(cnd), "\033[", fixed = TRUE)
})

test_that("set_workspace() opens the dialog in RStudio", {
  provide_credentials()
  rlang::local_interactive()
  local_mocked_bindings(
    use_login_gadget = function() TRUE,
    login_gadget = function(server_url = NULL, token = NULL, ask_workspace) list(workspace = "test-workspace")
  )
  server$answer(Workspace = list(workspace = test_workspace()))
  expect_message(set_workspace(), "Test workspace")
  expect_equal(resolve_workspace(), "test-workspace")
})

test_that("the dialog is titled after what it asks for", {
  skip_if_not_installed("shiny")
  expect_match(as.character(login_gadget_ui(FALSE, FALSE)), "Choose a workspace")
  expect_match(as.character(login_gadget_ui(TRUE, TRUE)), "Log in to OpenHEXA")
})
