test_that("the client is configured from environment variables", {
  provide_credentials(server_url = "https://api.env.example", token = "env-token")
  client <- openhexa_client()
  expect_s3_class(client, "openhexa_client")
  expect_equal(client$server_url, "https://api.env.example")
  expect_equal(client$token, "env-token")
})

test_that("explicit arguments take precedence", {
  provide_credentials(server_url = "https://api.env.example", token = "env-token")
  client <- openhexa_client("https://api.arg.example", "arg-token")
  expect_equal(client$server_url, "https://api.arg.example")
  expect_equal(client$token, "arg-token")
})

test_that("a missing configuration is reported", {
  provide_credentials(server_url = NULL, token = NULL, workspace = NULL)
  expect_error(openhexa_client(), class = "openhexa_config_error")
  expect_error(openhexa_client(test_server_url), class = "openhexa_config_error")
  expect_error(resolve_workspace(), class = "openhexa_config_error")
})

test_that("login() stores credentials for the session and logout() forgets them", {
  provide_credentials(server_url = NULL, token = NULL, workspace = NULL)
  login("login-token", test_server_url, "my-workspace", verify = FALSE)
  expect_equal(openhexa_client()$token, "login-token")
  expect_equal(resolve_workspace(), "my-workspace")
  expect_equal(resolve_workspace("other"), "other")
  logout()
  expect_error(openhexa_client(), class = "openhexa_config_error")
})

test_that("login() verifies the token", {
  provide_credentials()
  server$answer(Me = list(me = list(user = test_user())))
  expect_message(login(), "Ada")

  server$answer(Me = list(me = list(user = NULL)))
  expect_error(login(), class = "openhexa_auth_error")
})

test_that("printing a client hides the token", {
  client <- openhexa_client(test_server_url, "super-secret-token")
  output <- cli::cli_fmt(print(client))
  expect_false(any(grepl("super-secret", output)))
  expect_true(any(grepl("oken", output)))
})

test_that("SSL verification can be disabled", {
  provide_credentials(verify_ssl = FALSE)
  expect_false(verify_ssl())
  provide_credentials()
  expect_true(verify_ssl())
})

test_that("login() prompts for a missing token in interactive sessions", {
  provide_credentials(token = NULL, workspace = NULL)
  rlang::local_interactive()
  local_mocked_bindings(ask_token = function() "prompted")
  login(verify = FALSE)
  expect_equal(openhexa_client()$token, "prompted")
})

test_that("login() treats empty strings as missing values", {
  provide_credentials(token = "env-token", workspace = "env-workspace")
  login(token = "", server_url = "", workspace = "", verify = FALSE)
  expect_equal(openhexa_client()$token, "env-token")
  expect_equal(openhexa_client()$server_url, test_server_url)
  expect_equal(resolve_workspace(), "env-workspace")
})

test_that("login() prompts for a missing server URL in interactive sessions", {
  provide_credentials(server_url = NULL, token = "env-token", workspace = NULL)
  rlang::local_interactive()
  local_mocked_bindings(ask_server_url = function() "https://api.custom.example")
  login(verify = FALSE)
  expect_equal(openhexa_client()$server_url, "https://api.custom.example")
})

test_that("server URLs are accepted as users type or copy them", {
  expect_equal(normalize_server_url(" https://api.openhexa.example/graphql/ "), "https://api.openhexa.example")
  expect_equal(normalize_server_url("api.openhexa.example"), "https://api.openhexa.example") # https by default
  expect_equal(normalize_server_url("http://localhost:8000"), "http://localhost:8000")
})

test_that("login() offers the known servers", {
  provide_credentials(server_url = NULL, token = "env-token", workspace = NULL)
  rlang::local_interactive()
  local_mocked_bindings(ask_menu = function(choices, title) 2L)
  login(verify = FALSE)
  expect_equal(openhexa_client()$server_url, unname(known_servers[[2]]))

  logout()
  local_mocked_bindings(ask_menu = function(choices, title) 0L)
  expect_error(login(verify = FALSE), "No server selected", class = "openhexa_config_error")
})

test_that("login() offers to choose a workspace in the console", {
  provide_credentials(workspace = NULL)
  rlang::local_interactive()
  local_mocked_bindings(use_login_gadget = function() FALSE, ask_menu = function(choices, title) {
    expect_equal(choices, c('Test workspace ("test-workspace")', 'Other workspace ("other")'))
    2L
  })
  server$answer(
    Me = list(me = list(user = test_user())),
    Workspaces = list(workspaces = test_page(test_workspace(), test_workspace(slug = "other", name = "Other workspace"))),
    Workspace = list(workspace = test_workspace(slug = "other", name = "Other workspace"))
  )
  messages <- testthat::capture_messages(login())
  expect_match(messages[[2]], 'Using workspace Other workspace ("other")', fixed = TRUE)
  expect_equal(resolve_workspace(), "other")
})

test_that("set_workspace() without a workspace asks for one, or fails outside interactive sessions", {
  provide_credentials()
  expect_error(set_workspace(), "No workspace specified", class = "openhexa_input_error")

  rlang::local_interactive()
  local_mocked_bindings(use_login_gadget = function() FALSE, choose_workspace = function(client) "test-workspace")
  server$answer(Workspace = list(workspace = test_workspace()))
  expect_message(set_workspace(), "Test workspace")
  expect_equal(resolve_workspace(), "test-workspace")
})
