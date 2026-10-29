local_pipeline_run <- function(log_level = NA, .env = parent.frame()) {
  provide_credentials(token = "run-token", .env = .env)
  withr::local_envvar(
    HEXA_ENVIRONMENT = "CLOUD_PIPELINE", HEXA_RUN_ID = "run-1", HEXA_LOG_LEVEL = log_level,
    .local_envir = .env
  )
}

test_that("messages are sent to the pipeline run with the token of the run", {
  local_pipeline_run()
  login("user-token", "https://api.other.example", verify = FALSE)
  sent <- server$answer(LogPipelineMessage = function(vars, req) {
    expect_equal(httr2::req_get_headers(req, redacted = "reveal")$Authorization, "Bearer run-token")
    list(logPipelineMessage = list(success = TRUE, errors = list()))
  })
  current_run$log_info("Read ", 12, " rows")
  current_run$log_critical("Stop")
  expect_equal(sent$calls[[1]]$variables$input, list(priority = "INFO", message = "Read 12 rows"))
  expect_equal(sent$calls[[2]]$variables$input$priority, "CRITICAL")
  expect_equal(current_run$id, "run-1")
  expect_match(cli::cli_fmt(print(current_run)), "run-1")
})

test_that("messages below the log level of the run are ignored", {
  local_pipeline_run(log_level = "2")
  sent <- server$answer(LogPipelineMessage = list(logPipelineMessage = list(success = TRUE, errors = list())))
  current_run$log_debug("debug")
  current_run$log_info("info")
  current_run$log_warning("warning")
  expect_length(sent$calls, 1)
  expect_equal(sent$calls[[1]]$variables$input$priority, "WARNING")
})

test_that("the log level is read as a number or a name, INFO by default", {
  withr::local_envvar(HEXA_LOG_LEVEL = NA)
  expect_equal(run_log_level(), "INFO")
  withr::local_envvar(HEXA_LOG_LEVEL = "0")
  expect_equal(run_log_level(), "DEBUG")
  withr::local_envvar(HEXA_LOG_LEVEL = "error")
  expect_equal(run_log_level(), "ERROR")
  withr::local_envvar(HEXA_LOG_LEVEL = "nonsense")
  expect_equal(run_log_level(), "INFO")
})

test_that("a message that can't be sent raises a warning, not an error", {
  local_pipeline_run()
  server$answer(LogPipelineMessage = list(logPipelineMessage = test_failure("PIPELINE_ALREADY_COMPLETED")))
  expect_warning(current_run$log_info("late"), "PIPELINE_ALREADY_COMPLETED")

  server$answer(LogPipelineMessage = function(vars) stop("Connection refused"))
  expect_warning(current_run$log_info("lost"), "Could not send")
})

test_that("outside a pipeline, messages are printed in the console", {
  provide_credentials()
  withr::local_envvar(HEXA_ENVIRONMENT = "CLOUD_JUPYTER", HEXA_LOG_LEVEL = NA)
  sent <- server$answer()
  expect_message(current_run$log_warning("No data for ", 3, " districts"), "WARNING No data for 3 districts")
  expect_silent(current_run$log_debug("hidden"))
  expect_length(sent$calls, 0)
  expect_null(current_run$id)
  expect_match(cli::cli_fmt(print(current_run)), "Not in a pipeline run")
})
