test_that("list_pipelines() lists pipelines with their last run", {
  server$answer(Pipelines = list(pipelines = test_page(
    list(id = "p1", code = "extract", name = "Extract", description = NULL, type = "zipFile", schedule = "0 * * * *",
         createdAt = "2024-01-01T00:00:00Z", updatedAt = NULL, currentVersion = list(id = "pv1", versionNumber = 3, versionName = "v3"),
         lastRuns = list(items = list(list(id = "r1", status = "success", executionDate = "2024-01-02T00:00:00Z")))),
    list(id = "p2", code = "new", name = "New", description = NULL, type = "notebook", schedule = NULL,
         createdAt = "2024-01-01T00:00:00Z", updatedAt = NULL, currentVersion = NULL, lastRuns = list(items = list()))
  )))
  out <- list_pipelines()
  expect_equal(out$code, c("extract", "new"))
  expect_equal(out$last_run_status, c("success", NA))
  expect_equal(out$version, c(3L, NA))
})

test_that("get_pipeline() describes the parameters", {
  server$answer(PipelineByCode = list(pipelineByCode = test_pipeline()))
  pipeline <- get_pipeline("extract")
  expect_equal(pipeline$parameters$code, "year")
  expect_equal(pipeline$version$number, 3)
  expect_no_error(cli::cli_fmt(print(pipeline)))
})

test_that("run_pipeline() runs the pipeline with its config", {
  sent <- server$answer(
    PipelineByCode = list(pipelineByCode = test_pipeline()),
    RunPipeline = list(runPipeline = test_success(run = list(id = "r1"))),
    PipelineRun = list(pipelineRun = test_run(status = "queued"))
  )
  run <- run_pipeline("extract", config = list(year = 2023))
  expect_s3_class(run, "openhexa_pipeline_run")
  expect_equal(run$status, "queued")
  input <- sent$calls[[2]]$variables$input
  expect_equal(input$id, "p1")
  expect_equal(input$config, list(year = 2023))
  expect_no_error(cli::cli_fmt(print(run)))
})

test_that("run_pipeline() validates the config", {
  expect_error(run_pipeline("extract", config = list(1)), class = "openhexa_input_error")
  expect_error(run_pipeline("extract", config = "year=1"), class = "openhexa_input_error")
})

test_that("wait_pipeline_run() polls until the run finishes", {
  statuses <- c("queued", "running", "success", "success")
  i <- 0
  sent <- server$answer(PipelineRun = function(vars) {
    i <<- i + 1
    run <- test_run(status = statuses[[i]])
    if (isTRUE(vars$withLogs)) run$logs <- "done"
    list(pipelineRun = run)
  })
  expect_message(run <- wait_pipeline_run("r1", interval = 0), "success")
  expect_equal(run$status, "success")
  # Polls without the logs, then fetches them once the run is finished.
  expect_equal(vapply(sent$calls, function(call) isTRUE(call$variables$withLogs), logical(1)), c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(run$logs, "done", ignore_attr = TRUE)
})

test_that("wait_pipeline_run() warns about failed runs and honours the timeout", {
  server$answer(PipelineRun = list(pipelineRun = test_run(status = "failed")))
  expect_warning(suppressMessages(wait_pipeline_run("r1", interval = 0)), "failed")

  server$answer(PipelineRun = list(pipelineRun = test_run(status = "running")))
  expect_error(suppressMessages(wait_pipeline_run("r1", interval = 1, timeout = 0)), class = "openhexa_timeout_error")
})

test_that("list_pipeline_runs() returns at most n runs", {
  sent <- server$answer(PipelineRuns = function(vars) {
    items <- lapply(seq_len(vars$perPage), function(i) {
      list(id = paste0("r", i), status = "success", executionDate = NULL, duration = 1, triggerMode = "manual", user = NULL, version = NULL)
    })
    list(pipelineByCode = list(runs = list(totalPages = 10, items = items)))
  })
  expect_equal(nrow(list_pipeline_runs("extract", n = 3)), 3)
  expect_length(sent$calls, 1)
})

test_that("stopping a finished run is explained", {
  server$answer(StopPipeline = list(stopPipeline = test_failure("PIPELINE_ALREADY_COMPLETED")))
  expect_error(stop_pipeline_run("r1"), "already finished", class = "openhexa_mutation_error")
})

test_that("get_pipeline_run() fetches the logs, unless told not to", {
  server$answer(PipelineRun = function(vars) {
    run <- test_run()
    if (isTRUE(vars$withLogs)) run$logs <- "line 1\nline 2"
    list(pipelineRun = run)
  })
  expect_equal(get_pipeline_run("r1")$logs, "line 1\nline 2", ignore_attr = TRUE)
  expect_null(get_pipeline_run("r1", logs = FALSE)$logs)
})

test_that("logs are printed line by line", {
  server$answer(PipelineRun = list(pipelineRun = test_run(logs = "line 1\nline 2")))
  logs <- get_pipeline_run("r1")$logs
  expect_output(print(logs), "^line 1\nline 2$")
  expect_true(grepl("line 2", logs))
})
