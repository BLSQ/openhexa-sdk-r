#' List pipelines
#'
#' @param name Only return pipelines whose name matches this search term.
#' @inheritParams get_workspace
#' @return A tibble with one row per pipeline, including the status of its last run.
#' @export
list_pipelines <- function(name = NULL, workspace = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  items <- paginate(function(page, per_page) {
    graphql_op("Pipelines", list(workspaceSlug = workspace, name = name, page = page, perPage = per_page), client)$pipelines
  })
  records_to_tibble(items, list(
    code = col_chr("code"),
    name = col_chr("name"),
    description = col_chr("description"),
    type = col_chr("type"),
    schedule = col_chr("schedule"),
    version = col_int("currentVersion", "versionNumber"),
    last_run_status = col_chr("lastRuns", "items", 1, "status"),
    last_run_at = col_dttm("lastRuns", "items", 1, "executionDate"),
    created_at = col_dttm("createdAt"),
    id = col_chr("id")
  ))
}

#' Get a pipeline
#'
#' @param code The pipeline code, as shown in the URL of the pipeline page.
#' @inheritParams get_workspace
#' @return An `openhexa_pipeline` object: a list with `id`, `code`, `name`, `description`, `type`, `schedule`,
#'   `version` and `parameters` (a tibble describing the parameters accepted by [run_pipeline()]).
#' @export
get_pipeline <- function(code, workspace = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  data <- graphql_op("PipelineByCode", list(code = code, workspaceSlug = workspace), client)$pipelineByCode
  if (is.null(data)) {
    cli::cli_abort(
      "Pipeline {.val {code}} does not exist in workspace {.val {workspace}}.",
      class = c("openhexa_not_found_error", "openhexa_error")
    )
  }

  version <- data$currentVersion
  structure(
    list(
      id = data$id,
      code = data$code,
      name = data$name,
      description = data$description,
      type = data$type,
      schedule = data$schedule,
      version = if (!is.null(version)) {
        list(id = version$id, number = version$versionNumber, name = version$versionName)
      },
      parameters = records_to_tibble(version$parameters %||% list(), list(
        code = col_chr("code"),
        name = col_chr("name"),
        type = col_chr("type"),
        required = col_lgl("required"),
        multiple = col_lgl("multiple"),
        default = col_list("default"),
        choices = col_list("choices"),
        help = col_chr("help")
      )),
      workspace = workspace
    ),
    class = "openhexa_pipeline"
  )
}

#' @export
print.openhexa_pipeline <- function(x, ...) {
  cli::cli_text("<openhexa_pipeline> {.strong {x$name}} ({.val {x$code}}, {tolower(x$type)})")
  if (!is.null(x$version)) cli::cli_text("Version: {x$version$number} ({x$version$name})")
  if (!is.null(x$schedule)) cli::cli_text("Schedule: {.code {x$schedule}}")
  if (nrow(x$parameters) > 0) {
    cli::cli_text("Parameters:")
    for (i in seq_len(nrow(x$parameters))) {
      p <- x$parameters[i, ]
      cli::cli_bullets(c("*" = "{.field {p$code}} ({p$type}{if (p$required) ', required'}): {p$name}"))
    }
  }
  invisible(x)
}

#' Run a pipeline
#'
#' Runs the current version of a pipeline.
#'
#' @param code The pipeline code.
#' @param config A named list of parameter values, e.g. `list(year = 2024, countries = I("BFA"))`. Use [get_pipeline()]
#'   to see the parameters of a pipeline. Wrap a single value in [I()] to send it as a list for parameters that
#'   accept several values.
#' @param wait If `TRUE`, wait for the run to finish and return its final state, with its logs (see
#'   [wait_pipeline_run()]).
#' @param send_mail_notifications Whether to notify the pipeline recipients by email when the run finishes.
#' @param enable_debug_logs Whether to record debug-level messages.
#' @inheritParams get_workspace
#' @return An `openhexa_pipeline_run` object, see [get_pipeline_run()].
#' @export
#' @examples
#' \dontrun{
#' run <- run_pipeline("dhis2-extract", config = list(start_date = "2024-01-01"), wait = TRUE)
#' run$status
#' }
run_pipeline <- function(code, config = list(), wait = FALSE, send_mail_notifications = FALSE,
                             enable_debug_logs = FALSE, workspace = NULL, client = NULL) {
  if (!is.list(config) || (length(config) > 0 && !rlang::is_named(config))) {
    cli::cli_abort("{.arg config} must be a named list of parameter values.", class = c("openhexa_input_error", "openhexa_error"))
  }
  pipeline <- get_pipeline(code, workspace, client)
  input <- list(
    id = pipeline$id,
    config = as_json_object(config),
    sendMailNotifications = send_mail_notifications,
    enableDebugLogs = enable_debug_logs
  )
  result <- graphql_op("RunPipeline", list(input = input), client)$runPipeline
  check_mutation(result, c(
    INVALID_CONFIG = cli::format_inline("Invalid {.arg config}: check the parameters with {.code get_pipeline(\"{code}\")}."),
    PERMISSION_DENIED = "You don't have permission to run this pipeline.",
    PIPELINE_RUNS_LIMIT_REACHED = "The limit of concurrent runs for this pipeline is reached.",
    PIPELINE_VERSION_NOT_FOUND = "This pipeline has no version to run."
  ))

  if (wait) wait_pipeline_run(result$run$id, client = client) else get_pipeline_run(result$run$id, logs = FALSE, client = client)
}

#' List the runs of a pipeline
#'
#' @inheritParams get_pipeline
#' @param n The maximum number of runs to return, latest first.
#' @return A tibble with one row per run.
#' @export
list_pipeline_runs <- function(code, n = 20, workspace = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  items <- paginate(
    function(page, per_page) {
      vars <- list(code = code, workspaceSlug = workspace, page = page, perPage = per_page)
      graphql_op("PipelineRuns", vars, client)$pipelineByCode$runs
    },
    per_page = min(n, 50L),
    max_items = n
  )
  if (is.null(items)) {
    cli::cli_abort(
      "Pipeline {.val {code}} does not exist in workspace {.val {workspace}}.",
      class = c("openhexa_not_found_error", "openhexa_error")
    )
  }
  records_to_tibble(items, list(
    id = col_chr("id"),
    status = col_chr("status"),
    execution_date = col_dttm("executionDate"),
    duration = col_int("duration"),
    trigger = col_chr("triggerMode"),
    user = col_chr("user", "displayName"),
    version = col_int("version", "versionNumber")
  ))
}

#' Get a pipeline run
#'
#' @param run The run id, or an `openhexa_pipeline_run` object.
#' @param logs Whether to also fetch the run logs. Set it to `FALSE` to only check the status of a run, as the logs can
#'   be large.
#' @inheritParams get_user
#' @return An `openhexa_pipeline_run` object: a list with `id`, `pipeline`, `status`, `execution_date`, `duration`
#'   (in seconds), `progress` (0-100), `config`, `messages` (a tibble) and `logs`.
#' @export
get_pipeline_run <- function(run, logs = TRUE, client = NULL) {
  id <- run_id(run)
  data <- graphql_op("PipelineRun", list(id = id, withLogs = logs), client)$pipelineRun
  if (is.null(data)) {
    cli::cli_abort("Pipeline run {.val {id}} does not exist.", class = c("openhexa_not_found_error", "openhexa_error"))
  }
  structure(
    list(
      id = data$id,
      pipeline = data$pipeline$code,
      status = data$status,
      execution_date = parse_datetime(data$executionDate %||% NA_character_),
      duration = data$duration,
      progress = data$progress,
      config = data$config,
      trigger = data$triggerMode,
      user = data$user$displayName,
      version = data$version$versionNumber,
      messages = records_to_tibble(data$messages, list(
        timestamp = col_dttm("timestamp"),
        priority = col_chr("priority"),
        message = col_chr("message")
      )),
      logs = if (!is.null(data$logs)) structure(data$logs, class = c("openhexa_logs", "character"))
    ),
    class = "openhexa_pipeline_run"
  )
}

#' @export
print.openhexa_logs <- function(x, ...) {
  cat(x, sep = "\n")
  invisible(x)
}

#' @export
print.openhexa_pipeline_run <- function(x, ...) {
  cli::cli_text("<openhexa_pipeline_run> {.val {x$pipeline}} run {.val {x$id}}")
  cli::cli_text("Status: {.strong {x$status}} ({x$progress}%)")
  if (!is.na(x$execution_date)) cli::cli_text("Started: {format(x$execution_date, usetz = TRUE)}")
  if (!is.null(x$duration)) cli::cli_text("Duration: {x$duration}s")
  if (nrow(x$messages) > 0) cli::cli_text("Messages: {nrow(x$messages)}")
  invisible(x)
}

#' Wait for a pipeline run to finish
#'
#' Polls the run until it succeeds, fails, is stopped or skipped. A warning is raised if the run did not succeed.
#'
#' @inheritParams get_pipeline_run
#' @param interval Seconds between two status checks.
#' @param timeout Maximum number of seconds to wait.
#' @return The finished run, as an `openhexa_pipeline_run` object, with its `logs`.
#' @export
wait_pipeline_run <- function(run, interval = 5, timeout = Inf, client = NULL) {
  id <- run_id(run)
  start <- Sys.time()
  status <- NULL
  repeat {
    run <- get_pipeline_run(id, logs = FALSE, client = client)
    if (!identical(run$status, status)) {
      status <- run$status
      cli::cli_inform("Run {.val {id}} is {.strong {status}}.")
    }
    if (status %in% finished_run_statuses) {
      # The logs are only fetched once the run is finished, as they can be large.
      run <- get_pipeline_run(id, logs = TRUE, client = client)
      break
    }
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) + interval > timeout) {
      cli::cli_abort(
        "Run {.val {id}} did not finish within {timeout} seconds.",
        class = c("openhexa_timeout_error", "openhexa_error")
      )
    }
    Sys.sleep(interval)
  }
  if (status != "success") {
    cli::cli_warn("Run {.val {id}} finished with status {.strong {status}}. See {.code run$messages}.")
  }
  run
}

#' Stop a pipeline run
#'
#' @inheritParams get_pipeline_run
#' @return The run id, invisibly.
#' @export
stop_pipeline_run <- function(run, client = NULL) {
  id <- run_id(run)
  result <- graphql_op("StopPipeline", list(input = list(runId = id)), client)$stopPipeline
  check_mutation(result, c(
    PIPELINE_ALREADY_COMPLETED = "This run has already finished.",
    PIPELINE_ALREADY_STOPPED = "This run has already been stopped.",
    PERMISSION_DENIED = "You don't have permission to stop this run."
  ))
  invisible(id)
}

finished_run_statuses <- c("success", "failed", "stopped", "skipped")

run_id <- function(run) {
  if (inherits(run, "openhexa_pipeline_run")) run$id else run
}
