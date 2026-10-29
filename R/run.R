#' The current pipeline run
#'
#' `current_run` represents the pipeline run that your code is running in, like `current_run` in the Python SDK.
#' Use it when R code runs as part of an OpenHEXA pipeline (an R notebook pipeline, or an R script started by a
#' pipeline) to get the ID of the run and to send messages, shown on the run page of the web app. Outside a
#' pipeline, there is no run: the messages are printed in the console instead, so the same code works while you
#' develop it.
#'
#' * `current_run$log_debug(...)`, `current_run$log_info(...)`, `current_run$log_warning(...)`,
#'   `current_run$log_error(...)` and `current_run$log_critical(...)` send a message with that level. The arguments
#'   are pasted together, as with [message()]. Messages below the log level chosen when starting the run (`INFO` by
#'   default) are ignored.
#' * `current_run$id()` returns the ID of the run, or `NULL` outside a pipeline.
#'
#' A pipeline run is detected from the environment variables that OpenHEXA sets for it (`HEXA_ENVIRONMENT`,
#' `HEXA_SERVER_URL`, `HEXA_TOKEN`): messages are sent with the token of the run, not with the credentials set with
#' [login()]. A message that cannot be sent raises a warning, not an error, so that logging never stops a pipeline.
#'
#' @format A list of functions.
#' @export
#' @examples
#' current_run$log_info("Starting the extraction")
#' current_run$log_warning("No data for ", 3, " districts")
#'
#' \dontrun{
#' cases <- get_dataset_data("malaria-cases")
#' current_run$log_info("Read ", nrow(cases), " rows")
#' if (anyNA(cases$value)) current_run$log_error("Missing values in the cases")
#' }
current_run <- local({
  logger <- function(level) {
    force(level)
    function(...) log_run_message(level, paste0(...))
  }
  list(
    id = function() if (in_pipeline_run()) env_or_null("HEXA_RUN_ID"),
    log_debug = logger("DEBUG"),
    log_info = logger("INFO"),
    log_warning = logger("WARNING"),
    log_error = logger("ERROR"),
    log_critical = logger("CRITICAL")
  )
})

log_levels <- c("DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL")

in_pipeline_run <- function() {
  toupper(Sys.getenv("HEXA_ENVIRONMENT")) == "CLOUD_PIPELINE" &&
    !is.null(env_or_null("HEXA_SERVER_URL")) && !is.null(env_or_null("HEXA_TOKEN"))
}

run_log_level <- function() {
  value <- toupper(trimws(Sys.getenv("HEXA_LOG_LEVEL")))
  if (grepl("^[0-4]$", value)) return(log_levels[[as.integer(value) + 1L]])
  if (value %in% log_levels) value else "INFO"
}

log_run_message <- function(level, message) {
  if (match(level, log_levels) < match(run_log_level(), log_levels)) return(invisible(NULL))
  if (!in_pipeline_run()) {
    message(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), " ", level, " ", message)
    return(invisible(NULL))
  }
  client <- openhexa_client(env_or_null("HEXA_SERVER_URL"), env_or_null("HEXA_TOKEN"))
  result <- tryCatch(
    graphql_op("LogPipelineMessage", list(input = list(priority = level, message = message)), client = client),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    cli::cli_warn(c("Could not send the message to the pipeline run.", x = plain_message(result)))
  } else if (!isTRUE(result$logPipelineMessage$success)) {
    codes <- as.character(unlist(result$logPipelineMessage$errors))
    cli::cli_warn(c("Could not send the message to the pipeline run.", x = "Error code{?s}: {.val {codes}}."))
  }
  invisible(NULL)
}
