#' List workspace files
#'
#' Lists the files and directories stored in the workspace, at the given location.
#'
#' @param prefix The directory to list, e.g. `"data/"`. Defaults to the root of the workspace.
#' @param query Only return files whose name matches this search term.
#' @inheritParams get_workspace
#' @return A tibble with one row per file or directory.
#' @export
list_files <- function(prefix = NULL, query = NULL, workspace = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  items <- paginate(function(page, per_page) {
    vars <- list(workspaceSlug = workspace, prefix = prefix, query = query, page = page, perPage = per_page)
    graphql_op("BucketObjects", vars, client)$workspace$bucket$objects
  })
  if (is.null(items)) abort_workspace_not_found(workspace)

  files <- records_to_tibble(items, list(
    path = col_chr("key"),
    name = col_chr("name"),
    type = col_chr("type"),
    size = col_dbl("size"),
    updated_at = col_dttm("updatedAt")
  ))
  files$type <- tolower(files$type)
  files
}

#' Download a workspace file
#'
#' @param path The path of the file in the workspace, e.g. `"data/population.csv"`.
#' @param dest Where to save the file. Defaults to the file name in the working directory.
#' @param overwrite Whether to overwrite `dest` if it already exists.
#' @inheritParams get_workspace
#' @return The path to the downloaded file, invisibly.
#' @export
download_file <- function(path, dest = basename(path), overwrite = FALSE, workspace = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  check_dest(dest, overwrite)
  input <- list(workspaceSlug = workspace, objectKey = path)
  result <- graphql_op("PrepareObjectDownload", list(input = input), client)$prepareObjectDownload
  check_mutation(result, c(
    NOT_FOUND = cli::format_inline("File {.val {path}} does not exist in workspace {.val {workspace}}."),
    PERMISSION_DENIED = "You don't have permission to download files from this workspace."
  ))
  download_signed_url(result$downloadUrl, dest)
}

#' Read a workspace file into R
#'
#' Inside OpenHEXA, where the workspace files are mounted locally, the file is read directly. Elsewhere, it is
#' downloaded first. The reader is picked from the file extension, see [get_dataset_file()].
#'
#' @inheritParams download_file
#' @inheritParams get_dataset_file
#' @return The file contents, usually a tibble.
#' @export
#' @examples
#' \dontrun{
#' population <- get_file("data/population.csv")
#' }
get_file <- function(path, reader = NULL, ..., workspace = NULL, client = NULL) {
  local <- workspace_path(path)
  if (file.exists(local)) {
    return(read_by_extension(local, reader, ...))
  }
  dest <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  on.exit(unlink(dest), add = TRUE)
  download_file(path, dest, workspace = workspace, client = client)
  read_by_extension(dest, reader, ...)
}

#' Upload a file to the workspace
#'
#' Existing files at the same path are replaced.
#'
#' @param file The local file to upload.
#' @param path The destination path in the workspace, e.g. `"data/population.csv"`. Defaults to the
#'   name of the local file, at the root of the workspace.
#' @inheritParams get_workspace
#' @return `path`, invisibly.
#' @export
upload_file <- function(file, path = basename(file), workspace = NULL, client = NULL) {
  if (!file.exists(file)) {
    cli::cli_abort("{.file {file}} does not exist.", class = c("openhexa_file_not_found_error", "openhexa_error"))
  }
  workspace <- resolve_workspace(workspace)
  content_type <- guess_content_type(file)
  input <- list(workspaceSlug = workspace, objectKey = path, contentType = content_type)
  result <- graphql_op("PrepareObjectUpload", list(input = input), client)$prepareObjectUpload
  check_mutation(result, c(
    INVALID_PATH = cli::format_inline("{.val {path}} is not a valid file path."),
    PERMISSION_DENIED = "You don't have permission to upload files to this workspace."
  ))
  upload_signed_url(result$uploadUrl, file, content_type, result$headers)
  invisible(path)
}
