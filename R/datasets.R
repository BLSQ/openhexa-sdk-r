#' List datasets
#'
#' Lists the datasets of a workspace, including datasets shared with it by other workspaces.
#'
#' @param query Only return datasets whose name matches this search term.
#' @inheritParams get_workspace
#' @return A tibble with one row per dataset.
#' @export
list_datasets <- function(workspace = NULL, query = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  items <- paginate(function(page, per_page) {
    vars <- list(workspaceSlug = workspace, query = query, page = page, perPage = per_page)
    graphql_op("Datasets", vars, client)$workspace$datasets
  })
  if (is.null(items)) abort_workspace_not_found(workspace)

  records_to_tibble(items, list(
    slug = col_chr("dataset", "slug"),
    name = col_chr("dataset", "name"),
    description = col_chr("dataset", "description"),
    source_workspace = col_chr("dataset", "workspace", "slug"),
    latest_version = col_chr("dataset", "latestVersion", "name"),
    pinned = col_lgl("isPinned"),
    created_at = col_dttm("dataset", "createdAt"),
    updated_at = col_dttm("dataset", "updatedAt"),
    id = col_chr("dataset", "id")
  ))
}

#' Get a dataset
#'
#' @param dataset The dataset slug.
#' @inheritParams get_workspace
#' @return An `openhexa_dataset` object: a list with `id`, `slug`, `name`, `description`, `source_workspace` and
#'   `latest_version`. It can be passed to the other dataset functions instead of a slug, which saves an API call.
#' @export
get_dataset <- function(dataset, workspace = NULL, client = NULL) {
  if (inherits(dataset, "openhexa_dataset")) {
    return(dataset)
  }
  workspace <- resolve_workspace(workspace)
  link <- graphql_op("DatasetLinkBySlug", list(datasetSlug = dataset, workspaceSlug = workspace), client)$datasetLinkBySlug
  if (is.null(link)) {
    cli::cli_abort(
      c(
        "Dataset {.val {dataset}} does not exist in workspace {.val {workspace}}.",
        i = "Check the slug with {.fn list_datasets}, which also lists the datasets shared with this workspace."
      ),
      class = c("openhexa_not_found_error", "openhexa_error")
    )
  }

  data <- link$dataset
  structure(
    list(
      id = data$id,
      slug = data$slug,
      name = data$name,
      description = data$description,
      source_workspace = data$workspace$slug,
      latest_version = if (!is.null(data$latestVersion)) version_from_api(data$latestVersion),
      created_at = parse_datetime(data$createdAt),
      updated_at = parse_datetime(data$updatedAt)
    ),
    class = "openhexa_dataset"
  )
}

#' @export
print.openhexa_dataset <- function(x, ...) {
  cli::cli_text("<openhexa_dataset> {.strong {x$name}} ({.val {x$slug}}, from workspace {.val {x$source_workspace}})")
  if (!is.null(x$description) && nzchar(x$description)) cli::cli_text(x$description)
  cli::cli_text("Latest version: {if (is.null(x$latest_version)) 'none' else x$latest_version$name}")
  invisible(x)
}

#' List the versions of a dataset
#'
#' @param dataset A dataset slug, or an `openhexa_dataset` object from [get_dataset()].
#' @inheritParams get_workspace
#' @return A tibble with one row per version, latest first.
#' @export
list_dataset_versions <- function(dataset, workspace = NULL, client = NULL) {
  dataset <- get_dataset(dataset, workspace, client)
  items <- paginate(function(page, per_page) {
    graphql_op("DatasetVersions", list(datasetId = dataset$id, page = page, perPage = per_page), client)$dataset$versions
  })
  records_to_tibble(items, list(
    name = col_chr("name"),
    changelog = col_chr("changelog"),
    created_at = col_dttm("createdAt"),
    created_by = col_chr("createdBy", "displayName"),
    id = col_chr("id")
  ))
}

#' List the files of a dataset version
#'
#' @inheritParams list_dataset_versions
#' @param version The name of the version. Defaults to the latest version.
#' @return A tibble with one row per file.
#' @export
list_dataset_files <- function(dataset, version = NULL, workspace = NULL, client = NULL) {
  dataset <- get_dataset(dataset, workspace, client)
  version <- resolve_version(dataset, version, client)
  items <- paginate(function(page, per_page) {
    graphql_op("DatasetVersionFiles", list(versionId = version$id, page = page, perPage = per_page), client)$datasetVersion$files
  })
  files_to_tibble(items)
}

#' Download a dataset file
#'
#' @inheritParams list_dataset_files
#' @param filename The name of the file in the dataset version.
#' @param dest Where to save the file. Defaults to `filename` in the working directory.
#' @param overwrite Whether to overwrite `dest` if it already exists.
#' @return The path to the downloaded file, invisibly.
#' @export
download_dataset_file <- function(dataset, filename, dest = basename(filename), version = NULL,
                                      overwrite = FALSE, workspace = NULL, client = NULL) {
  check_dest(dest, overwrite)
  file <- dataset_file(dataset, filename, version, workspace, client)
  result <- graphql_op("PrepareVersionFileDownload", list(input = list(fileId = file$id)), client)$prepareVersionFileDownload
  check_mutation(result)
  download_signed_url(result$downloadUrl, dest)
}

#' Read a dataset file into R
#'
#' Downloads the file to a temporary location and reads it with a reader picked from its extension:
#' CSV and TSV (with readr if installed), Parquet (requires arrow), Excel (requires readxl), JSON and RDS.
#'
#' @inheritParams download_dataset_file
#' @param reader A function to read the file with, called as `reader(path, ...)`. Overrides the
#'   extension-based choice.
#' @param ... Passed to the reader.
#' @return The file contents, usually a tibble.
#' @export
#' @examples
#' \dontrun{
#' population <- get_dataset_file("population", "population_2024.csv")
#' }
get_dataset_file <- function(dataset, filename, version = NULL, reader = NULL, ...,
                                  workspace = NULL, client = NULL) {
  path <- tempfile(fileext = paste0(".", tools::file_ext(filename)))
  on.exit(unlink(path), add = TRUE)
  download_dataset_file(dataset, filename, path, version = version, workspace = workspace, client = client)
  read_by_extension(path, reader, ...)
}

#' Read all the tables of a dataset version
#'
#' Reads every tabular file (CSV, TSV, Parquet and Excel) of a dataset version and stacks them in a single tibble,
#' with a `file` column telling which file each row comes from. Columns missing from some files are filled with
#' `NA`. Use [get_dataset_file()] to read a single file.
#'
#' @inheritParams list_dataset_files
#' @param pattern A regular expression: only read the files whose name matches it, e.g. `"^cases_"`.
#' @param ... Passed to the reader of each file, see [get_dataset_file()].
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{
#' cases <- get_dataset_data("routine-data", pattern = "^cases_")
#' table(cases$file)
#' }
get_dataset_data <- function(dataset, version = NULL, pattern = NULL, ..., workspace = NULL, client = NULL) {
  dataset <- get_dataset(dataset, workspace, client)
  version <- resolve_version(dataset, version, client)
  files <- list_dataset_files(dataset, version, client = client)$filename
  files <- files[tolower(tools::file_ext(files)) %in% c("csv", "tsv", "parquet", "xlsx", "xls")]
  if (!is.null(pattern)) files <- files[grepl(pattern, files)]
  if (length(files) == 0) {
    cli::cli_abort(
      "Version {.val {version$name}} of dataset {.val {dataset$slug}} has no tabular file{if (!is.null(pattern)) ' matching the pattern'}.",
      class = c("openhexa_not_found_error", "openhexa_error")
    )
  }

  tables <- lapply(rlang::set_names(files), function(filename) {
    tibble::as_tibble(get_dataset_file(dataset, filename, version, ..., client = client))
  })
  data <- vctrs::vec_rbind(!!!tables, .names_to = "file")
  data[c(setdiff(names(data), "file"), "file")]
}

#' Create a dataset
#'
#' @param name The name of the dataset.
#' @param description A description of the dataset.
#' @inheritParams get_workspace
#' @return The new dataset, as an `openhexa_dataset` object.
#' @export
create_dataset <- function(name, description = NULL, workspace = NULL, client = NULL) {
  workspace <- resolve_workspace(workspace)
  input <- compact(list(workspaceSlug = workspace, name = name, description = description))
  result <- graphql_op("CreateDataset", list(input = input), client)$createDataset
  check_mutation(result, c(PERMISSION_DENIED = "You don't have permission to create datasets in this workspace."))
  get_dataset(result$dataset$slug, workspace, client)
}

#' Create a dataset version
#'
#' Files can only be added to the latest version of a dataset, so create a version then add files to it
#' with [add_dataset_file()].
#'
#' @inheritParams list_dataset_versions
#' @param name The name of the version, e.g. `"2024-Q1"` or `"v2"`.
#' @param changelog A description of what changed in this version.
#' @return A one-row tibble describing the new version.
#' @export
create_dataset_version <- function(dataset, name, changelog = NULL, workspace = NULL, client = NULL) {
  dataset <- get_dataset(dataset, workspace, client)
  input <- compact(list(datasetId = dataset$id, name = as.character(name), changelog = changelog))
  result <- graphql_op("CreateDatasetVersion", list(input = input), client)$createDatasetVersion
  check_mutation(result, c(
    DUPLICATE_NAME = "A version with this name already exists in this dataset.",
    PERMISSION_DENIED = "You don't have permission to create versions of this dataset."
  ))
  records_to_tibble(list(result$version), list(
    name = col_chr("name"),
    changelog = col_chr("changelog"),
    created_at = col_dttm("createdAt"),
    id = col_chr("id")
  ))
}

#' Add a file to a dataset version
#'
#' @inheritParams list_dataset_versions
#' @param path The local file to upload.
#' @param filename The name of the file in the dataset. Defaults to the name of the local file.
#' @param version The name of the version to add the file to. Only the latest version (the default)
#'   accepts new files.
#' @return A one-row tibble describing the new file.
#' @export
#' @examples
#' \dontrun{
#' create_dataset_version("population", "2024")
#' add_dataset_file("population", "population_2024.csv")
#' }
add_dataset_file <- function(dataset, path, filename = basename(path), version = NULL,
                                 workspace = NULL, client = NULL) {
  if (!file.exists(path)) {
    cli::cli_abort("{.file {path}} does not exist.", class = c("openhexa_file_not_found_error", "openhexa_error"))
  }
  dataset <- get_dataset(dataset, workspace, client)
  version <- resolve_version(dataset, version, client)
  content_type <- guess_content_type(filename)
  errors <- c(
    ALREADY_EXISTS = cli::format_inline("A file named {.val {filename}} already exists in this version."),
    LOCKED_VERSION = cli::format_inline("Version {.val {version$name}} is locked: only the latest version accepts new files."),
    PERMISSION_DENIED = "You don't have permission to add files to this dataset."
  )

  input <- list(versionId = version$id, contentType = content_type, uri = filename)
  upload <- graphql_op("GenerateDatasetUploadUrl", list(input = input), client)$generateDatasetUploadUrl
  check_mutation(upload, errors)
  upload_signed_url(upload$uploadUrl, path, content_type, upload$headers)

  result <- graphql_op("CreateDatasetVersionFile", list(input = input), client)$createDatasetVersionFile
  check_mutation(result, errors)
  files_to_tibble(list(result$file))
}

resolve_version <- function(dataset, version, client) {
  if (is.list(version)) {
    return(version)
  }
  if (is.null(version)) {
    if (is.null(dataset$latest_version)) {
      cli::cli_abort(
        c("Dataset {.val {dataset$slug}} has no versions yet.", i = "Create one with {.fn create_dataset_version}."),
        class = c("openhexa_not_found_error", "openhexa_error")
      )
    }
    return(dataset$latest_version)
  }
  versions <- list_dataset_versions(dataset, client = client)
  match <- versions[versions$name == version | versions$id == version, ]
  if (nrow(match) == 0) {
    cli::cli_abort(
      "Dataset {.val {dataset$slug}} has no version {.val {version}}.",
      class = c("openhexa_not_found_error", "openhexa_error")
    )
  }
  as.list(match[1, ])
}

dataset_file <- function(dataset, filename, version, workspace, client) {
  dataset <- get_dataset(dataset, workspace, client)
  version <- resolve_version(dataset, version, client)
  vars <- list(versionId = version$id, filename = filename)
  file <- graphql_op("DatasetVersionFileByName", vars, client)$datasetVersion$fileByName
  if (is.null(file)) {
    cli::cli_abort(
      "File {.val {filename}} does not exist in version {.val {version$name}} of dataset {.val {dataset$slug}}.",
      class = c("openhexa_not_found_error", "openhexa_error")
    )
  }
  file
}

version_from_api <- function(version) {
  list(
    id = version$id,
    name = version$name,
    changelog = version$changelog,
    created_at = parse_datetime(version$createdAt)
  )
}

files_to_tibble <- function(items) {
  records_to_tibble(items, list(
    filename = col_chr("filename"),
    content_type = col_chr("contentType"),
    size = col_dbl("size"),
    created_at = col_dttm("createdAt"),
    uri = col_chr("uri"),
    id = col_chr("id")
  ))
}

abort_workspace_not_found <- function(workspace) {
  cli::cli_abort(
    "Workspace {.val {workspace}} does not exist, or you don't have access to it.",
    class = c("openhexa_not_found_error", "openhexa_error")
  )
}
