#' Create an R notebook in the workspace
#'
#' Builds a Jupyter notebook that runs with the R kernel of OpenHEXA and saves it to the workspace files, ready to
#' be opened from the "Notebooks" page. Inside OpenHEXA, it is written to the mounted workspace directly;
#' elsewhere, it is uploaded.
#'
#' @param path The path of the notebook in the workspace, e.g. `"analysis/malaria.ipynb"`.
#' @param cells A character vector, one element per cell. Cells are code cells, unless named `"markdown"`:
#'   `c(markdown = "# Malaria cases", "cases <- get_file('data/cases.csv')", "summary(cases)")`.
#' @param overwrite Whether to replace an existing notebook at `path`. Only checked inside OpenHEXA: uploads always
#'   replace existing files.
#' @inheritParams get_workspace
#' @return `path`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' create_notebook("malaria.ipynb", c(
#'   markdown = "# Malaria cases",
#'   "library(openhexa)",
#'   "cases <- run_saved_query('monthly-malaria-cases')",
#'   "head(cases)"
#' ))
#' }
create_notebook <- function(path, cells = character(), overwrite = FALSE, workspace = NULL, client = NULL) {
  if (!is.character(cells)) {
    cli::cli_abort("{.arg cells} must be a character vector.", class = c("openhexa_input_error", "openhexa_error"))
  }
  if (tolower(tools::file_ext(path)) != "ipynb") path <- paste0(path, ".ipynb")
  json <- jsonlite::toJSON(notebook(cells), auto_unbox = TRUE, pretty = TRUE, null = "null")

  if (dir.exists(workspace_path())) {
    local <- workspace_path(path)
    check_dest(local, overwrite)
    writeLines(json, local, useBytes = TRUE)
  } else {
    file <- file.path(tempfile("notebook"), basename(path))
    dir.create(dirname(file))
    on.exit(unlink(dirname(file), recursive = TRUE), add = TRUE)
    writeLines(json, file, useBytes = TRUE)
    upload_file(file, path, workspace = workspace, client = client)
  }
  invisible(path)
}

# A notebook in the nbformat 4 format, for the IRkernel ("ir") kernel installed in OpenHEXA images.
notebook <- function(cells) {
  types <- names(cells) %||% rep("", length(cells))
  list(
    cells = lapply(seq_along(cells), function(i) notebook_cell(cells[[i]], types[[i]])),
    metadata = list(
      kernelspec = list(display_name = "R", language = "R", name = "ir"),
      language_info = list(name = "R", file_extension = ".r", mimetype = "text/x-r-source")
    ),
    nbformat = 4L,
    nbformat_minor = 5L
  )
}

notebook_cell <- function(source, type) {
  # Jupyter stores the source as a list of lines, each but the last ending with a newline.
  lines <- strsplit(source, "\n", fixed = TRUE)[[1]]
  lines <- I(paste0(lines, c(rep("\n", max(length(lines) - 1, 0)), rep("", min(length(lines), 1)))))
  id <- paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
  if (identical(type, "markdown")) {
    list(cell_type = "markdown", id = id, metadata = structure(list(), names = character()), source = lines)
  } else {
    list(
      cell_type = "code", id = id, execution_count = NULL, metadata = structure(list(), names = character()),
      outputs = I(list()), source = lines
    )
  }
}
