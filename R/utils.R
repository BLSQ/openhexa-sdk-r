# Fetch every page of a paginated field.
#
# `fetch(page, per_page)` must return the page object (with `items` and either `totalPages` or `hasNextPage`),
# or NULL if the parent object does not exist.
paginate <- function(fetch, per_page = 50L, max_items = Inf) {
  items <- list()
  page <- 1L
  repeat {
    result <- fetch(page = page, per_page = per_page)
    if (is.null(result)) {
      return(NULL)
    }
    items <- c(items, result$items)
    has_next <- if (!is.null(result$hasNextPage)) isTRUE(result$hasNextPage) else page < (result$totalPages %||% 0L)
    if (!has_next || length(items) >= max_items) break
    page <- page + 1L
  }
  if (is.finite(max_items)) utils::head(items, max_items) else items
}

# Column specifications for records_to_tibble(): the path to the value in each record, and its type.
col_chr <- function(...) list(path = list(...), type = "chr")
col_int <- function(...) list(path = list(...), type = "int")
col_dbl <- function(...) list(path = list(...), type = "dbl")
col_lgl <- function(...) list(path = list(...), type = "lgl")
col_dttm <- function(...) list(path = list(...), type = "dttm")
col_list <- function(...) list(path = list(...), type = "list")

# Turn a list of records (as returned by the API) into a tibble, one column per spec.
records_to_tibble <- function(records, spec) {
  columns <- lapply(spec, function(col) {
    values <- lapply(records, pluck, col$path)
    switch(col$type,
      chr = vapply(values, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1)),
      int = vapply(values, function(v) if (is.null(v)) NA_integer_ else as.integer(v), integer(1)),
      dbl = vapply(values, function(v) if (is.null(v)) NA_real_ else as.double(v), double(1)),
      lgl = vapply(values, function(v) if (is.null(v)) NA else as.logical(v), logical(1)),
      dttm = parse_datetime(vapply(values, function(v) if (is.null(v)) NA_character_ else v, character(1))),
      list = values
    )
  })
  tibble::new_tibble(columns, nrow = length(records))
}

pluck <- function(x, path) {
  for (key in path) {
    if (is.null(x) || (is.numeric(key) && key > length(x))) {
      return(NULL)
    }
    x <- x[[key]]
  }
  x
}

# Parse ISO 8601 timestamps as returned by the API (e.g. "2024-01-15T10:30:45.123456+00:00") into UTC.
parse_datetime <- function(x) {
  out <- as.POSIXct(substr(x, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  offsets <- regmatches(x, regexec("([+-])(\\d{2}):?(\\d{2})$", x))
  seconds <- vapply(offsets, function(m) {
    if (length(m) != 4) {
      return(0)
    }
    (if (m[2] == "-") -1 else 1) * (as.numeric(m[3]) * 3600 + as.numeric(m[4]) * 60)
  }, double(1))
  out - seconds
}

# Read a local file into R, picking a reader from the file extension.
read_by_extension <- function(path, reader = NULL, ...) {
  if (!is.null(reader)) {
    return(reader(path, ...))
  }
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    csv = read_delimited(path, ",", ...),
    tsv = read_delimited(path, "\t", ...),
    parquet = {
      rlang::check_installed("arrow", "to read Parquet files.")
      arrow::read_parquet(path, ...)
    },
    xlsx = ,
    xls = {
      rlang::check_installed("readxl", "to read Excel files.")
      readxl::read_excel(path, ...)
    },
    json = jsonlite::read_json(path, simplifyVector = TRUE, ...),
    rds = readRDS(path),
    cli::cli_abort(
      c(
        "Don't know how to read {.file {basename(path)}}.",
        i = "Pass a function as {.arg reader}, e.g. {.code reader = readLines}."
      ),
      class = c("openhexa_reader_error", "openhexa_error")
    )
  )
}

read_delimited <- function(path, delim, ...) {
  if (rlang::is_installed("readr")) {
    readr::read_delim(path, delim = delim, show_col_types = FALSE, ...)
  } else {
    tibble::as_tibble(utils::read.delim(path, sep = delim, ...))
  }
}

guess_content_type <- function(path) {
  types <- c(
    csv = "text/csv", tsv = "text/tab-separated-values", txt = "text/plain", json = "application/json",
    parquet = "application/vnd.apache.parquet", xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    xls = "application/vnd.ms-excel", zip = "application/zip", gz = "application/gzip", pdf = "application/pdf",
    png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg", geojson = "application/geo+json",
    gpkg = "application/geopackage+sqlite3", html = "text/html", md = "text/markdown"
  )
  unname(types[tolower(tools::file_ext(path))]) %|NA|% "application/octet-stream"
}

`%|NA|%` <- function(x, y) if (is.na(x)) y else x

# Signed URLs (dataset files, workspace files) are fetched without the API token.
download_signed_url <- function(url, dest) {
  httr2::request(url) |>
    with_ssl_options() |>
    httr2::req_perform(path = dest)
  invisible(dest)
}

upload_signed_url <- function(url, path, content_type, headers = NULL) {
  headers <- headers %||% list()
  content_type <- headers[["Content-Type"]] %||% content_type
  headers[["Content-Type"]] <- NULL
  httr2::request(url) |>
    httr2::req_method("PUT") |>
    httr2::req_body_file(path, type = content_type) |>
    httr2::req_headers(!!!headers) |>
    with_ssl_options() |>
    httr2::req_perform()
  invisible(NULL)
}

check_dest <- function(dest, overwrite) {
  if (file.exists(dest) && !overwrite) {
    cli::cli_abort(
      c("{.file {dest}} already exists.", i = "Use {.code overwrite = TRUE} to replace it."),
      class = c("openhexa_file_exists_error", "openhexa_error")
    )
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
}
