test_that("timestamps are parsed to UTC", {
  parsed <- parse_datetime(c("2024-01-15T10:30:45.123456+00:00", "2024-01-15T12:30:45+02:00", "2024-01-15T10:30:45Z", NA))
  expected <- as.POSIXct("2024-01-15 10:30:45", tz = "UTC")
  expect_equal(parsed[1:3], rep(expected, 3))
  expect_true(is.na(parsed[4]))
})

test_that("records are turned into tibbles", {
  records <- list(
    list(id = "1", n = 2L, ok = TRUE, at = "2024-01-15T10:30:45Z", nested = list(name = "a"), runs = list(list(s = "x"))),
    list(id = "2", n = NULL, ok = FALSE, at = NULL, nested = NULL, runs = list())
  )
  out <- records_to_tibble(records, list(
    id = col_chr("id"), n = col_int("n"), ok = col_lgl("ok"), at = col_dttm("at"),
    name = col_chr("nested", "name"), last = col_chr("runs", 1, "s"), raw = col_list("nested")
  ))
  expect_s3_class(out, "tbl_df")
  expect_equal(out$id, c("1", "2"))
  expect_equal(out$n, c(2L, NA))
  expect_equal(out$name, c("a", NA))
  expect_equal(out$last, c("x", NA))
  expect_s3_class(out$at, "POSIXct")
  expect_type(out$raw, "list")

  empty <- records_to_tibble(list(), list(id = col_chr("id")))
  expect_equal(nrow(empty), 0)
  expect_named(empty, "id")
})

test_that("pagination follows totalPages and hasNextPage", {
  pages <- list(list(items = list(1, 2), totalPages = 2), list(items = list(3), totalPages = 2))
  expect_equal(paginate(function(page, per_page) pages[[page]]), list(1, 2, 3))

  pages <- list(list(items = list(1), hasNextPage = TRUE), list(items = list(2), hasNextPage = FALSE))
  expect_equal(paginate(function(page, per_page) pages[[page]]), list(1, 2))

  infinite <- function(page, per_page) list(items = as.list(seq_len(per_page)), hasNextPage = TRUE)
  expect_length(paginate(infinite, per_page = 2, max_items = 5), 5)

  expect_null(paginate(function(page, per_page) NULL))
})

test_that("files are read according to their extension", {
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,x"), csv)
  expect_equal(read_by_extension(csv)$a, 1)

  json <- withr::local_tempfile(fileext = ".json")
  writeLines('{"a": [1, 2]}', json)
  expect_equal(read_by_extension(json)$a, c(1, 2))

  expect_equal(read_by_extension(csv, readLines), c("a,b", "1,x"))

  other <- withr::local_tempfile(fileext = ".xyz")
  writeLines("", other)
  expect_error(read_by_extension(other), class = "openhexa_reader_error")
})

test_that("content types are guessed from file names", {
  expect_equal(guess_content_type("data.CSV"), "text/csv")
  expect_equal(guess_content_type("data.parquet"), "application/vnd.apache.parquet")
  expect_equal(guess_content_type("data.unknown"), "application/octet-stream")
  expect_equal(guess_content_type("Makefile"), "application/octet-stream")
})
