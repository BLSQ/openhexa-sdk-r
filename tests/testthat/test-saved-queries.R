query_result <- function(columns, rows, truncated = FALSE) {
  list(executeSavedQuery = test_success(
    errorMessage = NULL, columns = as.list(columns), rows = rows, rowCount = length(rows), truncated = truncated
  ))
}

test_that("list_saved_queries() lists saved queries", {
  server$answer(SavedQueries = list(workspace = list(savedQueries = test_page(list(
    id = "q1", slug = "cases", name = "Cases", description = NULL, visibility = "WORKSPACE",
    createdAt = "2024-01-01T00:00:00Z", updatedAt = "2024-01-02T00:00:00Z", createdBy = list(displayName = "Ada")
  )))))
  out <- list_saved_queries()
  expect_equal(out$slug, "cases")
  expect_equal(out$visibility, "workspace")
  expect_equal(out$created_by, "Ada")
})

test_that("run_saved_query() returns a tibble", {
  sent <- server$answer(ExecuteSavedQuery = query_result(
    c("district", "cases", "note"),
    list(list(district = "A", cases = 1, note = NULL), list(district = "B", cases = 2.5, note = NULL))
  ))
  out <- run_saved_query("cases", max_rows = 100)
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("district", "cases", "note"))
  expect_equal(out$cases, c(1, 2.5))
  expect_equal(out$note, c(NA, NA))
  expect_equal(sent$calls[[1]]$variables$input, list(slug = "cases", maxRows = 100L))
})

test_that("truncated results raise a warning", {
  server$answer(ExecuteSavedQuery = query_result("a", list(list(a = 1)), truncated = TRUE))
  expect_warning(run_saved_query("cases"), "truncated")
})

test_that("query errors are explained", {
  server$answer(ExecuteSavedQuery = list(executeSavedQuery = list(
    success = FALSE, errors = list("QUERY_ERROR"), errorMessage = "relation \"x\" does not exist"
  )))
  expect_error(run_saved_query("cases"), "does not exist", class = "openhexa_mutation_error")
})

test_that("non-scalar values are kept as list columns", {
  out <- query_rows_to_tibble("tags", list(list(tags = list("a", "b")), list(tags = NULL)))
  expect_type(out$tags, "list")
})
