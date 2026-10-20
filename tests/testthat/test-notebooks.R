test_that("create_notebook() writes an R notebook to the mounted workspace", {
  dir <- withr::local_tempdir()
  provide_files(dir)
  expect_equal(create_notebook("analysis", c(markdown = "# Title", "x <- 1\nx")), "analysis.ipynb")

  nb <- jsonlite::read_json(file.path(dir, "analysis.ipynb"))
  expect_equal(nb$metadata$kernelspec$name, "ir")
  expect_equal(nb$nbformat, 4)
  expect_equal(vapply(nb$cells, function(c) c$cell_type, character(1)), c("markdown", "code"))
  expect_equal(nb$cells[[2]]$source, list("x <- 1\n", "x"))
  expect_null(nb$cells[[2]]$execution_count)
  expect_equal(nb$cells[[2]]$outputs, list())

  expect_error(create_notebook("analysis.ipynb"), class = "openhexa_file_exists_error")
})

test_that("create_notebook() uploads notebooks outside OpenHEXA", {
  uploaded <- NULL
  local_mocked_bindings(upload_file = function(file, path, ...) {
    uploaded <<- list(path = path, json = jsonlite::read_json(file))
    invisible(path)
  })
  create_notebook("nb.ipynb", "library(openhexa)")
  expect_equal(uploaded$path, "nb.ipynb")
  expect_equal(uploaded$json$cells[[1]]$source, list("library(openhexa)"))
})
