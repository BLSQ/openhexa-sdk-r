bucket_object <- function(key, type = "FILE") {
  list(key = key, name = basename(key), path = paste0("bucket/", key), type = type, size = 10, updatedAt = NULL)
}

test_that("list_files() lists files across pages", {
  sent <- server$answer(BucketObjects = function(vars) {
    items <- if (vars$page == 1) list(bucket_object("data/", "DIRECTORY")) else list(bucket_object("a.csv"))
    list(workspace = list(bucket = list(objects = list(hasNextPage = vars$page == 1, items = items))))
  })
  out <- list_files()
  expect_equal(out$path, c("data/", "a.csv"))
  expect_equal(out$type, c("directory", "file"))
  expect_length(sent$calls, 2)
})

test_that("get_file() reads mounted files directly", {
  dir <- withr::local_tempdir()
  provide_files(dir)
  dir.create(file.path(dir, "data"))
  writeLines(c("a", "1"), file.path(dir, "data", "x.csv"))
  expect_equal(get_file("data/x.csv")$a, 1)
})

test_that("get_file() downloads remote files", {
  provide_files(withr::local_tempdir())
  sent <- server$answer(PrepareObjectDownload = list(prepareObjectDownload = test_success(
    downloadUrl = "https://storage.test/x"
  )))
  server$serve("https://storage.test/x", c("a", "2"))
  expect_equal(get_file("data/x.csv")$a, 2)
  expect_equal(sent$calls[[1]]$variables$input, list(workspaceSlug = "test-workspace", objectKey = "data/x.csv"))
})

test_that("missing files are reported", {
  server$answer(PrepareObjectDownload = list(prepareObjectDownload = test_failure("NOT_FOUND")))
  dest <- file.path(withr::local_tempdir(), "x.csv")
  expect_error(download_file("x.csv", dest), "does not exist", class = "openhexa_mutation_error")
})

test_that("existing destinations are not overwritten by default", {
  dest <- withr::local_tempfile()
  writeLines("", dest)
  expect_error(download_file("x.csv", dest), class = "openhexa_file_exists_error")
})

test_that("upload_file() uploads to the signed URL", {
  file <- withr::local_tempfile(fileext = ".csv")
  writeLines("a", file)
  sent <- server$answer(PrepareObjectUpload = list(prepareObjectUpload = test_success(
    uploadUrl = "https://storage.test/up", headers = NULL
  )))
  expect_equal(upload_file(file, "data/a.csv"), "data/a.csv")
  expect_equal(sent$uploads[[1]]$url, "https://storage.test/up")
  expect_equal(sent$calls[[1]]$variables$input$contentType, "text/csv")
})
