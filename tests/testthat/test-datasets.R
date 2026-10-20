dataset_link <- function(...) list(datasetLinkBySlug = list(id = "link", dataset = test_dataset(...)))

test_that("list_datasets() lists datasets", {
  sent <- server$answer(Datasets = list(workspace = list(datasets = test_page(
    list(id = "link", isPinned = TRUE, dataset = test_dataset())
  ))))
  out <- list_datasets()
  expect_equal(out$slug, "population")
  expect_equal(out$latest_version, "2024")
  expect_equal(out$source_workspace, "test-workspace")
  expect_equal(sent$calls[[1]]$variables$workspaceSlug, "test-workspace")
})

test_that("get_dataset() returns the dataset", {
  server$answer(DatasetLinkBySlug = dataset_link())
  ds <- get_dataset("population")
  expect_s3_class(ds, "openhexa_dataset")
  expect_equal(ds$latest_version$name, "2024")
  expect_identical(get_dataset(ds), ds)
  expect_no_error(cli::cli_fmt(print(ds)))
})

test_that("missing datasets are reported with a hint", {
  server$answer(DatasetLinkBySlug = list(datasetLinkBySlug = NULL))
  expect_error(get_dataset("nope"), "list_datasets", class = "openhexa_not_found_error")
})

test_that("list_dataset_files() lists the files of the latest or a named version", {
  server$answer(
    DatasetLinkBySlug = dataset_link(),
    DatasetVersions = list(dataset = list(versions = test_page(
      c(test_version(id = "v2", name = "2024"), list(createdBy = NULL)),
      c(test_version(id = "v1", name = "2023", changelog = "First"), list(createdBy = NULL))
    ))),
    DatasetVersionFiles = function(vars) {
      list(datasetVersion = list(files = test_page(test_dataset_file(paste0(vars$versionId, ".csv")))))
    }
  )
  expect_equal(list_dataset_files("population")$filename, "v2.csv")
  expect_equal(list_dataset_files("population", version = "2023")$filename, "v1.csv")
  expect_error(list_dataset_files("population", version = "1999"), class = "openhexa_not_found_error")
})

test_that("a dataset without versions is reported", {
  server$answer(DatasetLinkBySlug = dataset_link(latestVersion = NULL))
  expect_error(list_dataset_files("population"), "no versions", class = "openhexa_not_found_error")
})

test_that("get_dataset_file() reads a file of the latest version", {
  sent <- server$answer(
    DatasetLinkBySlug = dataset_link(),
    DatasetVersionFileByName = list(datasetVersion = list(fileByName = test_dataset_file())),
    PrepareVersionFileDownload = list(prepareVersionFileDownload = test_success(downloadUrl = "https://storage.test/signed"))
  )
  server$serve("https://storage.test/signed", c("district,population", "Kadiogo,3000000"))

  out <- get_dataset_file("population", "population.csv")
  expect_equal(out$district, "Kadiogo")
  ops <- vapply(sent$calls, function(call) call$op, character(1))
  expect_equal(ops, c("DatasetLinkBySlug", "DatasetVersionFileByName", "PrepareVersionFileDownload"))
  expect_equal(sent$calls[[3]]$variables$input$fileId, "f1")
})

test_that("add_dataset_file() uploads to the signed URL then registers the file", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b", path)
  sent <- server$answer(
    DatasetLinkBySlug = dataset_link(),
    GenerateDatasetUploadUrl = list(generateDatasetUploadUrl = test_success(
      uploadUrl = "https://storage.test/upload",
      headers = list(`Content-Type` = "text/csv", `x-ms-blob-type` = "BlockBlob")
    )),
    CreateDatasetVersionFile = list(createDatasetVersionFile = test_success(file = test_dataset_file("data.csv")))
  )

  out <- add_dataset_file("population", path, "data.csv")
  expect_equal(out$filename, "data.csv")
  expect_equal(sent$uploads[[1]]$method, "PUT")
  expect_equal(sent$uploads[[1]]$headers$`x-ms-blob-type`, "BlockBlob")
  expect_equal(sent$calls[[2]]$variables$input, list(versionId = "v2", contentType = "text/csv", uri = "data.csv"))
})

test_that("upload errors are explained", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b", path)
  server$answer(
    DatasetLinkBySlug = dataset_link(),
    GenerateDatasetUploadUrl = list(generateDatasetUploadUrl = test_failure("ALREADY_EXISTS"))
  )
  expect_error(add_dataset_file("population", path, "data.csv"), "already exists", class = "openhexa_mutation_error")
  expect_error(add_dataset_file("population", "missing.csv"), class = "openhexa_file_not_found_error")
})

test_that("create_dataset_version() creates a version", {
  sent <- server$answer(
    DatasetLinkBySlug = dataset_link(),
    CreateDatasetVersion = list(createDatasetVersion = test_success(version = test_version(id = "v3", name = "2025")))
  )
  out <- create_dataset_version("population", 2025)
  expect_equal(out$name, "2025")
  expect_equal(sent$calls[[2]]$variables$input, list(datasetId = "ds1", name = "2025"))
})

test_that("get_dataset_data() stacks the tables of a version", {
  server$answer(
    DatasetLinkBySlug = dataset_link(),
    DatasetVersionFiles = list(datasetVersion = list(files = test_page(
      test_dataset_file("cases_a.csv"), test_dataset_file("cases_b.csv"), test_dataset_file("readme.md"),
      test_dataset_file("other.csv")
    )))
  )
  local_mocked_bindings(get_dataset_file = function(dataset, filename, ...) {
    if (filename == "cases_a.csv") data.frame(x = 1:2) else data.frame(x = 3L, y = "b")
  })
  out <- get_dataset_data("population", pattern = "^cases_")
  expect_equal(names(out), c("x", "y", "file"))
  expect_equal(out$x, 1:3)
  expect_equal(out$y, c(NA, NA, "b"))
  expect_equal(out$file, c("cases_a.csv", "cases_a.csv", "cases_b.csv"))

  expect_error(get_dataset_data("population", pattern = "nope"), class = "openhexa_not_found_error")
})
