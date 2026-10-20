test_that("list_workspaces() lists workspaces", {
  server$answer(Workspaces = list(workspaces = test_page(test_workspace(slug = "a", description = NULL))))
  out <- list_workspaces()
  expect_equal(out$slug, "a")
  expect_true(is.na(out$description))
})

test_that("get_workspace() returns the workspace with its configuration and countries", {
  sent <- server$answer(Workspace = list(workspace = test_workspace(
    configuration = list(threshold = "3"),
    countries = list(list(code = "BF", alpha3 = "BFA", name = "Burkina Faso"))
  )))
  ws <- get_workspace()
  expect_s3_class(ws, "openhexa_workspace")
  expect_equal(sent$calls[[1]]$variables$slug, "test-workspace")
  expect_equal(ws$countries$alpha3, "BFA")
  expect_equal(ws$configuration$threshold, "3")
  expect_no_error(cli::cli_fmt(print(ws)))
})

test_that("a missing workspace is reported", {
  server$answer(Workspace = list(workspace = NULL))
  expect_error(get_workspace("nope"), class = "openhexa_not_found_error")
})

test_that("workspace paths honour the environment", {
  provide_files()
  expect_equal(workspace_path("a", "b.csv"), "/home/hexa/workspace/a/b.csv")
  expect_equal(workspace_tmp_path("x"), "/home/hexa/tmp/x")
  provide_files("/data")
  expect_equal(workspace_path("b.csv"), "/data/b.csv")
})

test_that("set_workspace() selects the default workspace", {
  provide_credentials()
  server$answer(Workspace = list(workspace = test_workspace(slug = "other", name = "Other")))
  expect_message(set_workspace("other"), "Other")
  expect_equal(resolve_workspace(), "other")
})
