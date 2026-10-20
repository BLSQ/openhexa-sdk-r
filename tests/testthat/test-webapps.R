webapps_page <- function(names = character()) {
  items <- lapply(names, function(name) {
    test_webapp(name = name)[c("id", "slug", "name", "description", "url", "type", "isPublic")]
  })
  list(webapps = list(totalPages = 1, items = items))
}

test_that("list_webapps() lists web apps", {
  sent <- server$answer(Webapps = webapps_page("Dashboard"))
  out <- list_webapps()
  expect_equal(out$slug, "dashboard")
  expect_equal(out$type, "static")
  expect_equal(sent$calls[[1]]$variables$workspaceSlug, "test-workspace")
})

test_that("get_webapp() describes a web app", {
  server$answer(Webapp = list(webapp = test_webapp(files = list(
    list(path = "index.html", type = "file"), list(path = "lib", type = "directory"), list(path = "lib/a.js", type = "file")
  ))))
  webapp <- get_webapp("dashboard")
  expect_equal(webapp$files, c("index.html", "lib/a.js"))
  expect_equal(webapp$allowed_operations, "datasets_read")
  expect_no_error(cli::cli_fmt(print(webapp)))
})

test_that("get_webapp() reports missing web apps", {
  server$answer(Webapp = list(webapp = NULL))
  expect_error(get_webapp("nope"), class = "openhexa_not_found_error")
})

test_that("publish_webapp() creates new web apps from HTML", {
  sent <- server$answer(
    Webapps = webapps_page(),
    CreateWebapp = list(createWebapp = test_success(webapp = list(slug = "dashboard"))),
    Webapp = list(webapp = test_webapp())
  )
  expect_message(app <- publish_webapp("Dashboard", "<h1>Hi</h1>", allowed_operations = "datasets_read"), "Created")
  expect_s3_class(app, "openhexa_webapp")
  input <- sent$calls[[2]]$variables$input
  expect_equal(input$name, "Dashboard")
  expect_equal(input$workspaceSlug, "test-workspace")
  expect_equal(input$allowedOperations, list("DATASETS_READ"))
  expect_equal(input$source$static, list(list(path = "index.html", content = "<h1>Hi</h1>", encoding = "TEXT")))
  expect_null(input$isPublic)
})

test_that("publish_webapp() replaces the content of existing web apps", {
  dir <- withr::local_tempdir()
  writeLines("<h1>Hi</h1>", file.path(dir, "index.html"))
  writeBin(as.raw(1:3), file.path(dir, "logo.png"))
  sent <- server$answer(
    Webapps = webapps_page("Dashboard"),
    Webapp = list(webapp = test_webapp(files = list(list(path = "index.html", type = "file"), list(path = "old.css", type = "file")))),
    UpdateWebapp = list(updateWebapp = test_success(webapp = list(slug = "dashboard")))
  )
  expect_message(publish_webapp("Dashboard", dir, message = "New logo"), "Updated")
  input <- sent$calls[[3]]$variables$input
  expect_equal(input$id, "w1")
  expect_equal(input$filesToDelete, list("old.css"))
  expect_equal(input$commitMessage, "New logo")
  expect_equal(vapply(input$files, function(f) f$encoding, character(1)), c("TEXT", "BASE64"))
  expect_equal(input$files[[2]]$content, jsonlite::base64_enc(as.raw(1:3)))
})

test_that("publish_webapp() validates the content", {
  expect_error(publish_webapp("x", "not html"), class = "openhexa_input_error")
  expect_error(publish_webapp("x", 1), class = "openhexa_input_error")
  expect_error(publish_webapp("x", withr::local_tempdir()), "index.html", class = "openhexa_input_error")
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines("a", csv)
  expect_error(publish_webapp("x", csv), "not an HTML file", class = "openhexa_input_error")
})

test_that("htmltools objects are saved with their dependencies", {
  skip_if_not_installed("htmltools")
  files <- webapp_files(htmltools::tags$h1("Hi"))
  paths <- vapply(files, function(f) f$path, character(1))
  expect_true("index.html" %in% paths)
  expect_match(files[[which(paths == "index.html")]]$content, "<h1>Hi</h1>")
})
