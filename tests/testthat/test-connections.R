test_that("connections are read from environment variables", {
  provide_connection("my-dhis2", "dhis2", url = "https://dhis2.example", username = "admin", password = "district")
  con <- get_dhis2_connection("my-dhis2")
  expect_s3_class(con, "openhexa_dhis2_connection")
  expect_equal(con$url, "https://dhis2.example")
  expect_equal(con$password, "district")
})

test_that("PostgreSQL and S3 fields use the same names as the Python SDK", {
  provide_connection("db", "POSTGRESQL", host = "localhost", port = "5433", username = "u", password = "p", db_name = "hexa")
  provide_connection("bucket", "S3", access_key_id = "id", access_key_secret = "secret", bucket_name = "b")
  db <- get_postgresql_connection("db")
  expect_equal(db$database_name, "hexa")
  expect_identical(db$port, 5433L)

  s3 <- get_s3_connection("bucket")
  expect_equal(s3$secret_access_key, "secret")
  output <- cli::cli_fmt(print(s3))
  expect_false(any(grepl("secret$", output)))
})

test_that("custom connections expose all their fields, hidden when printed", {
  provide_connection("api", "CUSTOM", token = "tok123", base_url = "https://x.example")
  con <- get_custom_connection("api")
  expect_equal(con$token, "tok123")
  expect_equal(con$base_url, "https://x.example")
  expect_false(any(grepl("tok123", cli::cli_fmt(print(con)))))
})

test_that("outside a notebook, connections are fetched from the API and their type is checked", {
  sent <- server$answer(ConnectionBySlug = list(connectionBySlug = list(
    id = "1", slug = "iaso", name = "IASO", type = "IASO",
    fields = list(
      list(code = "url", value = "https://iaso.example", secret = FALSE),
      list(code = "username", value = "me", secret = FALSE),
      list(code = "password", value = "pw", secret = TRUE)
    )
  )))
  con <- get_iaso_connection("IASO")
  expect_equal(con$url, "https://iaso.example")
  expect_equal(sent$calls[[1]]$variables$connectionSlug, "iaso")
  expect_error(get_dhis2_connection("IASO"), class = "openhexa_connection_type_error")
})

test_that("unknown connections are reported", {
  provide_connection("nope", "not a connection type")
  server$answer(ConnectionBySlug = list(connectionBySlug = NULL))
  expect_error(get_connection("nope"), class = "openhexa_connection_not_found_error")
})

test_that("workspace database credentials come from the environment or the API", {
  provide_database(host = "db", port = "5432", username = "u", password = "p", db_name = "ws")
  expect_equal(get_database_credentials()$database_name, "ws")

  provide_database()
  server$answer(WorkspaceDatabaseCredentials = list(workspace = list(database = list(credentials = list(
    host = "remote", port = 5432, dbName = "ws2", username = "u", password = "p"
  )))))
  db <- get_database_credentials()
  expect_equal(db$host, "remote")
  expect_s3_class(db, "openhexa_postgresql_connection")
})

test_that("list_connections() lists connections without their values", {
  server$answer(Connections = list(workspace = list(connections = list(
    list(id = "c1", slug = "dhis2-play", name = "DHIS2 play", description = NULL, type = "DHIS2",
         createdAt = "2024-01-01T00:00:00Z", updatedAt = NULL,
         fields = list(list(code = "url", secret = FALSE), list(code = "password", secret = TRUE)))
  ))))
  out <- list_connections()
  expect_equal(out$identifier, "dhis2-play")
  expect_equal(out$fields, list(c("url", "password")))
})
