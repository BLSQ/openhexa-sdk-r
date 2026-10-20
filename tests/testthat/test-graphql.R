test_that("graphql_query() returns the data and drops NULL variables", {
  sent <- NULL
  httr2::local_mocked_responses(function(req) {
    sent <<- req
    httr2::response_json(body = list(data = list(answer = 42)))
  })
  data <- graphql_query("query Q($a: Int, $b: Int) { answer }", list(a = 1, b = NULL))
  expect_equal(data, list(answer = 42))
  expect_equal(sent$url, paste0(test_server_url, "/graphql/"))
  expect_equal(sent$body$data$variables, list(a = 1))
  expect_equal(httr2::req_get_headers(sent, redacted = "reveal")$Authorization, "Bearer test-token")
})

test_that("GraphQL errors are raised", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(body = list(data = NULL, errors = list(list(message = "Cannot query field \"nope\""))))
  })
  expect_error(graphql_query("{ nope }"), class = "openhexa_graphql_error")
  expect_error(graphql_query("{ nope }"), "Cannot query field")
})

test_that("empty variables are sent as a JSON object", {
  expect_equal(as.character(jsonlite::toJSON(as_json_object(list()), auto_unbox = TRUE)), "{}")
  expect_equal(as.character(jsonlite::toJSON(as_json_object(list(a = 1)), auto_unbox = TRUE)), "{\"a\":1}")
})

test_that("failed mutations raise errors with friendly messages", {
  expect_invisible(check_mutation(list(success = TRUE, errors = list())))
  result <- list(success = FALSE, errors = list("DUPLICATE_NAME"))
  expect_error(check_mutation(result, c(DUPLICATE_NAME = "Already taken.")), "Already taken.", class = "openhexa_mutation_error")
  expect_error(check_mutation(result), "refused", class = "openhexa_mutation_error")
})

test_that("every shipped GraphQL file holds one operation named after the file", {
  files <- list.files(system.file("graphql", package = "openhexa"), full.names = TRUE)
  expect_gt(length(files), 0)
  for (file in files) {
    name <- tools::file_path_sans_ext(basename(file))
    query <- operation(name)
    expect_match(query, paste0("^(query|mutation) ", name, "\\b"), info = file)
  }
})
