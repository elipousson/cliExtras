test_that("cli_ask reads a line and shows a message", {
  local_mocked_bindings(readline = function(...) "typed answer")
  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")

  expect_message(
    resp <- cli_ask("?", "Enter something"),
    "Enter something"
  )
  expect_identical(resp, "typed answer")
})

test_that("cli_ask reads a line without a message when ... is empty", {
  local_mocked_bindings(readline = function(...) "typed answer")
  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")

  expect_no_message(
    resp <- cli_ask("?")
  )
  expect_identical(resp, "typed answer")
})

test_that("cli_ask errors when session is not interactive", {
  local_mocked_bindings(is_interactive = function() FALSE, .package = "rlang")

  expect_error(
    cli_ask("?")
  )
})
