test_that("cli_menu returns the selected list item", {
  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")
  local_mocked_bindings(readline = function(...) "1")

  expect_message(
    resp <- cli_menu(list("A", "B", "C"), title = "Pick a letter?"),
    "Pick a letter?"
  )
  expect_identical(resp, "A")
})

test_that("cli_menu returns the selected vector item", {
  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")
  local_mocked_bindings(readline = function(...) "2")

  resp <- cli_menu(c("A", "B", "C"))
  expect_identical(resp, "B")
})

test_that("cli_menu returns the index when ind is TRUE", {
  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")
  local_mocked_bindings(readline = function(...) "2")

  resp <- cli_menu(c("A", "B", "C"), ind = TRUE)
  expect_identical(resp, "2")
})

test_that("cli_menu returns invisibly on exit", {
  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")
  local_mocked_bindings(readline = function(...) "0")

  expect_null(cli_menu(list("A", "B", "C")))
})

test_that("cli_menu re-prompts on invalid input", {
  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")
  responses <- c("9", "2")
  i <- 0
  local_mocked_bindings(readline = function(...) {
    i <<- i + 1
    responses[i]
  })

  resp <- cli_menu(list("A", "B", "C"))
  expect_identical(resp, "B")
  expect_identical(i, 2)
})

test_that("cli_menu errors for non-vector choices", {
  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")

  expect_error(cli_menu(NULL))
})
