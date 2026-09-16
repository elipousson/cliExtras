test_that("cli_yesno returns TRUE for a yes response", {
  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")
  local_mocked_bindings(cli_menu = function(...) "Yes")

  expect_true(
    cli_yesno("Continue?", yes = "Yes", no = "No", n_yes = 1, n_no = 1)
  )
})

test_that("cli_yesno returns FALSE for a no response", {
  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")
  local_mocked_bindings(cli_menu = function(...) "No")

  expect_false(
    cli_yesno("Continue?", yes = "Yes", no = "No", n_yes = 1, n_no = 1)
  )
})

test_that("cli_yesno errors when session is not interactive", {
  local_mocked_bindings(is_interactive = function() FALSE, .package = "rlang")

  expect_error(
    cli_yesno("Continue?", n_yes = 1, n_no = 1)
  )
})

test_that("check_yes passes silently on a yes response", {
  local_mocked_bindings(cli_ask = function(...) "Y")

  expect_null(check_yes("Continue?"))
})

test_that("check_yes accepts a default empty response", {
  local_mocked_bindings(cli_ask = function(...) "")

  expect_null(check_yes("Continue?"))
})

test_that("check_yes aborts on a no response", {
  local_mocked_bindings(cli_ask = function(...) "n")

  expect_error(
    check_yes("Continue?"),
    "Aborted"
  )
})

test_that("check_yes uses a custom abort message", {
  local_mocked_bindings(cli_ask = function(...) "no")

  expect_error(
    check_yes("Continue?", message = "Custom abort message"),
    "Custom abort message"
  )
})
