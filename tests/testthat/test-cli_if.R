test_that("cli_if works", {
  expect_message(
    cli_if(TRUE, text = "Message")
  )
  expect_message(
    cli_ifnot(FALSE, text = "Message")
  )
})

test_that("cli_if errors", {
  expect_error(
    cli_if(TRUE)
  )
  expect_error(
    cli_ifnot(FALSE)
  )
})

test_that("cli_if does nothing when the predicate is not satisfied", {
  expect_no_message(
    cli_if(FALSE, "no show")
  )
  expect_null(cli_if(FALSE, "no show"))

  expect_no_message(
    cli_ifnot(TRUE, "no show")
  )
})

test_that("cli_if uses .fn instead of the default", {
  expect_message(
    cli_if(TRUE, "custom text", .fn = cli::cli_alert_success),
    "custom text"
  )
})

test_that("cli_if uses .default when .fn is NULL", {
  expect_message(
    cli_if(TRUE, "default text", .default = cli::cli_alert_info),
    "default text"
  )
})

test_that("cli_if aborts when the predicate does not return a boolean", {
  expect_error(
    cli_if(1, "msg", .predicate = function(x) "not boolean")
  )
  expect_error(
    cli_if(1, "msg", .predicate = function(x) NA)
  )
})

test_that("cli_if aborts when the predicate errors", {
  expect_error(
    cli_if(1, "msg", .predicate = function(x) stop("boom"))
  )
})

test_that("cli_ifnot defaults to rlang::is_false as the predicate", {
  expect_message(
    cli_ifnot(FALSE, "shown on FALSE"),
    "shown on FALSE"
  )
  expect_no_message(
    cli_ifnot(TRUE, "not shown on TRUE")
  )
})
