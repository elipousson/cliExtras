test_that("cli_quiet leaves the option unchanged when quiet is FALSE", {
  withr::local_options(list(cli.default_handler = NULL))
  expect_null(cli_quiet(quiet = FALSE))
  expect_null(getOption("cli.default_handler"))
})

test_that("cli_quiet locally sets cli.default_handler when quiet is TRUE", {
  withr::local_options(list(cli.default_handler = NULL))

  f <- function(quiet = FALSE) {
    cli_quiet(quiet = quiet)
    getOption("cli.default_handler")
  }

  expect_null(f(FALSE))
  expect_identical(f(TRUE), suppressMessages)
  # Option is restored after the local scope in f() exits
  expect_null(getOption("cli.default_handler"))
})

test_that("cli_quiet permanently sets the option when push is TRUE", {
  withr::local_options(list(cli.default_handler = NULL))
  withr::defer(options("cli.default_handler" = NULL))

  cli_quiet(quiet = TRUE, push = TRUE)
  expect_identical(getOption("cli.default_handler"), suppressMessages)
})

test_that("set_cli_quiet sets both options and can be silent", {
  withr::local_options(list(cli.default_handler = NULL, cliExtras.quiet = NULL))

  expect_no_message(set_cli_quiet(TRUE, msg = FALSE))
  expect_true(getOption("cliExtras.quiet"))
  expect_identical(getOption("cli.default_handler"), suppressMessages)

  expect_no_message(set_cli_quiet(FALSE, msg = FALSE))
  expect_false(getOption("cliExtras.quiet"))
  expect_null(getOption("cli.default_handler"))
})

test_that("set_cli_quiet displays a message describing the change", {
  withr::local_options(list(cli.default_handler = NULL, cliExtras.quiet = NULL))

  expect_message(
    set_cli_quiet(TRUE, msg = TRUE),
    "muffle"
  )
})

test_that("quiet_cli_inform respects the cliExtras.quiet option", {
  withr::local_options(list(cliExtras.quiet = TRUE))
  expect_no_message(quiet_cli_inform("hello"))

  withr::local_options(list(cliExtras.quiet = FALSE))
  expect_message(quiet_cli_inform("hello"), "hello")
})
