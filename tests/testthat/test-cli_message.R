test_that("cli_abort_ifnot works", {
  expect_error(
    cli_abort_ifnot(
      "Pass on TRUE" = TRUE,
      "Error on FALSE" = FALSE
    ),
    "Error on FALSE"
  )
  expect_error(
    cli_abort_ifnot(
      "Error on FALSE" = FALSE
    ),
    "Error on FALSE"
  )
  expect_error(
    cli_abort_ifnot(
      list("Error on FALSE" = FALSE)
    ),
    "Error on FALSE"
  )
  expect_error(
    cli_abort_ifnot(
      list(
        "Pass on TRUE" = TRUE,
        "Error on FALSE" = FALSE
      )
    ),
    "Error on FALSE"
  )
  expect_error(
    cli_abort_ifnot(
      message = "Error on FALSE",
      condition = FALSE
    ),
    "Error on FALSE"
  )
  expect_error(
    cli_abort_if(
      message = "Error on TRUE",
      condition = TRUE
    ),
    "Error on TRUE"
  )
  expect_no_warning(
    cli_warn_ifnot(
      message = "Warn on FALSE",
      condition = TRUE
    )
  )
  expect_warning(
    cli_warn_ifnot(
      message = "Warn on FALSE",
      condition = FALSE
    ),
    "Warn on FALSE"
  )
  expect_warning(
    cli_warn_if(
      message = "Warn on TRUE",
      condition = TRUE
    ),
    "Warn on TRUE"
  )
  expect_no_message(
    cli_inform_ifnot(
      message = "Inform on FALSE",
      condition = TRUE
    )
  )
  expect_message(
    cli_inform_ifnot(
      message = "Inform on FALSE",
      condition = FALSE
    ),
    "Inform on FALSE"
  )
  expect_message(
    cli_inform_if(
      "Inform on TRUE",
      condition = TRUE
    ),
    "Inform on TRUE"
  )
  # FIXME: Correct escaping for warning to work as expected
  # expect_warning(
  #   cli_warn_ifnot(is.numeric("A")),
  #   '`is.numeric(\"A\")` must be \`TRUE\`'
  # )
})
