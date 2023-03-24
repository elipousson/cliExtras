test_that("multiplication works", {
  # cli_abort_if
  expect_null(
    cli_abort_if(
      "Error message" = FALSE
    )
  )
  expect_error(
    cli_abort_if(
      "Error message" = TRUE
    ),
    "Error message"
  )
  expect_error(
    cli_abort_if(
      message = "Error message",
      condition = TRUE
    ),
    "Error message"
  )
  expect_error(
    cli_abort_if(
      "Error message",
      condition = TRUE
    ),
    "Error message"
  )
  expect_null(
    cli_abort_ifnot(
      "Error message" = TRUE
    )
  )
  expect_error(
    cli_abort_ifnot(
      "Error message" = FALSE
    ),
    "Error message"
  )
  expect_error(
    cli_abort_ifnot(
      message = "Error message",
      condition = FALSE
    ),
    "Error message"
  )
  expect_error(
    cli_abort_ifnot(
      message = "Error message",
      condition = FALSE
    ),
    "Error message"
  )
  expect_error(
    cli_abort_ifnot(
      "Error message",
      condition = FALSE
    ),
    "Error message"
  )
  expect_error(
    cli_abort_ifnot(
      1 == 2
    ),
    "`1 == 2` must be `TRUE`"
  )
})
