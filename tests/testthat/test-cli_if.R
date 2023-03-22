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
