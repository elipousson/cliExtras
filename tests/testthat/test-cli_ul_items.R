test_that("cli_ul_items displays a name/value bullet list", {
  msgs <- capture_messages(cli_ul_items(c(a = 1, b = 2)))
  expect_length(msgs, 2)
  expect_match(msgs[1], "a")
  expect_match(msgs[1], "1")
  expect_match(msgs[2], "b")
  expect_match(msgs[2], "2")
})

test_that("cli_ul_items uses the style and sep arguments", {
  msgs <- capture_messages(
    cli_ul_items(c(a = "x"), style = c("field", "val"), sep = " = ")
  )
  expect_match(msgs, "a")
  expect_match(msgs, "x")
})
