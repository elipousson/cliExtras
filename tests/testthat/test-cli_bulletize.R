test_that("cli_bulletize displays a bulleted list", {
  expect_message(
    cli_bulletize(c(a = "1", b = "2")),
    "1"
  )
  msgs <- capture_messages(cli_bulletize(c(a = "1", b = "2")))
  expect_length(msgs, 2)
})

test_that("cli_bulletize respects n_show", {
  msgs <- capture_messages(
    cli_bulletize(rep("val", 20), n_show = 3)
  )
  expect_length(msgs, 4)
  expect_match(msgs[4], "17 more")
})

test_that("cli_bulletize applies before/after/sep", {
  msgs <- capture_messages(
    cli_bulletize("value", before = "[", after = "]", sep = "-")
  )
  expect_match(msgs, "\\[-value\\]")
})
