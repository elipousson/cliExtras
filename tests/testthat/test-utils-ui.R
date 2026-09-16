test_that("bracketize wraps values in braces", {
  expect_identical(bracketize("value"), "{value}")
  expect_identical(bracketize("a", "b"), "{ab}")
  expect_identical(
    bracketize("value", .open = "[", .close = "]"),
    "[value]"
  )
  expect_identical(
    bracketize(c("a", "b"), collapse = ", "),
    "{a}, {b}"
  )
})

test_that("stylize adds a cli style prefix", {
  expect_identical(stylize("value", "val"), "{.val value}")
  expect_identical(stylize("value", "val", bracket = TRUE), "{.val {value}}")
})

test_that("stylize returns x unchanged when style is missing", {
  expect_identical(stylize("value"), "value")
  expect_identical(stylize("value", style = ""), "value")
  expect_identical(stylize("value", style = NULL), "value")
})

test_that("bulletize creates a named bullet vector", {
  out <- bulletize(c("a", "b", "c"))
  expect_identical(unname(out), c("a", "b", "c"))
  expect_identical(names(out), rep("*", 3))
})

test_that("bulletize truncates long vectors and reports the remainder", {
  out <- bulletize(rep("val", 20), n_show = 3)
  expect_length(out, 4)
  expect_identical(unname(out)[1:3], rep("val", 3))
  expect_match(unname(out)[4], "17 more")
  expect_identical(names(out)[4], " ")
})

test_that("bulletize applies the n_fudge tolerance", {
  # n = 7, n_show = 5: 7 - 5 = 2, which is not > n_fudge (2), so nothing is
  # truncated and all 7 items are shown.
  out <- bulletize(letters[1:7], n_show = 5, n_fudge = 2)
  expect_length(out, 7)

  # n = 8, n_show = 5: 8 - 5 = 3 > n_fudge (2), so items are truncated.
  out2 <- bulletize(letters[1:8], n_show = 5, n_fudge = 2)
  expect_length(out2, 6)
  expect_match(unname(out2)[6], "3 more")
})

test_that("bulletize applies before/after/sep and style", {
  out <- bulletize("value", before = "[", after = "]", sep = "-")
  expect_identical(unname(out), "[-value]")

  out2 <- bulletize("value", style = "val")
  expect_identical(unname(out2), "{.val value}")
})
