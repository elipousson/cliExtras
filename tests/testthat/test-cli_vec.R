test_that("cli_vec_last sets the vec-last style", {
  x <- cli_vec_last(c("a", "b", "c"))
  expect_identical(attr(x, "cli_style")[["vec-last"]], " or ")
  expect_identical(cli::cli_fmt(cli::cli_text("{x}")), "a, b or c")
})

test_that("cli_vec_last accepts a custom vec_last and style", {
  x <- cli_vec_last(c("a", "b"), style = list(color = "blue"), vec_last = " and ")
  expect_identical(attr(x, "cli_style")[["vec-last"]], " and ")
  expect_identical(attr(x, "cli_style")[["color"]], "blue")
  expect_identical(cli::cli_fmt(cli::cli_text("{x}")), "a and b")
})

test_that("cls_vec wraps values in angle brackets", {
  x <- cls_vec(c("a", "b"))
  expect_identical(cli::cli_fmt(cli::cli_text("{x}")), "<a> or <b>")
})
