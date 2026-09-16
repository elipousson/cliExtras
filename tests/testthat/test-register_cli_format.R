test_that("register_cli_format registers a cli_format S3 method", {
  format_fn <- function(x, style = NULL, ...) paste0("<custom:", x, ">")

  register_cli_format("cliExtras_test_class", format_fn)

  x <- structure("value", class = "cliExtras_test_class")
  expect_identical(cli::cli_format(x), "<custom:value>")
})
