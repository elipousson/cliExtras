test_that("cli_list_files works", {
  skip("tests not working on package build")
  path <- system.file("R", package = "cliExtras")
  expect_message(
    cli_list_files(path),
    "20 files found at"
  )
  expect_message(
    cli_list_files(path, include_dirs = FALSE),
    "20 files found at"
  )
  expect_message(
    cli_list_files(list.files(path)[1:3]),
    "3 files found:"
  )
  expect_message(
    cli_list_files(path, text = "Files from cliExtras:"),
    "Files from cliExtras:"
  )
})
