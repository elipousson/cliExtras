local_test_files <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  files <- file.path(dir, c("a.R", "b.R", "c.txt"))
  file.create(files)
  dir.create(file.path(dir, "subdir"))
  list(dir = dir, files = files)
}

test_that("cli_list_files lists files found at a directory path", {
  fx <- local_test_files()

  expect_message(
    cli_list_files(fx$dir),
    "4 file/folder.+found at"
  )
  msgs <- capture_messages(cli_list_files(fx$dir))
  expect_match(msgs[2], "a.R")
  expect_match(msgs[3], "b.R")
  expect_match(msgs[4], "c.txt")
  expect_match(msgs[5], "subdir")
})

test_that("cli_list_files can exclude directories", {
  fx <- local_test_files()

  msgs <- capture_messages(cli_list_files(fx$dir, include_dirs = FALSE))
  expect_match(msgs[1], "3 file.+found at")
  expect_no_match(paste(msgs, collapse = ""), "subdir")
})

test_that("cli_list_files lists a supplied vector of files", {
  fx <- local_test_files()

  expect_message(
    cli_list_files(fx$files[1:2]),
    "2 files found"
  )
  msgs <- capture_messages(cli_list_files(fx$files[1:2]))
  expect_match(msgs[2], "a.R")
  expect_match(msgs[3], "b.R")
})

test_that("cli_list_files reports when no files are found at a path", {
  fx <- local_test_files()

  expect_message(
    cli_list_files(file.path(fx$dir, "does-not-exist")),
    "No files found at"
  )
})

test_that("cli_list_files reports when called without path or files", {
  expect_message(
    cli_list_files(),
    "No files found in"
  )
})

test_that("cli_list_files uses custom text when provided", {
  fx <- local_test_files()

  expect_message(
    cli_list_files(fx$dir, text = "Files from cliExtras:"),
    "Files from cliExtras:"
  )
})

test_that("cli_list_files can return the file list invisibly", {
  fx <- local_test_files()

  res <- suppressMessages(cli_list_files(fx$dir, return_list = TRUE))
  expect_setequal(res, c("a.R", "b.R", "c.txt", "subdir"))

  res_novisible <- suppressMessages(cli_list_files(fx$dir))
  expect_null(res_novisible)
})

test_that("cli_list_files shows full paths when show_full is TRUE", {
  fx <- local_test_files()

  msgs <- capture_messages(cli_list_files(fx$files[1:2], show_full = TRUE))
  expect_match(msgs[2], fx$dir, fixed = TRUE)

  msgs_default <- capture_messages(cli_list_files(fx$files[1:2]))
  expect_no_match(msgs_default[2], fx$dir, fixed = TRUE)
})
