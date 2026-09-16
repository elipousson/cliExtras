test_that("cli_paths is deprecated", {
  expect_warning(
    cli_paths("a.R", "Found"),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("cli_paths displays the count and file bullets", {
  msgs <- withr::with_options(
    list(lifecycle_verbosity = "quiet"),
    capture_messages(cli_paths(c("dir/a.R", "dir/b.R"), "Found"))
  )
  expect_match(msgs[1], "Found 2 files:")
  expect_match(msgs[2], "a.R")
  expect_match(msgs[3], "b.R")
})

test_that("cli_paths shows the basename when files share a directory", {
  msgs <- withr::with_options(
    list(lifecycle_verbosity = "quiet"),
    capture_messages(cli_paths(c("dir/a.R", "dir/b.R"), "Found"))
  )
  expect_no_match(msgs[2], "dir/")
})
