test_that("cli_progress_pipe returns the input data unchanged", {
  df <- data.frame(x = 1:3)
  expect_message(
    res <- cli_progress_pipe(df, "Data has {nrow(data)} rows."),
    "Data has 3 rows"
  )
  expect_identical(res, df)
})

test_that("cli_progress_pipe works within a pipe", {
  df <- data.frame(letters = LETTERS, numbers = 1:26)
  expect_message(
    res <- df |>
      cli_progress_pipe("Data has {nrow(data)} rows and {ncol(data)} columns.") |>
      head(2),
    "26 rows and 2 columns"
  )
  expect_identical(res, head(df, 2))
})

test_that("cli_progress_pipe pauses when time is provided", {
  df <- data.frame(x = 1)
  start <- Sys.time()
  suppressMessages(cli_progress_pipe(df, "msg", time = 0.05))
  expect_gte(as.numeric(Sys.time() - start, units = "secs"), 0.05)
})
