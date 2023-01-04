#' Simplified cli progress message for combining with a pipe
#'
#' A convenience function for creating progress messages that use piped data as
#' an input.
#'
#' @param data Input data. Reference w/ "{data}" if .envir is currrent_env().
#' @param .envir The environment to use for auto-termination and for glue
#'   substitution. It is also used to find and set the current progress bar.
#'   Defaults to [current_env()] ([cli::cli_progress_bar()] uses
#'   [parent.frame()])
#' @inheritParams cli::cli_progress_message
#' @param clear Whether to remove the progress bar from the screen after it has
#'   terminated. Defaults to `FALSE` ([cli::cli_progress_bar()] uses `TRUE`).
#' @inheritDotParams cli::cli_progress_bar
#' @param time Passed to [Sys.sleep()] for optional pause after displaying
#'   progress message. Defaults to `NULL`.
#' @examples
#' df <- data.frame("letters" = LETTERS, "numbers" = c(1:26))
#'
#' df |>
#'   cli_progress_pipe("Data has {nrow(data)} rows and {ncol(data)} columns.") |>
#'   head(2)
#' @export
cli_progress_pipe <- function(data,
                              ...,
                              current = TRUE,
                              clear = FALSE,
                              .auto_close = TRUE,
                              .envir = current_env(),
                              time = NULL) {
  cli::cli_progress_message(
    ...,
    current = current,
    clear = clear,
    .auto_close = .auto_close,
    .envir = .envir
  )

  if (!is.null(time)) {
    Sys.sleep(time)
  }

  data
}
