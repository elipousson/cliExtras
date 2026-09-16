
#' @noRd
cli_fmt_text <- function(...,
                         .envir = parent.frame(),
                         collapse = FALSE,
                         strip_newline = FALSE) {
  cli::cli_fmt(
    cli::cli_text(..., .envir = .envir),
    collapse = collapse,
    strip_newline = strip_newline
  )
}
