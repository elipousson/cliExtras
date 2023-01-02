#' Display a Message then Read a Line from the Terminal
#'
#' This function aborts if the session is not interactive.
#'
#' @param message Primary prompt to user passed to [cli::cli_inform()].
#' @param prompt Characters to show as user prompt in console following
#'   displayed message.
#' @inheritParams cli::cli_inform
#' @export
#' @importFrom cli cli_inform cat_rule
cli_ask <- function(prompt = ">>", message = NULL, ..., .envir = parent.frame()) {
  check_interactive()
  if (!is.null(message)) {
    cli::cli_text(message, ..., .envir = .envir)
  }

  readline(paste0(prompt, "\u00a0"))
}
