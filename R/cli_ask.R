#' Display a Message then Read a Line from the Terminal
#'
#' This function aborts if the session is not interactive.
#'
#' @param message Primary prompt to user passed to [cli::cli_inform()].
#' @param prompt Characters to show as user prompt in console following
#'   displayed message.
#' @inheritParams cli::cli_inform
#' @export
cli_ask <- function(message, ..., prompt = ">> ", .envir = parent.frame()) {
  check_interactive()
  cli::cli_inform(message, ..., .envir = .envir)
  readline(prompt = prompt)
}
