#' Display a Message then Read a Line from the Terminal
#'
#' @param message Primary prompt to user passed to [cli::cli_inform()].
#' @param prompt Characters to show as user prompt in console following
#'   displayed message. A non-breaking space is always placed after the prompt
#'   before passing to [readline()].
#' @inheritParams cli::cli_inform
#' @seealso [cli_yesno()]
#' @export
#' @importFrom cli cli_inform cat_rule
cli_ask <- function(prompt = "?",
                    message = NULL,
                    ...,
                    .envir = parent.frame()) {
  check_interactive()
  if (!is.null(message)) {
    cli::cli_text(message, ..., .envir = .envir)
  }

  readline(paste0(prompt, "\u00a0"))
}
