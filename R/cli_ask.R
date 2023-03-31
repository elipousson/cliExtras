#' Display a Message then Read a Line from the Terminal
#'
#' @param prompt Characters to show as user prompt in console following
#'   displayed message. A non-breaking space is always placed after the prompt
#'   before passing to [readline()]. Defaults to "?".
#' @inheritParams cli::cli_bullets
#' @inheritParams rlang::args_error_context
#' @inheritDotParams cli::cli_bullets
#' @seealso [cli_yesno()]
#' @export
#' @importFrom cli cli_bullets
#' @importFrom rlang caller_env is_empty list2
cli_ask <- function(prompt = "?",
                    ...,
                    .envir = rlang::caller_env(),
                    call = .envir) {
  check_interactive(call = call)
  if (!rlang::is_empty(rlang::list2(...))) {
    cli::cli_bullets(..., .envir = .envir)
  }
  readline(paste0(prompt, "\u00a0"))
}
