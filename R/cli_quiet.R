#' Helper functions to get or set the cli.quiet option
#'
#' [cli_quiet()] gets the cli.quiet option and [set_cli_quiet()] sets the
#' option.
#' @param quiet Option value to set or default value if option is not set.
#'   Defaults to `FALSE`.
#' @export
cli_quiet <- function(quiet = FALSE) {
  getOption("cli.quiet", default = quiet)
}

#' @name set_cli_quiet
#' @rdname cli_quiet
#' @export
set_cli_quiet <- function(quiet = FALSE) {
  options("cli.quiet" = quiet)
}

#' Quiet versions of [cli_inform()], [cli_bullets()], and [cli_alert()]
#'
#' The implementation of the cli.quiet option is based on the implementation of
#' a similar setting in googlesheets4, googledrive, and gargle.
#'
#' @param ... Additional parameters passed to [cli_inform()], [cli_bullets()],
#'   or [cli_alert()]
#' @inheritParams cli::cli_inform
#' @export
#' @importFrom rlang is_true
#' @importFrom cli cli_inform
quiet_cli_inform <- function(..., .envir = parent.frame()) {
  quiet <- cli_quiet()
  if (!is.na(quiet) && rlang::is_true(quiet)) {
    return(invisible())
  }

  cli::cli_inform(
    ...,
    .envir = .envir
  )
}

#' @name quiet_cli_bullets
#' @rdname quiet_cli_inform
#' @export
#' @importFrom rlang is_true
#' @importFrom cli cli_bullets
quiet_cli_bullets <- function(..., .envir = parent.frame()) {
  quiet <- cli_quiet()
  if (!is.na(quiet) && rlang::is_true(quiet)) {
    return(invisible())
  }

  cli::cli_bullets(
    ...,
    .envir = .envir
  )
}

#' @name quiet_cli_alert
#' @rdname quiet_cli_inform
#' @export
#' @importFrom rlang is_true
#' @importFrom cli cli_alert
quiet_cli_alert <- function(..., .envir = parent.frame()) {
  quiet <- cli_quiet()
  if (!is.na(quiet) && rlang::is_true(quiet)) {
    return(invisible())
  }

  cli::cli_alert(
    ...,
    .envir = .envir
  )
}
