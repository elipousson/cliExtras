#' Helper functions to get or set the cliExtras.quiet option
#'
#' [cli_quiet()] gets the cliExtras.quiet option and [set_cli_quiet()] sets the
#' option.
#' @param quiet Option value to set or default value if option is not set.
#'   Defaults to `FALSE`.
#' @export
cli_quiet <- function(quiet = FALSE) {
  getOption("cliExtras.quiet", default = quiet)
}

#' @name set_cli_quiet
#' @rdname cli_quiet
#' @param msg If `TRUE`, [set_cli_quiet()] displays a message confirming the
#'   option changes. If `FALSE`, the function does not display a message.
#' @export
#' @importFrom cli cli_rule cli_inform cli_end
set_cli_quiet <- function(quiet = FALSE, msg = !quiet) {
  handler_label <- "`NULL`"
  quiet_label <- "allow"
  default_handler <- NULL

  if (quiet) {
    quiet_label <- "muffle"
    handler_label <- cli::cli_fmt(cli::cli_text("{.fn suppressMessages}"))
    default_handler <- suppressMessages
  }

  if (cli_quiet() && !quiet) {
    options("cli.default_handler" = default_handler)
    options("cliExtras.quiet" = quiet)
  }

  if (isTRUE(msg)) {
    cli::cli_rule("Updating options", id = "set.cli.quiet")
    cli::cli_inform(
      c(
        "v" = "Setting {.field cli.default_handler} to {handler_label} to
      {quiet_label} {.pkg cli} messages.",
      "v" = "Setting {.field cliExtras.quiet} to {.code {quiet}} to
      {quiet_label} {.pkg cliExtras} warnings and info messages."
      ),
      id = "set.cli.quiet"
    )
    cli::cli_end(id = "set.cli.quiet")
  }

  options("cli.default_handler" = default_handler)
  options("cliExtras.quiet" = quiet)
}

#' Quiet version of [cli_inform()]
#'
#' The implementation of the cliExtras.quiet option is based on the
#' implementation of a similar setting in googlesheets4, googledrive, and
#' gargle. If the cliExtras.quiet option is set to `TRUE`, [quiet_cli_inform()]
#' does not trigger a message. It is used by [cli_inform_ifnot()] and
#' [cli_inform_if()] but not by most other functions in cliExtras.
#'
#' @param ... Additional parameters passed to [cli_inform()]
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
