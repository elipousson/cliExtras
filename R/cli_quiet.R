#' Use rlang to set cli.default_handler to suppressMessages as a local or
#' permanent option
#'
#' [cli_quiet()] is a helper to enable a quiet option in other functions.
#'
#' @param quiet If `FALSE`, leave cli.default_handler option unchanged. If
#'   `TRUE`, set cli.default_handler to `suppressMessages` temporaily with
#'   [rlang::local_options()] or permanently with [rlang::push_options()].
#' @param push If `TRUE`, set cli.default_handler option with
#'   [rlang::push_options()].
#' @inheritParams rlang::local_options
#' @examples
#' test_fn <- function(quiet = FALSE) {
#'   cli_quiet(quiet = quiet)
#'   cli::cli_alert_info(
#'     "{.arg quiet} is {.val {quiet}}"
#'   )
#' }
#'
#' options("cli.default_handler" = NULL)
#'
#' test_fn()
#'
#' test_fn(quiet = TRUE)
#'
#' @export
#' @importFrom rlang caller_env is_false is_true push_options local_options
cli_quiet <- function(quiet = FALSE, push = FALSE, .frame = caller_env()) {
  if (rlang::is_false(quiet)) {
    return(invisible(NULL))
  }

  if (rlang::is_true(push)) {
    return(rlang::push_options("cli.default_handler" = suppressMessages))
  }

  rlang::local_options("cli.default_handler" = suppressMessages, .frame = .frame)
}

#' Set the cli.default_handler and cliExtras.quiet options
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' @name set_cli_quiet
#' @param quiet If `TRUE` set the cli.default_handler option to
#'   `suppressMessages` and cliExtras.quiet option to `TRUE`. Defaults to
#'   `FALSE`.
#' @param msg If `TRUE`, [set_cli_quiet()] displays a message confirming the
#'   option changes. If `FALSE`, the function does not display a message.
#'   Defaults to `!quiet`
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
  quiet <- getOption("cliExtras.quiet", default = FALSE)
  if (!is.na(quiet) && rlang::is_true(quiet)) {
    return(invisible())
  }

  cli::cli_inform(
    ...,
    .envir = .envir
  )
}
