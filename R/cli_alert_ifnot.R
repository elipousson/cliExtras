#' Display an alert message if condition is met
#'
#' @noRd
#' @keywords internal
#' @importFrom rlang check_required is_logical is_empty is_character is_true
#'   is_named
cli_conditional_alert <- function(text,
                                  condition = NULL,
                                  fn,
                                  not = FALSE,
                                  id = NULL,
                                  class = NULL,
                                  wrap = TRUE,
                                  .envir = parent.frame(),
                                  call = parent.frame()) {
  fn <- set_cli_alert_fn(fn, call)

  condition <- set_ifnot(x = condition, not = not, call = call)

  if (!rlang::is_true(condition)) {
    return(invisible())
  }

  fn(
    text = text,
    id = id,
    class = class,
    wrap = wrap,
    .envir = .envir
  )
}

#' Set and validate function for cli_conditional_alert
#'
#' @noRd
#' @keywords internal
set_cli_alert_fn <- function(fn, call = parent.frame()) {
  rlang::check_required(fn, call = call)

  if (is.character(fn)) {
    fn <- switch(fn,
      "danger" = cli::cli_alert_danger,
      "info" = cli::cli_alert_info,
      "success" = cli::cli_alert_success,
      "warning" = cli::cli_alert_warning
    )
  }

  if (!rlang::is_function(fn)) {
    cli::cli_abort(
      '{.arg fn} must be a {.pkg cli} function or one of these strings:
      {c("danger", "info", "success", "warning")}',
      call = call
    )
  }

  fn
}

#' CLI conditional alerts
#'
#' Alerts are typically short status messages. Note: These functions use `wrap =
#' TRUE` by default. Alert messages can be muffled by [set_cli_quiet()] while
#' [cli::cli_inform()] messages are not.
#'
#' @inheritParams cli::cli_alert
#' @param condition If `TRUE`, display alert for "if" functions. If `FALSE`,
#'   display alert for "ifnot" functions.
#' @param fn cli function to use for alert. Defaults to `cli::cli_alert`.
#'   Supported options also include "danger", "info", "success", and "warning".
#' @param ... Additional parameters passed to cli_alert_if or cli_alert_ifnot by
#'   functions like `cli_info_ifnot()`.
#' @param call Caller environment. Used to improve error messages for argument
#'   checks.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   cli_alert_if(text = "Example text", condition = TRUE)
#'
#'   cli_alert_ifnot(text = "Example text", condition = FALSE)
#'
#'   cli_alert_success_if(text = "Success!", condition = TRUE)
#'
#'   cli_alert_danger_ifnot(text = "Danger!", condition = FALSE)
#' }
#' }
#' @seealso
#'  \code{\link[cli]{cli_alert}}
#' @rdname cli_alert_ifnot
#' @export
#' @importFrom cli cli_alert
cli_alert_ifnot <- function(text = NULL,
                            condition = NULL,
                            fn = cli::cli_alert,
                            id = NULL,
                            class = NULL,
                            wrap = TRUE,
                            .envir = parent.frame(),
                            call = parent.frame()) {
  cli_conditional_alert(
    text = text,
    condition = condition,
    fn = fn,
    .envir = .envir,
    id = id,
    class = class,
    call = call,
    not = TRUE
  )
}

#' @name cli_danger_ifnot
#' @rdname cli_alert_ifnot
#' @export
cli_danger_ifnot <- function(text = NULL, condition = NULL, ..., .envir = parent.frame()) {
  cli_alert_ifnot(
    text = text,
    condition = condition,
    ...,
    .envir = .envir, fn = "danger"
  )
}

#' @name cli_info_ifnot
#' @rdname cli_alert_ifnot
#' @export
cli_info_ifnot <- function(text = NULL, condition = NULL, ..., .envir = parent.frame()) {
  cli_alert_ifnot(
    text = text,
    condition = condition,
    ...,
    .envir = .envir, fn = "info"
  )
}

#' @name cli_success_ifnot
#' @rdname cli_alert_ifnot
#' @export
cli_success_ifnot <- function(text = NULL, condition = NULL, ..., .envir = parent.frame()) {
  cli_alert_ifnot(
    text = text,
    condition = condition,
    ...,
    .envir = .envir, fn = "success"
  )
}

#' @name cli_warning_ifnot
#' @rdname cli_alert_ifnot
#' @export
cli_warning_ifnot <- function(text = NULL, condition = NULL, ..., .envir = parent.frame()) {
  cli_alert_ifnot(
    text = text,
    condition = condition,
    ...,
    .envir = .envir, fn = "warning"
  )
}

#' @name cli_alert_if
#' @rdname cli_alert_ifnot
#' @export
cli_alert_if <- function(text = NULL,
                         condition = NULL,
                         fn = cli::cli_alert,
                         id = NULL,
                         class = NULL,
                         wrap = TRUE,
                         .envir = parent.frame(),
                         call = parent.frame()) {
  cli_conditional_alert(
    text = text,
    condition = condition,
    fn = fn,
    .envir = .envir,
    id = id,
    class = class,
    call = call,
    not = FALSE
  )
}

#' @name cli_danger_if
#' @rdname cli_alert_ifnot
#' @export
cli_danger_if <- function(text = NULL, condition = NULL, ..., .envir = parent.frame()) {
  cli_alert_if(
    text = text,
    condition = condition,
    ...,
    .envir = .envir, fn = "danger"
  )
}

#' @name cli_info_if
#' @rdname cli_alert_ifnot
#' @export
cli_info_if <- function(text = NULL, condition = NULL, ..., .envir = parent.frame()) {
  cli_alert_if(
    text = text,
    condition = condition,
    ...,
    .envir = .envir, fn = "info"
  )
}

#' @name cli_success_if
#' @rdname cli_alert_ifnot
#' @export
cli_success_if <- function(text = NULL, condition = NULL, ..., .envir = parent.frame()) {
  cli_alert_if(
    text = text,
    condition = condition,
    ...,
    .envir = .envir, fn = "success"
  )
}

#' @name cli_warning_if
#' @rdname cli_alert_ifnot
#' @export
cli_warning_if <- function(text = NULL, condition = NULL, ..., .envir = parent.frame()) {
  cli_alert_if(
    text = text,
    condition = condition,
    ...,
    .envir = .envir, fn = "warning"
  )
}
