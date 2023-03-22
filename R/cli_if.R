#' Execute a cli function if a predicate function returns TRUE
#'
#' Execute a function if a predicate function returns TRUE. Intended for use
#' with cli functions.
#'
#' @param x Parameter to passed to .predicate function, Default: `NULL`
#' @param ... Additional parameters passed to .fn.
#' @param .predicate Single parameter predicate function, Defaults to
#'   `rlang::is_true` for [cli_if()] or `rlang::is_false` for [cli_ifnot()]. If
#'   .predicate returns `TRUE`, execute .fn. Aborts if .predicate does not
#'   return a boolean value.
#' @param .fn Function to call with ... parameters if x, Default: `NULL`
#' @param .default Default function to execute when .predicate function returns
#'   `TRUE`, Default: `cli::cli_alert`
#' @returns The output from the .fn function or .default if .fn is `NULL`
#' @examples
#' cli_if(FALSE, "No alert.")
#'
#' cli_if(TRUE, "Alert on TRUE!")
#'
#' cli_ifnot(FALSE, "Alert on FALSE!")
#'
#' @rdname cli_if
#' @inheritParams rlang::args_error_context
#' @export
#' @export
#' @importFrom rlang is_true try_fetch is_bool is_error call2 has_name
#'   call_args_names call_modify
#' @importFrom cli cli_alert cli_abort
cli_if <- function(x = NULL,
                   ...,
                   .predicate = is_true,
                   .fn = NULL,
                   .default = cli_alert,
                   call = caller_env()) {
  check <- rlang::try_fetch(
    .predicate(x),
    error = function(cnd) cnd
  )

  if (!rlang::is_bool(check)) {
    parent <- NULL
    if (rlang::is_error(check)) {
      parent <- check
    }

    cli::cli_abort(
      "{.fn {.predicate}} must return a {.cls logical} object,
      not {.obj_type_friendly {check}}.",
      call = call,
      parent = parent
    )
  }

  if (rlang::is_true(check)) {
    .fn <- .fn %||% .default
    fn_call <- rlang::call2(.fn, ...)
    if (rlang::has_name(rlang::call_args_names(fn_call), "call")) {
      fn_call <- rlang::call_modify(fn_call, call = call, .homonyms = "last")
    }
    rlang::try_fetch(
      eval(fn_call),
      error = function(cnd) {
        cli::cli_abort(
          "{.fn {quote(.fn)}} can't be evaluated.",
          parent = cnd,
          call = call
        )
      }
    )
  }
}

#' @name cli_ifnot
#' @rdname cli_if
#' @export
#' @importFrom cli cli_alert
cli_ifnot <- function(x = NULL,
                      ...,
                      .predicate = is_false,
                      .fn = NULL,
                      .default = cli_alert,
                      call = caller_env()) {
  cli_if(
    x = x,
    ...,
    .predicate = .predicate,
    .fn = .fn,
    .default = .default,
    call = call
  )
}
