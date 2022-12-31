#' Signal an error, warning, or message with a cli formatted message if any
#' expressions in ... are not all TRUE or are all TRUE
#'
#' @param ... Any number of R expressions which should each evaluate to a
#'   logical vector. If ... is named, the names will be passed to as the message
#'   parameter. If message is provided, any names for the logical expressions
#'   are ignored.
#' @param condition Logical. For ifnot functions, if `FALSE`, signal an error,
#'   warning, or message. For if functions, signal an error, warning, or message
#'   if TRUE. Defaults to `NULL`. Ignored if multiple parameters are provided to
#'   ...
#' @inheritParams cli::cli_abort
#' @export
#' @importFrom rlang list2 is_true is_named
#' @importFrom cli cli_abort
cli_abort_ifnot <- function(...,
                            message = NULL,
                            call = .envir,
                            .envir = parent.frame(),
                            .frame = .envir,
                            condition = NULL) {
  cli_ifnot(
    ...,
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame,
    condition = condition,
    fn = cli::cli_abort
  )
}

#' @name cli_abort_if
#' @rdname cli_abort_ifnot
cli_abort_if <- function(...,
                         message = NULL,
                         call = .envir,
                         .envir = parent.frame(),
                         .frame = .envir,
                         condition = NULL) {
  cli_if(
    ...,
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame,
    condition = condition,
    fn = cli::cli_abort
  )
}

#' @name cli_warn_ifnot
#' @rdname cli_abort_ifnot
#' @export
cli_warn_ifnot <- function(...,
                           message = NULL,
                           .envir = parent.frame(),
                           condition = NULL) {
  cli_ifnot(
    ...,
    message = message,
    .envir = .envir,
    condition = condition,
    fn = cli::cli_warn
  )
}

#' @name cli_warn_if
#' @rdname cli_abort_ifnot
#' @export
cli_warn_if <- function(...,
                        message = NULL,
                        .envir = parent.frame(),
                        condition = NULL) {
  cli_if(
    ...,
    message = message,
    .envir = .envir,
    condition = condition,
    fn = cli::cli_warn
  )
}

#' @name cli_inform_ifnot
#' @rdname cli_abort_ifnot
#' @export
cli_inform_ifnot <- function(...,
                             message = NULL,
                             .envir = parent.frame(),
                             condition = NULL) {
  cli_ifnot(
    ...,
    message = message,
    .envir = .envir,
    condition = condition,
    fn = quiet_cli_inform
  )
}

#' @name cli_inform_if
#' @rdname cli_abort_ifnot
#' @export
cli_inform_if <- function(...,
                          message = NULL,
                          .envir = parent.frame(),
                          condition = NULL) {
  quiet <-
    cli_ifnot(
      ...,
      message = message,
      .envir = .envir,
      condition = condition,
      fn = quiet_cli_inform
    )
}

#' @noRd
#' @importFrom cli cli_inform
cli_if <- function(...,
                   message = NULL,
                   call = NULL,
                   .envir = parent.frame(),
                   .frame = NULL,
                   condition = NULL,
                   fn) {
  cli_message(
    ...,
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame,
    condition = condition,
    fn = fn,
    not = FALSE
  )
}

#' @noRd
#' @importFrom cli cli_abort
cli_ifnot <- function(...,
                      message = NULL,
                      call = NULL,
                      .envir = parent.frame(),
                      .frame = NULL,
                      condition = NULL,
                      fn) {
  cli_message(
    ...,
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame,
    condition = condition,
    fn = fn,
    not = TRUE
  )
}

#' @noRd
#' @importFrom rlang check_required list2 is_logical is_empty is_character
#'   is_true is_named
cli_message <- function(...,
                        message = NULL,
                        call = NULL,
                        .envir = parent.frame(),
                        .frame = NULL,
                        condition = NULL,
                        fn,
                        not = FALSE) {
  rlang::check_required(fn, call = call)

  params <- rlang::list2(...)

  if (length(params) > 1) {
    # Ignore any condition value if multiple params are provided
    for (x in seq(params)) {
      cli_message(
        params[[x]],
        message = message %||% names(params)[x],
        call = call,
        .envir = .envir,
        .frame = .frame,
        fn = fn,
        not = not
      )
    }

    return(invisible(NULL))
  }

  if (rlang::is_logical(condition)) {
    if (!rlang::is_empty(params) && rlang::is_character(params[[1]])) {
      message <- params[[1]]
    }

    params[[1]] <- condition
  }

  params[[1]] <- set_ifnot(params[[1]], not = not)

  if (!rlang::is_true(params[[1]])) {
    return(invisible(NULL))
  }

  if (rlang::is_named(params)) {
    message <- message %||% names(params)[1]
  }

  if (is.null(call)) {
    fn(
      message = message,
      .envir = .envir
    )

    return(invisible(NULL))
  }

  fn(
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame
  )

  return(invisible(NULL))
}

#' @noRd
#' @importFrom rlang is_true
set_ifnot <- function(x,
                      not = FALSE) {
  if (rlang::is_true(not)) {
    return(!x)
  }

  x
}
