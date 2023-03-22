#' Signal an error, warning, or message with a cli formatted message if any
#' expressions in ... are not all TRUE or are all TRUE
#'
#' @param ... Any number of R expressions which should each evaluate to a
#'   logical vector. If `...` is named, the names will be passed to as the
#'   message parameter. If message is provided, any names for the logical
#'   expressions are ignored. If only some items from `...` are named, the
#'   missing names are created with [rlang::exprs_auto_name()]. If a single list
#'   is provided, the list is assumed to be a named list of logical values or
#'   expressions that evaluate to logical values.
#' @param condition Logical. For `ifnot` style functions, if `FALSE`, signal an
#'   error, warning, or message. For `if` style functions, signal an error,
#'   warning, or message if `TRUE`. Defaults to `NULL`. Ignored if multiple
#'   parameters are provided to `...`
#' @inheritParams cli::cli_abort
#' @export
#' @importFrom cli cli_abort
cli_abort_ifnot <- function(...,
                            message = NULL,
                            call = .envir,
                            .envir = parent.frame(),
                            .frame = .envir,
                            condition = NULL) {
  cli_ifnot_msg(
    ...,
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame,
    condition = condition,
    .fn = cli::cli_abort
  )
}

#' @name cli_abort_if
#' @rdname cli_abort_ifnot
#' @export
#' @importFrom cli cli_abort
cli_abort_if <- function(...,
                         message = NULL,
                         call = .envir,
                         .envir = parent.frame(),
                         .frame = .envir,
                         condition = NULL) {
  cli_if_msg(
    ...,
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame,
    condition = condition,
    .fn = cli::cli_abort
  )
}

#' @name cli_warn_ifnot
#' @rdname cli_abort_ifnot
#' @export
#' @importFrom cli cli_warn
cli_warn_ifnot <- function(...,
                           message = NULL,
                           .envir = parent.frame(),
                           condition = NULL) {
  cli_ifnot_msg(
    ...,
    message = message,
    .envir = .envir,
    condition = condition,
    .fn = cli::cli_warn
  )
}

#' @name cli_warn_if
#' @rdname cli_abort_ifnot
#' @export
cli_warn_if <- function(...,
                        message = NULL,
                        .envir = parent.frame(),
                        condition = NULL) {
  cli_if_msg(
    ...,
    message = message,
    .envir = .envir,
    condition = condition,
    .fn = cli::cli_warn
  )
}

#' @name cli_inform_ifnot
#' @rdname cli_abort_ifnot
#' @export
cli_inform_ifnot <- function(...,
                             message = NULL,
                             .envir = parent.frame(),
                             condition = NULL) {
  cli_ifnot_msg(
    ...,
    message = message,
    .envir = .envir,
    condition = condition,
    .fn = quiet_cli_inform
  )
}

#' @name cli_inform_if
#' @rdname cli_abort_ifnot
#' @export
cli_inform_if <- function(...,
                          message = NULL,
                          .envir = parent.frame(),
                          condition = NULL) {
  cli_if_msg(
    ...,
    message = message,
    .envir = .envir,
    condition = condition,
    .fn = quiet_cli_inform
  )
}


#' @noRd
cli_ifnot_msg <- function(...,
                          message = NULL,
                          call = NULL,
                          .envir = parent.frame(),
                          .frame = NULL,
                          condition = NULL,
                          .fn) {
  cli_if_msg(
    ...,
    message = message,
    call = call,
    .envir = .envir,
    .frame = .frame,
    condition = condition,
    .fn = .fn,
    not = TRUE
  )
}

#' @noRd
#' @importFrom rlang check_required is_logical is_empty is_character is_true
#'   is_named
cli_if_msg <- function(...,
                       message = NULL,
                       call = NULL,
                       .envir = parent.frame(),
                       .frame = NULL,
                       condition = NULL,
                       .fn,
                       not = FALSE) {
  rlang::check_required(.fn, call = call)
  params <- set_params(..., not = not)

  if (length(params) > 1) {
    # Ignore any condition value if multiple params are provided
    for (x in seq_along(params)) {
      # FIXME: Check if how this works with params[x] vs. params[[x]]
      cli_if_msg(
        params[x],
        message = message %||% names(params)[x],
        call = call,
        .envir = .envir,
        .frame = .frame,
        .fn = .fn,
        not = not
      )
    }

    return(invisible())
  }

  if (rlang::is_logical(condition)) {
    if (!rlang::is_empty(params) && rlang::is_character(params, n = 1)) {
      message <- params[[1]]
    }

    params[[1]] <- condition
  }

  params[[1]] <- set_ifnot(
    x = params[[1]],
    message = "{.arg condition} or conditions passed to {.arg ...}
    must be {.cls logical}, not {.cls {class(params[[1]])}}.",
    not = not, call = call, .envir = current_env()
  )

  cli_if(
    x = params[[1]],
    message = message %||% names(params)[1],
    .fn = .fn,
    call = call,
    .envir = .envir,
    .frame = .frame
  )
}

#' @noRd
#' @importFrom rlang list2 is_empty has_length is_list is_named is_true
#'   is_named2 has_name exprs_auto_name exprs set_names
set_params <- function(..., not = FALSE) {
  params <- rlang::list2(...)

  if (rlang::is_empty(params)) {
    return(params)
  }

  nm_list_params <-
    all(
      c(
        rlang::has_length(params, n = 1),
        rlang::is_list(params),
        rlang::is_named(params[[1]])
      )
    )

  if (rlang::is_true(nm_list_params)) {
    return(params[[1]])
  }

  if (rlang::is_named2(params)) {
    return(params)
  }

  empty_nm <- rlang::has_name(params, "")

  replace_nm <- names(rlang::exprs_auto_name(rlang::exprs(...)))

  replace_nm[empty_nm] <-
    paste0(
      "{.code ", replace_nm[empty_nm], "}", " must be {.code ", not, "}"
    )

  rlang::set_names(params, replace_nm)
}

#' @noRd
#' @importFrom rlang is_logical is_true caller_arg current_env
#' @importFrom cli cli_abort
set_ifnot <- function(x,
                      arg = caller_arg(x),
                      message = "{.arg {arg}} must be {.cls logical}, not {.cls {class(x)}}.",
                      not = FALSE,
                      call = parent.frame(),
                      .envir = current_env()) {
  if (!rlang::is_logical(x)) {
    cli::cli_abort(message, .envir = .envir, call = call)
  }

  if (rlang::is_true(not)) {
    return(!x)
  }

  x
}
