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
#' @export
#' @importFrom cli cli_abort
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
#' @importFrom cli cli_warn
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
  cli_if(
    ...,
    message = message,
    .envir = .envir,
    condition = condition,
    fn = quiet_cli_inform
  )
}

#' @noRd
cli_if <- function(...,
                   message = NULL,
                   call = NULL,
                   .envir = parent.frame(),
                   .frame = NULL,
                   condition = NULL,
                   fn) {
  cli_conditional_message(
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
cli_ifnot <- function(...,
                      message = NULL,
                      call = NULL,
                      .envir = parent.frame(),
                      .frame = NULL,
                      condition = NULL,
                      fn) {
  cli_conditional_message(
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
#' @importFrom rlang check_required is_logical is_empty is_character is_true
#'   is_named
cli_conditional_message <- function(...,
                                    message = NULL,
                                    call = NULL,
                                    .envir = parent.frame(),
                                    .frame = NULL,
                                    condition = NULL,
                                    fn,
                                    not = FALSE) {
  rlang::check_required(fn, call = call)

  params <- set_params(..., not = not)

  if (length(params) > 1) {
    # Ignore any condition value if multiple params are provided
    for (x in seq_along(params)) {
      # FIXME: Check if how this works with params[x] vs. params[[x]]
      cli_conditional_message(
        params[x],
        message = message %||% names(params)[x],
        call = call,
        .envir = .envir,
        .frame = .frame,
        fn = fn,
        not = not
      )
    }

    return(invisible())
  }

  if (rlang::is_logical(condition)) {
    if (!rlang::is_empty(params) && rlang::is_character(params[[1]])) {
      message <- params[[1]]
    }

    params[[1]] <- condition
  }

  params[[1]] <- set_ifnot(params[[1]], not, call)

  if (!rlang::is_true(params[[1]])) {
    return(invisible())
  }

  if (rlang::is_named(params)) {
    message <- message %||% names(params)[1]
  }

  if (!is.null(call)) {
    fn(
      message = message,
      call = call,
      .envir = .envir,
      .frame = .frame
    )
  }

  fn(
    message = message,
    .envir = .envir
  )
}

#' @noRd
#' @importFrom rlang list2 is_empty is_named exprs_auto_name exprs set_names
set_params <- function(..., not = FALSE) {
  params <- rlang::list2(...)

  if (rlang::is_empty(params)) {
    return(list())
  }

  list_params <-
    (length(params) == 1) & is.list(params[[1]]) & rlang::is_named(params[[1]])

  if (list_params) {
    return(params[[1]])
  }

  missing_nm <- names(params) == ""

  if (identical(missing_nm, logical(0))) {
    missing_nm <- TRUE
  }

  if (any(missing_nm)) {
    return(params)
  }

  replace_nm <- names(rlang::exprs_auto_name(rlang::exprs(...)))

  replace_nm[missing_nm] <- paste0(
    "{.code ", replace_nm[missing_nm], "}",
    " must be {.code ", not, "}"
  )

  rlang::set_names(params, replace_nm)
}


#' @noRd
#' @importFrom rlang is_logical is_true
#' @importFrom cli cli_abort
set_ifnot <- function(x,
                      not = FALSE,
                      call = parent.frame()) {
  if (!rlang::is_logical(x)) {
    cli::cli_abort("{.arg condition} or {.arg ...} parameters
                   must be {.cls logical}, not {.cls {class(x)}}.",
                   call = call)
  }

  if (rlang::is_true(not)) {
    return(!x)
  }

  x
}
