#' Yes No with Variable Responses using cli
#'
#' This is a modified version of [yesno::yesno()] and [usethis::ui_yeah()] to
#' work with [cli_inform()].
#'
#' @param message Passed to [cli_inform()]
#' @param yes,no Character strings with yes and no options.
#' @param n_yes,n_no Number of yes and no options to provide in user prompt.
#' @inheritParams cli::cli_abort
#' @export
#' @importFrom rlang is_interactive
#' @importFrom utils menu
cli_yesno <- function(message,
                      yes = c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely"),
                      no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
                      n_yes = 1,
                      n_no = 1,
                      call = .envir,
                      .envir = parent.frame()) {
  cli_abort_ifnot(
    c(
      "User input required, but session is not interactive.",
      "Query: ", message
    ),
    call = .envir,
    condition = is_interactive()
  )

  qs <- c(sample(yes, n_yes), sample(no, n_no))
  rand <- sample(length(qs))

  cli_inform(message, .envir = .envir)
  utils::menu(qs[rand]) == which(rand == 1)
}

#' Display a Message then Read a Line from the Terminal
#'
#' @param message Primary prompt to user prefixed by ">" when passed to
#'   [cli_inform()].
#' @param ... Secondary messages passed to [cli_inform()]
#' @param prompt Character to show as user prompt in console following displayed
#'   message.
#' @inheritParams cli::cli_inform
#' @export
cli_ask <- function(message, ..., prompt = ">> ", .envir = parent.frame()) {
  cli_inform(c(">" = message, ...), .envir = .envir)
  readline(prompt = prompt)
}

#' Signal an error, warning or message with a cli formatted message if a
#' condition is FALSE
#'
#'
#' @param condition if condition is not logical, it is assumed to be a single
#'   argument function that is applied to the object passed to the .data
#'   parameter.
#' @param .data object to test with condition if condition is a function.
#' @inheritParams cli::cli_abort
#' @export
#' @importFrom rlang is_logical as_function
cli_abort_ifnot <- function(...,
                            condition = FALSE,
                            .data = NULL,
                            call = .envir,
                            .envir = parent.frame()) {
  if (!rlang::is_logical(condition)) {
    condition <- condition_as_fn(condition, .data, call = call)
  }

  if (!condition) {
    cli_abort(
      ...,
      call = call,
      .envir = .envir
    )
  }

  invisible(NULL)
}

#' @name cli_warn_ifnot
#' @rdname cli_abort_ifnot
#' @export
#' @importFrom rlang is_logical
cli_warn_ifnot <- function(...,
                           condition = FALSE,
                           .data = NULL,
                           .envir = parent.frame()) {
  if (!rlang::is_logical(condition)) {
    condition <- condition_as_fn(condition, .data, call = call)
  }

  if (!condition) {
    cli_warn(
      ...,
      .envir = .envir
    )
  }

  invisible(NULL)
}


#' @name cli_inform_ifnot
#' @rdname cli_abort_ifnot
#' @export
#' @importFrom rlang is_logical
cli_inform_ifnot <- function(...,
                             condition = FALSE,
                             .data = NULL,
                             .envir = parent.frame()) {
  if (!rlang::is_logical(condition)) {
    condition <- condition_as_fn(condition, .data, call = call)
  }

  if (!condition) {
    cli_inform(
      ...,
      .envir = .envir
    )
  }

  invisible(NULL)
}


#' Use condition argument as function
#'
#' @noRd
#' @importFrom rlang is_function as_function is_logical
condition_as_fn <- function(condition,
                            .data = NULL,
                            call = parent.frame()) {
  if (!rlang::is_function(condition)) {
    condition <- rlang::as_function(condition)
  }

  condition <- condition(.data)

  if (rlang::is_logical(condition)) {
    return(condition)
  }

  cli_abort(
    "{.fn condition} returned a {.cls {class(condition)} object, not logical.",
    call = call
  )
}


#' Format a list of items as an unordered list with cli_ul()
#'
#' @rdname cli_ul_items
#' @export
#' @importFrom cli cli_ul cli_li cli_end
cli_ul_items <- function(items) {
  items <- sapply(items, as.character)
  cli::cli_ul()
  sapply(
    seq_along(items),
    function(x) {
      cli::cli_li("{.code {names(items)[[x]]}}: {.val {items[[x]]}}")
    }
  )
  cli::cli_end()
}
