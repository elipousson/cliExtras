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

  cli::cli_inform(message, .envir = .envir)
  utils::menu(qs[rand]) == which(rand == 1)
}

#' Display a Message then Read a Line from the Terminal
#'
#' @param message Primary prompt to user passed to [cli_inform()].
#' @param prompt Characters to show as user prompt in console following
#'   displayed message.
#' @inheritParams cli::cli_inform
#' @export
cli_ask <- function(message, ..., prompt = ">> ", .envir = parent.frame()) {
  cli::cli_inform(message, ..., .envir = .envir)
  readline(prompt = prompt)
}

#' Display a Message then Read a Line from the Terminal
#'
#' @param title title for menu
#'
#' @param prompt Characters to show as user prompt in console following
#'   displayed menu options.
#' @param ... Secondary messages passed to [cli_inform()] with title as message.
#' @inheritParams cli_ask
#' @inheritParams cli::cli_inform
#' @export
cli_menu <- function(choices,
                     title = NULL,
                     message = NULL,
                     prompt = ">> ",
                     choice_style = c("num", "val"),
                     ...) {
  rlang::check_required(choices)

  if (!is.null(title)) {
    cli::cli_inform(
      message = title,
      ...
    )
  }

  if (!rlang::is_named(choices)) {
    choices <- rlang::set_names(choices, as.character(c(1:length(choices))))
  }

  cli_ul_items(
    choices,
    style = choice_style
  )

  choices[[cli_ask(message = message %||% "Selection:", prompt = prompt)]]
}

#' Signal an error, warning, or message with a cli formatted message if any
#' expressions in ... are not all TRUE
#'
#' @param ... Any number of R expressions which should each evaluate to a
#'   logical vector. If ... is named, the names will be passed to as the message
#'   parameter. If message is provided, any names for the logical expressions
#'   are ignored.
#' @param condition Deprecated. Logical. If `FALSE`, signal an error, warning,
#'   or message. Defaults to `NULL`.
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
    fn = cli::cli_inform
  )
}

#' @noRd
#' @importFrom rlang list2 is_true is_named
cli_ifnot <- function(...,
                      message = NULL,
                      call = NULL,
                      .envir = parent.frame(),
                      .frame = NULL,
                      condition = NULL,
                      fn = NULL) {
  params <- rlang::list2(...)

  if (length(params) > 1) {
    for (x in seq(params)) {
      cli_ifnot(
        params[[x]],
        message = message %||% names(params)[x],
        .envir = .envir,
        fn = fn
      )
    }

    return(invisible(NULL))
  }

  # NOTE: This is to support backwards compatibility with the prior syntax
  if (!is.null(condition) && !condition) {
    message <- params[[1]]
    params[[1]] <- FALSE
  }

  if (!rlang::is_true(params[[1]])) {
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
    } else {
      fn(
        message = message,
        .envir = .envir
      )
    }
  }

  invisible(NULL)
}

#' Format a list of items as an unordered list with cli_ul()
#'
#' @rdname cli_ul_items
#' @inheritParams cli::cli_li
#' @param style Length 2 character vector indicating cli style to use for names
#'   of items and style to use for items.
#' @param sep Length 1 character vector with separator between item names and
#'   names.
#' @export
#' @importFrom cli cli_ul cli_li cli_end
cli_ul_items <- function(items, style = c("code", "val"), sep = ": ") {
  items <- sapply(items, as.character)
  cli::cli_ul()
  sapply(
    seq_along(items),
    function(x) {
      cli::cli_li(
        paste0(
          "{.", style[1], " {names(items)[[x]]}}",
          sep,
          "{.", style[2], " {items[[x]]}}"
        )
      )
    }
  )
  cli::cli_end()
}

#' Display a list of file paths
#'
#' @param path One or more file paths.
#' @param msg Passed to message for [cli_inform()]
#' @seealso
#'  [cli::cli_bullets()]
#' @rdname cli_paths
#' @export
#' @importFrom cli cli_inform cli_bullets
#' @importFrom rlang set_names
cli_paths <- function(path,
                      msg) {
  len_path <- length(path)

  cli::cli_inform(
    c("v" = paste(msg, "{len_path} file{?s}:"))
  )

  if (length(unique(dirname(path))) == 1) {
    path <- basename(path)
  }

  path <- paste0("{.file ", path, "}")
  cli::cli_bullets(rlang::set_names(path, rep("*", len_path)))
}

#' Display a list of files as a list of items
#'
#' @inheritParams base::list.files
#' @param message Passed to [cli::cli_inform()].
#' @param .envir Passed to [cli::cli_inform()]. Defaults to
#'   [rlang::current_env()] rather than [parent.frame()] to support evaluation
#'   of default message that includes the number of files found at the path.
#' @inheritDotParams base::list.files
#' @seealso
#'  [cli::cli_bullets()]
#' @name cli_list_files
#' @export
#' @importFrom cli cli_inform cli_bullets
#' @importFrom rlang set_names
cli_list_files <- function(path,
                           pattern = NULL,
                           full.names = TRUE,
                           message = "{length(files)} file{?s} found in {.path {path}}:",
                           bullet = "*",
                           .envir = current_env(),
                           ...) {
  files <- list.files(path = path, pattern = pattern, full.names = full.names, ...)

  cli::cli_inform(
    message = message,
    .envir = .envir
  )

  cli::cli_bullets(
    rlang::set_names(paste0("{.file ", files, "}"), rep(bullet, length(files)))
  )
}
