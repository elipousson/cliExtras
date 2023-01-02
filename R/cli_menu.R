#' Display a Message then Read a Line from the Terminal
#'
#' @param title Title for menu. Passed to [cli::cli_h2()]
#' @param message Additional message to display after choices and before prompt.
#' @param choices Required list of choices. If named, use the names as the
#'   choice options.
#' @param prompt Characters to show as user prompt in console following
#'   displayed menu options. Defaults to "Selection:" (matching [utils::menu()])
#' @param labels Style to use for menu choices (from
#'   [isstatic::as_numbered_labels]). Defaults to "arabic". Ignored if choices
#'   are a named vector or list.
#' @param sep Separator between choice name and value. Ignored if choices
#'   are a named vector or list.
#' @inheritParams cli_bulletize
#' @inheritParams cli_ask
#' @inheritParams cli::cli_inform
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   cli_menu(list("A", "B", "C"), title = "Pick a letter?")
#'
#'   cli_menu(c("A", "B", "C"), labels = "Alpha")
#'
#'   cli_menu(list(list(1, 2, 3), "A", "B", "C"), title = "Pick from a list")
#' }
#' }
#' @export
#' @importFrom rlang check_required is_named set_names
#' @importFrom cli cli_h2 cat_rule console_width cli
cli_menu <- function(choices,
                     title = NULL,
                     message = "Enter your selection or press {.kbd 0} to exit.",
                     prompt = "Selection:",
                     labels = "arabic",
                     sep = ": ",
                     bullet = "",
                     exit = "0",
                     ind = FALSE,
                     call = .envir,
                     .envir = parent.frame()) {
  rlang::check_required(choices)

  if (!is.null(title)) {
    title <- cli::cli_h2(text = title, .envir = .envir)
  }

  title <- title %||% cli::cat_rule(width = cli::console_width() / 2)

  if (!rlang::is_named(choices)) {
    choices <- rlang::set_names(
      choices,
      as_numbered_labels(choices, labels)
    )
  }

  choice_bullets <- NULL

  if (is.list(choices)) {
    choice_bullets <- vector("list", length(choices))

    for (i in seq_along(choices)) {
      choice_bullets[[i]] <-
        cli_bulletize(
          "{choices[i]}",
          bullet = bullet,
          before = names(choices[i]),
          sep = sep,
          .envir = current_env()
        )
    }
  } else if (is.vector(choices)) {
    choice_bullets <-
      cli_bulletize(
        choices,
        before = names(choices),
        bullet = bullet,
        sep = sep
      )
  }

  cli_abort_if(
    "{.arg choices} must be a vector object
    (either list or simple vector)." = is.null(choice_bullets)
  )

  cli::cli({
    title
    choice_bullets
  })

  choose_from_menu(
    message = message,
    prompt = prompt,
    choices = choices,
    exit = exit,
    ind = ind
  )
}

#' @noRd
#' @importFrom rlang has_name
choose_from_menu <- function(prompt = ">>",
                             message = NULL,
                             choices,
                             exit = "0",
                             ind = FALSE,
                             .envir = current_frame(),
                             ...) {
  choice <- cli_ask(prompt = prompt, message = message, .envir = parent.frame(), ...)

  choice <- as.character(choice)

  if (choice == exit) {
    return(invisible())
  }

  nm <- tolower(names(choices))

  if (tolower(choice) %in% nm) {
    choice <- which(nm == tolower(choice))

    if (ind) {
      return(choice)
    }

    return(choices[[choice]])
  }

  choose_from_menu(
    message = "Select an item from the menu ({.kbd {names(choices)}}),
        or {.kbd {exit}} to exit.",
    prompt = prompt,
    choices,
    exit
  )
}
