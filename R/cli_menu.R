#' Display a Message then Read a Line from the Terminal
#'
#' @param title Title for menu. Passed to [cli::cli_h1()]
#' @param message Additional message to display after choices and before prompt.
#' @param choices Required list of choices. If named, use the names as the
#'   choice options.
#' @param prompt Characters to show as user prompt in console following
#'   displayed menu options. Defaults to "Selection: " (matching [utils::menu()])
#' @inheritParams isstatic::as_numbered_labels
#' @inheritParams cli_bulletize
#' @inheritParams cli_ask
#' @inheritParams cli::cli_inform
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   cli_menu(c("A", "B", "C"))
#'
#'   cli_menu(c("A", "B", "C"), labels = "Alpha")
#' }
#' }
#' @export
#' @importFrom rlang check_required is_named set_names
#' @importFrom cli cli_h1
cli_menu <- function(choices,
                     title = NULL,
                     message = NULL,
                     prompt = "Selection: ",
                     style = c("num", "val"),
                     labels = "arabic",
                     sep = ". ",
                     bullet = "",
                     call = .envir,
                     .envir = parent.frame()) {
  rlang::check_required(choices)

  if (!is.null(title)) {
    cli::cli_h1(text = title, .envir = .envir)
  }

  if (!rlang::is_named(choices)) {
    choices <- rlang::set_names(
      choices,
      as_numbered_labels(choices, labels, pad = " ")
    )
  }

  cli_bulletize(
    choices,
    bullet = bullet,
    style = style,
    sep = sep
  )

  choose_from_menu(
    message = message,
    prompt = prompt,
    choices,
    call
  )
}

#' @noRd
choose_from_menu <- function(message = NULL,
                             prompt = "Selection: ",
                             choices,
                             call = parent.frame(),
                             exit = "0",
                             ...) {
  selection <- cli_ask(message = message, prompt = prompt, ...)

  if (selection == exit) {
    return(invisible())
  }

  if (!any(rlang::has_name(choices, selection))) {
    choose_from_menu(
      message = "Select an item from the menu
      ({.val {names(choices)}}),
      or {.val {exit}} to exit.",
      prompt = prompt,
      choices,
      call,
      exit
    )
  }

  choices[[selection]]
}
