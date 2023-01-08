#' Display a Message then Read a Line from the Terminal
#'
#' @param title Title for menu. Character vector passed to [cli::cli_h2()] if
#'   title is length 1 or to [cli::cli_bullets()] if length is greater than 1.
#' @param message Additional message to display after choices and before prompt.
#' @param choices Required vector or list of choices.
#' @param prompt Characters to show as user prompt in console following
#'   displayed menu options. Defaults to "Selection:" (matching [utils::menu()])
#' @param exit Character to use to exit menu.
#' @param ind If `TRUE`, return index position, if `FALSE` (default), return
#'   item from choices.
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
#' @importFrom rlang check_required set_names
#' @importFrom cli cli_h2 cli_bullets cat_rule console_width cli_ol cli
cli_menu <- function(choices,
                     title = NULL,
                     message = "Enter your selection or press {.kbd 0} to exit.",
                     prompt = "Selection:",
                     exit = 0,
                     ind = FALSE,
                     id = NULL,
                     call = .envir,
                     .envir = parent.frame()) {
  rlang::check_required(choices)

  if (!is.null(title)) {
    if (length(title) == 1) {
      title <- cli::cli_h2(text = title, id = id, .envir = .envir)
    } else if (length(title) > 1) {
      title <- cli::cli_bullets(title, id = id, .envir = .envir)
    }
  }

  title <- title %||% cli::cat_rule(
    width = as.integer(cli::console_width() / 2)
    )

  choices <- rlang::set_names(choices, seq_along(choices))

  choice_bullets <- NULL

  if (is.list(choices)) {
    choice_bullets <- vector("list", length(choices))

    for (i in seq_along(choices)) {
      choice_bullets[[i]] <-
        cli_bulletize(
          "{choices[i]}",
          bullet = "",
          before = names(choices[i]),
          sep = ". ",
          id = id,
          .envir = current_env()
        )
    }
  } else if (is.vector(choices)) {
    choice_bullets <-
      cli::cli_ol(
        choices,
        id = id
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

  choice <- cli_ask(
    prompt,
    message,
    .envir = .envir
  )

  while(TRUE) {
    if (choice == as.integer(exit)) {
      return(invisible())
    }

    if (choice %in% seq_along(choices)) {
      if (ind) {
        return(choice)
      }

      return(choices[[choice]])
    }

    choice <- cli_ask(
      prompt = prompt,
      .envir = .envir
    )
  }
}
