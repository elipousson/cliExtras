#' Display a list of files as a list of items
#'
#' @inheritParams base::list.files
#' @param message Passed to [cli::cli_inform()].
#' @param bullet Character defining style to use list of file names.
#' @param .envir Passed to [cli::cli_inform()]. Defaults to
#'   [rlang::current_env()] rather than [parent.frame()] to support evaluation
#'   of default message that includes the number of files found at the path.
#' @param n_show Number of file names to show in list. The remaining number of
#'   files n_show are noted at the end of the list but the file names are not
#'   displayed. Defaults to 10.
#' @param return_list If `TRUE`, return the list of files after displaying the
#'   cli message. Defaults to `FALSE`.
#' @inheritDotParams base::list.files
#' @examples
#' cli_list_files(system.file("R", package = "cliExtras"), n_show = 5)
#'
#' @seealso
#'  [cli::cli_bullets()]
#' @name cli_list_files
#' @export
#' @importFrom cli cli_inform cli_bullets
#' @importFrom rlang set_names
cli_list_files <- function(path,
                           pattern = NULL,
                           full.names = FALSE,
                           message = "{length(files)} file{?s} found in {.path {path}}:",
                           bullet = "*",
                           n_show = 10,
                           .envir = current_env(),
                           return_list = FALSE,
                           ...) {
  files <- list.files(path = path, pattern = pattern, full.names = full.names, ...)

  if (identical(files, character(0))) {
    cli::cli_alert_danger(
      "No files found at {.arg path}: {.path {path}}"
    )

    return(invisible(NULL))
  }

  cli::cli_alert_info(
    text = message,
    wrap = TRUE,
    .envir = .envir
  )

  style <- "file"

  cli::cli_bullets(
    bulletize(files, n_show = n_show, style = style)
  )

  if (return_list) {
    return(invisible(files))
  }
}
