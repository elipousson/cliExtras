#' Display a list of files as a list of items
#'
#' @inheritParams base::list.files
#' @param files List to file names to display. Ignored if path is provided. If
#'   path is a vector of existing files, path is used as files. If files share a
#'   single directory, path is set to that directory, otherwise path is set to
#'   `NULL`.
#' @param text Passed to [cli::cli_alert_info()]. If `NULL` (default), text
#'   appears reporting the number of files/folders found at the path (if path
#'   provided).
#' @param bullet Character defining style to use list of file names.
#' @param .envir Passed to [cli::cli_inform()]. Defaults to
#'   [rlang::current_env()] rather than [parent.frame()] to support evaluation
#'   of default message that includes the number of files found at the path.
#' @param n_show Number of file names to show in list. The remaining number of
#'   files n_show are noted at the end of the list but the file names are not
#'   displayed. Defaults to 10.
#' @param show_full If `TRUE`, always show the full file path available. If
#'   `FALSE`, show just the base name. Set to `FALSE` automatically if path is a
#'   vector of file names is a single directory.
#' @param include_dirs If `TRUE`, include directories in listed files. Defaults
#'   to `FALSE`. Passed to the include.dirs parameter of [base::list.files()] if
#'   path is a directory.
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
cli_list_files <- function(path = NULL,
                           files = NULL,
                           pattern = NULL,
                           text = NULL,
                           bullet = "*",
                           n_show = 10,
                           show_full = FALSE,
                           include_dirs = TRUE,
                           return_list = FALSE,
                           .envir = current_env(),
                           ...) {
  if (all(is_dir(path))) {
    files <- list.files(
      path = path,
      pattern = pattern,
      include.dirs = include_dirs,
      ...
    )
  } else if (any(has_fileext(path))) {
    files <- path
    path <- unique(dirname(files))
    if ((length(path) != 1) || all(path == ".")) {
      show_full <- TRUE
      path <- NULL
    }
  }

  if (identical(files, character(0))) {
    text <- "No files found in {.arg files}."
    if (!is.null(path)) {
      text <- "No files found at {.arg path}: {.path {path}}"
    }
    cli::cli_alert_danger(text)
    return(invisible(NULL))
  }

  if (!include_dirs) {
    files <- files[!is_dir(files)]
  }

  text <- set_files_text(path, files, text)

  cli::cli_alert_info(
    text = text,
    wrap = TRUE,
    .envir = .envir
  )

  style <- "file"
  show_files <- files

  if (isFALSE(show_full)) {
    show_files <- basename(files)
  }

  cli::cli_bullets(
    text = bulletize(show_files, n_show = n_show, style = style)
  )

  if (return_list) {
    return(invisible(files))
  }
}

#' @noRd
set_files_text <- function(path = NULL,
                           files = NULL,
                           text = NULL) {
  if (!is.null(text)) {
    return(text)
  }

  has_dirs <- any(is_dir(files))

  if (has_dirs) {
    text <- "{length(files)} file/folder{?s} found:"
  } else {
    text <- "{length(files)} file{?s} found:"
  }

  if (is.null(path) || (length(path) > 1)) {
    return(text)
  }

  if (has_dirs) {
    text <- "{length(files)} file/folder{?s} found at {.path {path}}:"
  } else {
    text <- "{length(files)} file{?s} found at {.path {path}}:"
  }

  text
}
