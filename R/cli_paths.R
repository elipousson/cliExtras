#' Display a list of file paths
#'
#' `r lifecycle::badge('superseded')`
#'
#' Use [cli_list_files()] instead.
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
  lifecycle::deprecate_warn(
    when = "0.1.0",
    what = "cli_paths()",
    details = "Please use `cliExtras::cli_list_files()` instead."
  )

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
