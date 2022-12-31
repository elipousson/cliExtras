#' Format a list of items as an unordered list with cli_ul()
#'
#' @rdname cli_ul_items
#' @inheritParams cli::cli_li
#' @param style Length 2 character vector indicating cli style to use for names
#'   of items and style to use for items.
#' @param sep Length 1 character vector with separator between item names and
#'   names.
#' @inheritParams stylize
#' @export
#' @importFrom cli cli_ul cli_li cli_end
cli_ul_items <- function(items,
                         style = c("code", "val"),
                         sep = ": ",
                         bracket = FALSE) {
  quiet <- cli_quiet()
  if (!is.na(quiet) && rlang::is_true(quiet)) {
    return(invisible())
  }

  cli::cli_ul(.close = FALSE)
  sapply(
    seq_along(items),
    function(x) {
      cli::cli_li(
        paste0(
          stylize(names(items)[[x]], style[1], bracket),
          sep,
          stylize(as.character(items[[x]]), style[2], bracket)
        )
      )
    }
  )
  cli::cli_end()
}
