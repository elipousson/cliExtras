#' List of items using bulletize helper
#'
#' @inheritParams bulletize
#' @inheritParams cli::cli_bullets
#' @rdname cli_bulletize
#' @export
#' @importFrom rlang is_named
cli_bulletize <- function(items,
                          bullet = "*",
                          n_show = Inf,
                          n_fudge = 2,
                          style = NULL,
                          sep = NULL,
                          id = NULL,
                          class = NULL,
                          .envir = parent.frame()) {
  if (!is.null(sep) && rlang::is_named(items)) {
    if (length(style) == 1) {
      style <- c(NULL, style)
    }

    items <-
      sapply(
        seq_along(items),
        function(x) {
          paste0(
            stylize(names(items)[[x]], style[1], FALSE),
            sep,
            stylize(as.character(items[[x]]), style[2], FALSE)
          )
        }
      )

    style <- NULL
  }

  quiet_cli_bullets(
    bulletize(items, bullet, n_show, n_fudge, style),
    id = id,
    class = class,
    .envir = .envir
  )
}