#' List of items using bulletize helper
#'
#' @param items A named vector or list to use in creating a bulletted list with
#'   [cli::cli_bullets()].
#' @param sep Separator string to insert between item name and item if items is
#'   a named list or vector. Defaults to `NULL`.
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
                          before = NULL,
                          sep = NULL,
                          after = NULL,
                          id = NULL,
                          class = NULL,
                          .envir = parent.frame()) {
  cli::cli_bullets(
    bulletize(
      items,
      bullet = bullet,
      sep = sep,
      before = before,
      after = after,
      n_show = n_show,
      n_fudge = n_fudge
    ),
    id = id,
    class = class,
    .envir = .envir
  )
}
