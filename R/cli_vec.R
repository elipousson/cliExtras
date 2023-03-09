#' Helper to add custom cli style to a vector with vec_last as a parameter
#'
#' @param vec_last Used as value for "vec-last" item in style object.
#' @inheritParams cli::cli_vec
#' @keywords internal
#' @export
#' @importFrom cli cli_vec
cli_vec_last <- function(x, style = list(), vec_last = " or ") {
  cli::cli_vec(
    x,
    style = c(
      style,
      "vec-last" = vec_last
    )
  )
}

#' @rdname cli_vec_last
#' @name cls_vec
#' @keywords internal
#' @export
cls_vec <- function(x, vec_last = "or") {
  cli_vec_last(
    x,
    style = list(
      before = "<",
      after = ">",
      color = "blue"
    ),
    vec_last = vec_last
  )
}
