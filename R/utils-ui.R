#' Assorted helper functions to format text and vectors for cli messages
#'
#' @description
#'
#' These functions are convenient alternatives to the internal cli functions but
#' may be removed as my understanding of the cli package improves.
#'
#' - [bracketize()] pastes open and close brackets around a vector.
#' - [stylize()] appends a style prefix, e.g. "`{.val value}`" to a vector.
#' - [bulletize()] turns a vector into a neatly abbreviated bullet list.
#'
#' [bulletize()] is adapted from the gargle package (see
#' [`utils-ui.R`](https://github.com/r-lib/gargle/blob/4021167fd2f7aca7194027bf73a5a06296ca03dc/R/utils-ui.R)).
#' This function is available under a MIT license and is the work of the gargle
#' authors.
#'
#' @name cli_helpers
#' @examples
#' bracketize("value")
#'
#' stylize("styled value", "val")
#'
#' stylize("styled variable", "val", bracket = TRUE)
#'
#' bulletize(c("val 1", "val 2", "val 3"))
#'
#' bulletize(rep("val", 20), n_show = 3)
#'
#' @param x A vector.
NULL

#' @rdname cli_helpers
#' @name bracketize
#' @param .open,.close Open and close bracket characters.
#' @inheritParams base::paste0
#' @export
bracketize <- function(..., .open = "{", .close = "}", collapse = NULL) {
  paste0(.open, ..., .close, collapse = collapse)
}

#' @rdname cli_helpers
#' @name stylize
#' @param style A cli style name, e.g. code, val, file, url
#' @param bracket If `TRUE`, pass x to bracketize.
#' @export
stylize <- function(x,
                    style = NULL,
                    bracket = FALSE,
                    .open = "{",
                    .close = "}",
                    collapse = NULL) {
  if (!all(is.character(style)) | all(style == "")) {
    return(x)
  }

  if (bracket) {
    x <- bracketize(x, .open = .open, .close = .close, collapse = collapse)
  }

  bracketize(
    ".",
    style,
    " ",
    x,
    .open = .open,
    .close = .close,
    collapse = collapse
  )
}


# Copyright (c) 2022 gargle authors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' @rdname cli_helpers
#' @name bulletize
#' @param bullet Character to use for bullet. Defaults to "".
#' @param n_show The maximum number of items to include in the bullet list.
#'   Defaults to 5.
#' @param n_fudge The minimum number of items to include in summary of additional
#'   bullet items. If the summary would only include a number of items equal or
#'   less than n_fudge, they are included in the bullet list and the summary is
#'   not displayed. Defaults to 2.
#' @param sep,before,after Additional characters or character vectors applied
#'   using `paste0(before, sep, out, after)`.
#' @export
#' @importFrom utils head
#' @importFrom cli symbol cat_bullet
bulletize <- function(x,
                      bullet = "*",
                      sep = NULL,
                      before = NULL,
                      after = NULL,
                      style = NULL,
                      n_show = 5,
                      n_fudge = 2,
                      bracket = FALSE) {
  n <- length(x)
  n_show_actual <- compute_n_show(n, n_show, n_fudge)
  out <- utils::head(x, n_show_actual)

  if (!is_all_null(list(before, sep, after))) {
    out <- paste0(before, sep, out, after)
  }

  if (!is.null(style)) {
    out <- stylize(out, style, bracket)
  }

  out <- rlang::set_names(out, rep.int(bullet, n_show_actual))

  n_not_shown <- n - n_show_actual

  if (n_not_shown == 0) {
    return(out)
  }

  c(out, c(" " = paste0(cli::symbol$ellipsis, " and ", n_not_shown, " more")))
}

#' Compute number to show with fudge
#'
#' Do not show "... and x more" if x is very small, i.e. 1 or 2.
#'
#' @source This function is adapted from the gargle package (see
#' [utils-ui.R](https://github.com/r-lib/gargle/blob/4021167fd2f7aca7194027bf73a5a06296ca03dc/R/utils-ui.R)).
#'
#' This function is available under a MIT license and is the work of the gargle
#' authors.
#'
#' @noRd
compute_n_show <- function(n, n_show_nominal = 5, n_fudge = 2) {
  if (n > n_show_nominal && n - n_show_nominal > n_fudge) {
    return(n_show_nominal)
  }

  n
}
