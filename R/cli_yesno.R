# # MIT License
#
# Copyright (c) 2021 yesno authors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# # MIT License
#
# Copyright (c) 2020 usethis authors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Yes No with Variable Responses using cli
#'
#' Adapted from [yesno::yesno()] and [usethis::ui_yeah()] to work with
#' [cli_inform()]. This function does not respect the cliExtras.quiet option and
#' aborts if the session is not interactive.
#'
#' The yesno and usethis packages are both available under an MIT license
#' ([yesno
#' LICENSE](https://github.com/poissonconsulting/yesno/blob/main/LICENSE.md) and
#' [usethis LICENSE](https://github.com/r-lib/usethis/blob/main/LICENSE.md)) and
#' are the work of the yesno and usethis authors.
#'
#' @param message Passed to [cli_inform()]
#' @param yes,no Character strings with yes and no options.
#' @param n_yes,n_no Number of yes and no options to provide in user prompt.
#' @inheritParams cli::cli_abort
#' @returns `TRUE` if yes response and `FALSE` if no response.
#' @export
#' @importFrom utils menu
cli_yesno <- function(message,
                      yes = c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely"),
                      no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
                      n_yes = 2,
                      n_no = 1,
                      call = .envir,
                      .envir = parent.frame()) {
  check_interactive(
    c(
      "User input required, but session is not interactive.",
      "Query: ", message
    ),
    call = call,
    .envir = .envir
  )

  qs <- c(sample(yes, n_yes), sample(no, n_no))
  rand <- sample(length(qs))

  resp <-
    cli_menu(
      qs[rand],
      title = message,
      message = "",
      .envir = .envir
    )

  tolower(resp) %in% tolower(yes)
}

#' @name check_yes
#' @rdname cli_yesno
#' @param prompt For [check_yes()], the prompt is always preceded by "? " and
#'   followed by "(Y/n)" and padded with non-breaking spaces on both sides.
#' @export
check_yes <- function(prompt = NULL,
                      yes = c("", "Y", "Yes", "Yup", "Yep", "Yeah"),
                      message = "Aborted. A yes is required.",
                      .envir = parent.frame(),
                      call = .envir) {
  resp <- cli_ask(paste0("?\u00a0", prompt, "\u00a0(Y/n)"), .envir = .envir)

  cli_abort_ifnot(
    message = message,
    condition = tolower(resp) %in% tolower(yes),
    .envir = .envir,
    call = call
  )
}
