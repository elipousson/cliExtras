# Yes No with Variable Responses using cli

Adapted from `yesno::yesno()` and `usethis::ui_yeah()` to work with
[`cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html). This
function does not respect the cliExtras.quiet option and aborts if the
session is not interactive.

## Usage

``` r
cli_yesno(
  message,
  yes = c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely"),
  no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
  n_yes = 2,
  n_no = 1,
  call = .envir,
  .envir = rlang::caller_env()
)

check_yes(
  prompt = NULL,
  yes = c("", "Y", "Yes", "Yup", "Yep", "Yeah"),
  message = "Aborted. A yes is required.",
  .envir = rlang::caller_env(),
  call = .envir
)
```

## Arguments

- message:

  Passed to
  [`cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)

- yes, no:

  Character strings with yes and no options.

- n_yes, n_no:

  Number of yes and no options to provide in user prompt.

- call:

  The execution environment of a currently running function, e.g.
  `call = caller_env()`. The corresponding function call is retrieved
  and mentioned in error messages as the source of the error.

  You only need to supply `call` when throwing a condition from a helper
  function which wouldn't be relevant to mention in the message.

  Can also be `NULL` or a [defused function
  call](https://rlang.r-lib.org/reference/topic-defuse.html) to
  respectively not display any call or hard-code a code to display.

  For more information about error calls, see [Including function calls
  in error
  messages](https://rlang.r-lib.org/reference/topic-error-call.html).

- .envir:

  Environment to evaluate the glue expressions in.

- prompt:

  For `check_yes()`, the prompt is always preceded by "? " and followed
  by "(Y/n)" and padded with non-breaking spaces on both sides.

## Value

`TRUE` if yes response and `FALSE` if no response.

## Details

The yesno and usethis packages are both available under an MIT license
([yesno
LICENSE](https://github.com/poissonconsulting/yesno/blob/main/LICENSE.md)
and [usethis
LICENSE](https://github.com/r-lib/usethis/blob/main/LICENSE.md)) and are
the work of the yesno and usethis authors.
