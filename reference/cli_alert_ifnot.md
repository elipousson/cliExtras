# CLI conditional alerts

Alerts are typically short status messages. Note: These functions use
`wrap = TRUE` by default. Alert messages can be muffled by
[`set_cli_quiet()`](https://elipousson.github.io/cliExtras/reference/set_cli_quiet.md)
while
[`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)
messages are not.

## Usage

``` r
cli_alert_ifnot(
  text = NULL,
  condition = NULL,
  .fn = cli::cli_alert,
  id = NULL,
  class = NULL,
  wrap = TRUE,
  .envir = parent.frame(),
  call = parent.frame()
)

cli_danger_ifnot(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_info_ifnot(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_success_ifnot(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_warning_ifnot(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_alert_if(
  text = NULL,
  condition = NULL,
  .fn = cli::cli_alert,
  id = NULL,
  class = NULL,
  wrap = TRUE,
  .envir = parent.frame(),
  call = parent.frame()
)

cli_danger_if(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_info_if(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_success_if(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_warning_if(text = NULL, condition = NULL, ..., .envir = parent.frame())
```

## Arguments

- text:

  Text of the alert.

- condition:

  If `TRUE`, display alert for "if" functions. If `FALSE`, display alert
  for "ifnot" functions.

- .fn:

  cli function to use for alert. Defaults to
  [`cli::cli_alert`](https://cli.r-lib.org/reference/cli_alert.html).
  Supported options also include "danger", "info", "success", and
  "warning".

- id:

  Id of the alert element. Can be used in themes.

- class:

  Class of the alert element. Can be used in themes.

- wrap:

  Whether to auto-wrap the text of the alert.

- .envir:

  Environment to evaluate the glue expressions in.

- call:

  Caller environment. Used to improve error messages for argument
  checks.

- ...:

  Additional parameters passed to cli_alert_if or cli_alert_ifnot by
  functions like `cli_info_ifnot()`.

## See also

[`cli::cli_alert()`](https://cli.r-lib.org/reference/cli_alert.html)

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive()) {
  cli_alert_if(text = "Example text", condition = TRUE)

  cli_alert_ifnot(text = "Example text", condition = FALSE)

  cli_alert_success_if(text = "Success!", condition = TRUE)

  cli_alert_danger_ifnot(text = "Danger!", condition = FALSE)
}
} # }
```
