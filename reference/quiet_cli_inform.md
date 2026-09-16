# Quiet version of [`cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)

The implementation of the cliExtras.quiet option is based on the
implementation of a similar setting in googlesheets4, googledrive, and
gargle. If the cliExtras.quiet option is set to `TRUE`,
`quiet_cli_inform()` does not trigger a message. It is used by
[`cli_inform_ifnot()`](https://elipousson.github.io/cliExtras/reference/cli_abort_ifnot.md)
and
[`cli_inform_if()`](https://elipousson.github.io/cliExtras/reference/cli_abort_ifnot.md)
but not by most other functions in cliExtras.

## Usage

``` r
quiet_cli_inform(..., .envir = parent.frame())
```

## Arguments

- ...:

  Additional parameters passed to
  [`cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)

- .envir:

  Environment to evaluate the glue expressions in.
