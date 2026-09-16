# Set the cli.default_handler and cliExtras.quiet options

**\[deprecated\]**

## Usage

``` r
set_cli_quiet(quiet = FALSE, msg = !quiet)
```

## Arguments

- quiet:

  If `TRUE` set the cli.default_handler option to `suppressMessages` and
  cliExtras.quiet option to `TRUE`. Defaults to `FALSE`.

- msg:

  If `TRUE`, `set_cli_quiet()` displays a message confirming the option
  changes. If `FALSE`, the function does not display a message. Defaults
  to `!quiet`
