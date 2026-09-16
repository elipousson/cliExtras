# Use rlang to set cli.default_handler to suppressMessages as a local or permanent option

`cli_quiet()` is a helper to enable a quiet option in other functions.

## Usage

``` r
cli_quiet(quiet = FALSE, push = FALSE, .frame = caller_env())
```

## Arguments

- quiet:

  If `FALSE`, leave cli.default_handler option unchanged. If `TRUE`, set
  cli.default_handler to `suppressMessages` temporaily with
  [`rlang::local_options()`](https://rlang.r-lib.org/reference/local_options.html)
  or permanently with
  [`rlang::push_options()`](https://rlang.r-lib.org/reference/local_options.html).

- push:

  If `TRUE`, set cli.default_handler option with
  [`rlang::push_options()`](https://rlang.r-lib.org/reference/local_options.html).

- .frame:

  The environment of a stack frame which defines the scope of the
  temporary options. When the frame returns, the options are set back to
  their original values.

## Examples

``` r
test_fn <- function(quiet = FALSE) {
  cli_quiet(quiet = quiet)
  cli::cli_alert_info(
    "{.arg quiet} is {.val {quiet}}"
  )
}

options("cli.default_handler" = NULL)

test_fn()
#> ℹ `quiet` is FALSE

test_fn(quiet = TRUE)
```
