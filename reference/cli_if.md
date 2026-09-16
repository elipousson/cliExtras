# Execute a cli function if a predicate function returns TRUE

Execute a function if a predicate function returns TRUE. Intended for
use with cli functions.

## Usage

``` r
cli_if(
  x = NULL,
  ...,
  .predicate = is_true,
  .fn = NULL,
  .default = cli_alert,
  call = caller_env()
)

cli_ifnot(
  x = NULL,
  ...,
  .predicate = is_false,
  .fn = NULL,
  .default = cli_alert,
  call = caller_env()
)
```

## Arguments

- x:

  Parameter to passed to .predicate function, Default: `NULL`

- ...:

  Additional parameters passed to .fn.

- .predicate:

  Single parameter predicate function, Defaults to
  [`rlang::is_true`](https://rlang.r-lib.org/reference/is_true.html) for
  `cli_if()` or
  [`rlang::is_false`](https://rlang.r-lib.org/reference/is_true.html)
  for `cli_ifnot()`. If .predicate returns `TRUE`, execute .fn. Aborts
  if .predicate does not return a boolean value.

- .fn:

  Function to call with ... parameters if x, Default: `NULL`

- .default:

  Default function to execute when .predicate function returns `TRUE`,
  Default:
  [`cli::cli_alert`](https://cli.r-lib.org/reference/cli_alert.html)

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

The output from the .fn function or .default if .fn is `NULL`

## Examples

``` r
cli_if(FALSE, "No alert.")

cli_if(TRUE, "Alert on TRUE!")
#> → Alert on TRUE!

cli_ifnot(FALSE, "Alert on FALSE!")
#> → Alert on FALSE!
```
