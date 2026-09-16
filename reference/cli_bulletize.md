# List of items using bulletize helper

List of items using bulletize helper

## Usage

``` r
cli_bulletize(
  items,
  bullet = "*",
  n_show = Inf,
  n_fudge = 2,
  style = NULL,
  sep = NULL,
  before = NULL,
  after = NULL,
  id = NULL,
  class = NULL,
  .envir = parent.frame()
)
```

## Arguments

- items:

  A named vector or list to use in creating a bulletted list with
  [`cli::cli_bullets()`](https://cli.r-lib.org/reference/cli_bullets.html).

- bullet:

  Character to use for bullet. Defaults to "".

- n_show:

  The maximum number of items to include in the bullet list. Defaults to
  5.

- n_fudge:

  The minimum number of items to include in summary of additional bullet
  items. If the summary would only include a number of items equal or
  less than n_fudge, they are included in the bullet list and the
  summary is not displayed. Defaults to 2.

- style:

  A cli style name, e.g. code, val, file, url

- sep, before, after:

  Additional characters or character vectors applied using
  `paste0(before, sep, out, after)`. Defaults to `NULL`.

- id:

  Optional id of the `div.bullets` element, can be used in themes.

- class:

  Optional additional class(es) for the `div.bullets` element.

- .envir:

  Environment to evaluate the glue expressions in.
