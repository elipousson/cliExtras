# Assorted helper functions to format text and vectors for cli messages

These functions are convenient alternatives to the internal cli
functions but may be removed as my understanding of the cli package
improves.

- `bracketize()` pastes open and close brackets around a vector.

- `stylize()` appends a style prefix, e.g. "`{.val value}`" to a vector.

- `bulletize()` turns a vector into a neatly abbreviated bullet list.

`bulletize()` is adapted from the gargle package (see
[`utils-ui.R`](https://github.com/r-lib/gargle/blob/4021167fd2f7aca7194027bf73a5a06296ca03dc/R/utils-ui.R)).
This function is available under a MIT license and is the work of the
gargle authors.

## Usage

``` r
bracketize(..., .open = "{", .close = "}", collapse = NULL)

stylize(
  x,
  style = NULL,
  bracket = FALSE,
  .open = "{",
  .close = "}",
  collapse = NULL
)

bulletize(
  x,
  bullet = "*",
  sep = NULL,
  before = NULL,
  after = NULL,
  style = NULL,
  n_show = 5,
  n_fudge = 2,
  bracket = FALSE
)
```

## Arguments

- ...:

  one or more R objects, to be converted to character vectors.

- .open, .close:

  Open and close bracket characters.

- collapse:

  an optional character string to separate the results. Not
  [`NA_character_`](https://rdrr.io/r/base/NA.html). When `collapse` is
  a string, the result is always a string
  ([`character`](https://rdrr.io/r/base/character.html) of length 1).

- x:

  A vector.

- style:

  A cli style name, e.g. code, val, file, url

- bracket:

  If `TRUE`, pass x to bracketize.

- bullet:

  Character to use for bullet. Defaults to "".

- sep, before, after:

  Additional characters or character vectors applied using
  `paste0(before, sep, out, after)`.

- n_show:

  The maximum number of items to include in the bullet list. Defaults to
  5.

- n_fudge:

  The minimum number of items to include in summary of additional bullet
  items. If the summary would only include a number of items equal or
  less than n_fudge, they are included in the bullet list and the
  summary is not displayed. Defaults to 2.

## Examples

``` r
bracketize("value")
#> [1] "{value}"

stylize("styled value", "val")
#> [1] "{.val styled value}"

stylize("styled variable", "val", bracket = TRUE)
#> [1] "{.val {styled variable}}"

bulletize(c("val 1", "val 2", "val 3"))
#>       *       *       * 
#> "val 1" "val 2" "val 3" 

bulletize(rep("val", 20), n_show = 3)
#>               *               *               *                 
#>           "val"           "val"           "val" "… and 17 more" 
```
