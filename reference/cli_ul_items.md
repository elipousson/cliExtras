# Format a list of items as an unordered list with cli_ul()

Format a list of items as an unordered list with cli_ul()

## Usage

``` r
cli_ul_items(items, style = c("code", "val"), sep = ": ", bracket = FALSE)
```

## Arguments

- items:

  Character vector of items, or `NULL`.

- style:

  Length 2 character vector indicating cli style to use for names of
  items and style to use for items.

- sep:

  Length 1 character vector with separator between item names and names.

- bracket:

  If `TRUE`, pass x to bracketize.
