# Helper to add custom cli style to a vector with vec_last as a parameter

Helper to add custom cli style to a vector with vec_last as a parameter

## Usage

``` r
cli_vec_last(x, style = list(), vec_last = " or ")

cls_vec(x, vec_last = " or ")
```

## Arguments

- x:

  Vector that will be collapsed by cli.

- style:

  Style to apply to the vector. It is used as a theme on a `span`
  element that is created for the vector. You can set `vec-sep`,
  `vec-sep2`, and `vec-last` to modify the general separator, the 2-item
  separator, and the last separator.

- vec_last:

  Used as value for "vec-last" item in style object.
