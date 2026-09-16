# Register a custom cli format method with registerS3method

Register a custom cli format method with registerS3method

## Usage

``` r
register_cli_format(class = NULL, method = NULL, envir = parent.frame())
```

## Arguments

- class, method:

  Object class and format function passed to
  [`registerS3method()`](https://rdrr.io/r/base/ns-internal.html).

- envir:

  Environment where the S3 method is registered. Defaults to
  parent.frame().
