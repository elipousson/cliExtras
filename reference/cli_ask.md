# Display a Message then Read a Line from the Terminal

Display a Message then Read a Line from the Terminal

## Usage

``` r
cli_ask(prompt = "?", ..., .envir = rlang::caller_env(), call = .envir)
```

## Arguments

- prompt:

  Characters to show as user prompt in console following displayed
  message. A non-breaking space is always placed after the prompt before
  passing to [`readline()`](https://rdrr.io/r/base/readline.html).
  Defaults to "?".

- ...:

  Arguments passed on to
  [`cli::cli_bullets`](https://cli.r-lib.org/reference/cli_bullets.html)

  `text`

  :   Character vector of items. See details below on how names are
      interpreted.

  `id`

  :   Optional id of the `div.bullets` element, can be used in themes.

  `class`

  :   Optional additional class(es) for the `div.bullets` element.

- .envir:

  Environment to evaluate the glue expressions in.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## See also

[`cli_yesno()`](https://elipousson.github.io/cliExtras/reference/cli_yesno.md)
