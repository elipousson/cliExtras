# Display a Message then Read a Line from the Terminal

Display a Message then Read a Line from the Terminal

## Usage

``` r
cli_menu(
  choices,
  title = NULL,
  message = "Enter your selection or press {.kbd 0} to exit.",
  prompt = "Selection:",
  exit = 0,
  ind = FALSE,
  id = NULL,
  call = .envir,
  .envir = parent.frame()
)
```

## Arguments

- choices:

  Required vector or list of choices.

- title:

  Title for menu. Character vector passed to
  [`cli::cli_h2()`](https://cli.r-lib.org/reference/cli_h1.html) if
  title is length 1 or to
  [`cli::cli_bullets()`](https://cli.r-lib.org/reference/cli_bullets.html)
  if length is greater than 1.

- message:

  Additional message to display after choices and before prompt.

- prompt:

  Characters to show as user prompt in console following displayed menu
  options. Defaults to "Selection:" (matching
  [`utils::menu()`](https://rdrr.io/r/utils/menu.html))

- exit:

  Character to use to exit menu.

- ind:

  If `TRUE`, return index position, if `FALSE` (default), return item
  from choices.

- id:

  Optional id of the `div.bullets` element, can be used in themes.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- .envir:

  Environment to evaluate the glue expressions in.

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive()) {
  cli_menu(list("A", "B", "C"), title = "Pick a letter?")

  cli_menu(c("A", "B", "C"), labels = "Alpha")

  cli_menu(list(list(1, 2, 3), "A", "B", "C"), title = "Pick from a list")
}
} # }
```
