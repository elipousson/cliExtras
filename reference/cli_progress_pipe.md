# Simplified cli progress message for combining with a pipe

A convenience function for creating progress messages that use piped
data as an input.

## Usage

``` r
cli_progress_pipe(
  data,
  ...,
  current = TRUE,
  clear = FALSE,
  .auto_close = TRUE,
  .envir = current_env(),
  time = NULL
)
```

## Arguments

- data:

  Input data. Reference w/ "{data}" if .envir is currrent_env().

- ...:

  Arguments passed on to
  [`cli::cli_progress_bar`](https://cli.r-lib.org/reference/cli_progress_bar.html)

  `name`

  :   This is typically used as a label, and should be short, at most 20
      characters.

  `status`

  :   New status string of the progress bar, if not `NULL`.

  `type`

  :   Type of the progress bar. It is used to select a default display
      if `format` is not specified. Currently supported types:

      - `iterator`: e.g. a for loop or a mapping function,

      - `tasks`: a (typically small) number of tasks,

      - `download`: download of one file,

      - `custom`: custom type, `format` must not be `NULL` for this
        type.

  `total`

  :   Total number of progress units, or `NA` if it is unknown.
      `cli_progress_update()` can update the total number of units. This
      is handy if you don't know the size of a download at the
      beginning, and also in some other cases. If `format` is set to
      `NULL`, `format` (plus `format_done` and `format_failed`) will be
      updated when you change `total` from `NA` to a number. I.e.
      default format strings will be updated, custom ones won't be.

  `format`

  :   Format string. It has to be specified for custom progress bars,
      otherwise it is optional, and a default display is selected based
      on the progress bat type and whether the number of total units is
      known. Format strings may contain glue substitution, the support
      pluralization and cli styling. See
      [progress-variables](https://cli.r-lib.org/reference/progress-variables.html)
      for special variables that you can use in the custom format.

  `format_done`

  :   Format string for successful termination. By default the same as
      `format`.

  `format_failed`

  :   Format string for unsuccessful termination. By default the same as
      `format`.

  `auto_terminate`

  :   Whether to terminate the progress bar if the number of current
      units reaches the number of total units.

  `extra`

  :   Extra data to add to the progress bar. This can be used in custom
      format strings for example. It should be a named list.
      `cli_progress_update()` can update the extra data. Often you can
      get away with referring to local variables in the format string,
      and then you don't need to use this argument. Explicitly including
      these constants or variables in `extra` can result in cleaner
      code. In the rare cases when you need to refer to the same
      progress bar from multiple functions, and you can them to `extra`.

  `inc`

  :   Increment in progress units. This is ignored if `set` is not
      `NULL`.

  `set`

  :   Set the current number of progress units to this value. Ignored if
      `NULL`.

  `id`

  :   Progress bar to update or terminate. If `NULL`, then the current
      progress bar of the calling function (or `.envir` if specified) is
      updated or terminated.

  `force`

  :   Whether to force a display update, even if no update is due.

  `result`

  :   String to select successful or unsuccessful termination. It is
      only used if the progress bar is not cleared from the screen. It
      can be one of `"done"`, `"failed"`, `"clear"`, and `"auto"`.

- current:

  Passed to
  [`cli_progress_bar()`](https://cli.r-lib.org/reference/cli_progress_bar.html).

- clear:

  Whether to remove the progress bar from the screen after it has
  terminated. Defaults to `FALSE`
  ([`cli::cli_progress_bar()`](https://cli.r-lib.org/reference/cli_progress_bar.html)
  uses `TRUE`).

- .auto_close:

  Passed to
  [`cli_progress_bar()`](https://cli.r-lib.org/reference/cli_progress_bar.html).

- .envir:

  The environment to use for auto-termination and for glue substitution.
  It is also used to find and set the current progress bar. Defaults to
  [`current_env()`](https://rlang.r-lib.org/reference/stack.html)
  ([`cli::cli_progress_bar()`](https://cli.r-lib.org/reference/cli_progress_bar.html)
  uses [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html))

- time:

  Passed to [`Sys.sleep()`](https://rdrr.io/r/base/Sys.sleep.html) for
  optional pause after displaying progress message. Defaults to `NULL`.

## Examples

``` r
df <- data.frame("letters" = LETTERS, "numbers" = c(1:26))

df |>
  cli_progress_pipe("Data has {nrow(data)} rows and {ncol(data)} columns.") |>
  head(2)
#> Data has 26 rows and 2 columns.
#> Data has 26 rows and 2 columns.
#> 
#>   letters numbers
#> 1       A       1
#> 2       B       2
```
