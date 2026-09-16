# Signal an error, warning, or message with a cli formatted message if any expressions in ... are not all TRUE or are all TRUE

Signal an error, warning, or message with a cli formatted message if any
expressions in ... are not all TRUE or are all TRUE

## Usage

``` r
cli_abort_ifnot(
  ...,
  message = NULL,
  call = .envir,
  .envir = parent.frame(),
  .frame = .envir,
  condition = NULL
)

cli_abort_if(
  ...,
  message = NULL,
  call = .envir,
  .envir = parent.frame(),
  .frame = .envir,
  condition = NULL
)

cli_warn_ifnot(..., message = NULL, .envir = parent.frame(), condition = NULL)

cli_warn_if(..., message = NULL, .envir = parent.frame(), condition = NULL)

cli_inform_ifnot(
  ...,
  message = NULL,
  .envir = parent.frame(),
  condition = NULL
)

cli_inform_if(..., message = NULL, .envir = parent.frame(), condition = NULL)
```

## Arguments

- ...:

  Any number of R expressions which should each evaluate to a logical
  vector. If `...` is named, the names will be passed to as the message
  parameter. If message is provided, any names for the logical
  expressions are ignored. If only some items from `...` are named, the
  missing names are created with
  [`rlang::exprs_auto_name()`](https://rlang.r-lib.org/reference/exprs_auto_name.html).
  If a single list is provided, the list is assumed to be a named list
  of logical values or expressions that evaluate to logical values.

- message:

  It is formatted via a call to
  [`cli_bullets()`](https://cli.r-lib.org/reference/cli_bullets.html).

- call:

  The execution environment of a currently running function, e.g.
  `call = caller_env()`. The corresponding function call is retrieved
  and mentioned in error messages as the source of the error.

  You only need to supply `call` when throwing a condition from a helper
  function which wouldn't be relevant to mention in the message.

  Can also be `NULL` or a [defused function
  call](https://rlang.r-lib.org/reference/topic-defuse.html) to
  respectively not display any call or hard-code a code to display.

  For more information about error calls, see [Including function calls
  in error
  messages](https://rlang.r-lib.org/reference/topic-error-call.html).

- .envir:

  Environment to evaluate the glue expressions in.

- .frame:

  The throwing context. Used as default for `.trace_bottom`, and to
  determine the internal package to mention in internal errors when
  `.internal` is `TRUE`.

- condition:

  Logical. For `ifnot` style functions, if `FALSE`, signal an error,
  warning, or message. For `if` style functions, signal an error,
  warning, or message if `TRUE`. Defaults to `NULL`. Ignored if multiple
  parameters are provided to `...`
