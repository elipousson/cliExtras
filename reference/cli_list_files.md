# Display a list of files as a list of items

Display a list of files as a list of items

## Usage

``` r
cli_list_files(
  path = NULL,
  files = NULL,
  pattern = NULL,
  text = NULL,
  bullet = "*",
  n_show = 10,
  show_full = FALSE,
  include_dirs = TRUE,
  return_list = FALSE,
  .envir = current_env(),
  ...
)
```

## Arguments

- path:

  a character vector of full path names; the default corresponds to the
  working directory, [`getwd()`](https://rdrr.io/r/base/getwd.html).
  Tilde expansion (see
  [`path.expand`](https://rdrr.io/r/base/path.expand.html)) is
  performed. Missing values will be ignored. Elements with a marked
  encoding will be converted to the native encoding (and if that fails,
  considered non-existent).

- files:

  List to file names to display. Ignored if path is provided. If path is
  a vector of existing files, path is used as files. If files share a
  single directory, path is set to that directory, otherwise path is set
  to `NULL`.

- pattern:

  an optional [regular expression](https://rdrr.io/r/base/regex.html).
  Only file names which match the regular expression will be returned.

- text:

  Passed to
  [`cli::cli_alert_info()`](https://cli.r-lib.org/reference/cli_alert.html).
  If `NULL` (default), text appears reporting the number of
  files/folders found at the path (if path provided).

- bullet:

  Character defining style to use list of file names.

- n_show:

  Number of file names to show in list. The remaining number of files
  n_show are noted at the end of the list but the file names are not
  displayed. Defaults to 10.

- show_full:

  If `TRUE`, always show the full file path available. If `FALSE`, show
  just the base name. Set to `FALSE` automatically if path is a vector
  of file names is a single directory.

- include_dirs:

  If `TRUE`, include directories in listed files. Defaults to `FALSE`.
  Passed to the include.dirs parameter of
  [`base::list.files()`](https://rdrr.io/r/base/list.files.html) if path
  is a directory.

- return_list:

  If `TRUE`, return the list of files after displaying the cli message.
  Defaults to `FALSE`.

- .envir:

  Passed to
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html).
  Defaults to
  [`rlang::current_env()`](https://rlang.r-lib.org/reference/stack.html)
  rather than [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)
  to support evaluation of default message that includes the number of
  files found at the path.

- ...:

  Arguments passed on to
  [`base::list.files`](https://rdrr.io/r/base/list.files.html)

  `all.files`

  :   a logical value. If `FALSE`, only the names of visible files are
      returned (following Unix-style visibility, that is files whose
      name does not start with a dot). If `TRUE`, all file names will be
      returned.

  `full.names`

  :   a logical value. If `TRUE`, the directory path is prepended to the
      file names to give a relative file path. If `FALSE`, the file
      names (rather than paths) are returned.

  `recursive`

  :   logical. Should the listing recurse into directories?

  `ignore.case`

  :   logical. Should pattern-matching be case-insensitive?

  `include.dirs`

  :   logical. Should subdirectory names be included in recursive
      listings? (They always are in non-recursive ones).

  `no..`

  :   logical. Should both `"."` and `".."` be excluded also from
      non-recursive listings?

## See also

[`cli::cli_bullets()`](https://cli.r-lib.org/reference/cli_bullets.html)

## Examples

``` r
cli_list_files(system.file("R", package = "cliExtras"), n_show = 5)
#> ℹ 3 files found at /home/runner/work/_temp/Library/cliExtras/R:
#> • cliExtras
#> • cliExtras.rdb
#> • cliExtras.rdx
```
