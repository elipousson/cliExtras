# cliExtras development

* Update `cli_quiet()` to work as a helper function within other functions to set the cli.default_handler option to `suppressMessages()`.
* Refactor `cli_abort_ifnot()` and similar functions to use the new `cli_if()` function.
* Refactor `cli_list_files()` including replacing message parameter with text and allowing option to pass character vector with files directly.
* Add `standalone-cliExtras.R` script that can be imported with `usethis::use_standalone()` (available in the development version of `{usethis}`).
* Adopt lifecycle to flag `cli_paths()` as a superseded function.
* Add `.envir` to standalone `cli_if()` and `cli_ifnot()` functions.

# cliExtras 0.1.0.9001 (2023-03-16)

* Update `check_yes()` to pass .envir and call parameters and to wrap prompt with non-breaking spaces.
* Add new `cli_alert_ifnot()`, `cli_alert_if()`, and related functions.

# cliExtras 0.1.0.9000 (2023-03-09)

* Add `cli_if()` and `cli_ifnot()`
* Add `cli_paths()`, `cli_list_files()`, and `cli_bulletize()`
* Add interactive `cli_menu()`, `cli_yesno()`, and `cli_ask()`
