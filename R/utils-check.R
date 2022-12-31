#' Check if the current session is interactive (and abort if not)
#'
#' @noRd
check_interactive <- function(...,
                              message = "User input is required but this session is not interactive.",
                              .envir = parent.frame()) {
  cli_abort_ifnot(
    ...,
    message = message,
    .envir = .envir,
    condition = rlang::is_interactive()
  )
}


#' Check if a bullet character is a supported default value (and abort if not)
#'
#' @noRd
check_cli_bullet <- function(bullet, ...) {
  bullets <-
    c(
      "noindent",
      " " = "indent",
      "*" = "bullet",
      ">" = "arrow",
      "v" = "success",
      "x" = "danger",
      "!" = "warning",
      "i" = "info",
      ...
    )

  if (all(nchar(bullet) < 2)) {
    return(rlang::arg_match(bullet, names(bullets)))
  }

  names(bullets)[rlang::arg_match(bullet, bullets) == bullets]
}
