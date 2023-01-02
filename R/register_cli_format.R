#' Register a custom cli format method with registerS3method
#'
#' @param class,method Object class and format function passed to
#'   [registerS3method()].
#' @param envir Environment where the S3 method is registered. Defaults to
#'   parent.frame().
#' @export
#' @importFrom cli cli_format
register_cli_format <- function(class = NULL,
                                method = NULL,
                                envir = parent.frame()) {
  registerS3method("cli_format", class = class, method = method, envir = envir)
}
