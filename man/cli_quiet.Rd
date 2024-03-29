% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cli_quiet.R
\name{cli_quiet}
\alias{cli_quiet}
\title{Use rlang to set cli.default_handler to suppressMessages as a local or
permanent option}
\usage{
cli_quiet(quiet = FALSE, push = FALSE, .frame = rlang::caller_env())
}
\arguments{
\item{quiet}{If \code{FALSE}, leave cli.default_handler option unchanged. If
\code{TRUE}, set cli.default_handler to \code{suppressMessages} temporaily with
\code{\link[rlang:local_options]{rlang::local_options()}} or permanently with \code{\link[rlang:local_options]{rlang::push_options()}}.}

\item{push}{If \code{TRUE}, set cli.default_handler option with
\code{\link[rlang:local_options]{rlang::push_options()}}.}

\item{.frame}{The environment of a stack frame which defines the
scope of the temporary options. When the frame returns, the
options are set back to their original values.}
}
\description{
\code{\link[=cli_quiet]{cli_quiet()}} is a helper to enable a quiet option in other functions.
}
\examples{
test_fn <- function(quiet = FALSE) {
  cli_quiet(quiet = quiet)
  cli::cli_alert_info(
    "{.arg quiet} is {.val {quiet}}"
  )
}

options("cli.default_handler" = NULL)

test_fn()

test_fn(quiet = TRUE)

}
