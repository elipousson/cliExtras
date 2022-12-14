% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cli_quiet.R
\name{quiet_cli_inform}
\alias{quiet_cli_inform}
\title{Quiet version of \code{\link[=cli_inform]{cli_inform()}}}
\usage{
quiet_cli_inform(..., .envir = parent.frame())
}
\arguments{
\item{...}{Additional parameters passed to \code{\link[=cli_inform]{cli_inform()}}}

\item{.envir}{Environment to evaluate the glue expressions in.}
}
\description{
The implementation of the cliExtras.quiet option is based on the
implementation of a similar setting in googlesheets4, googledrive, and
gargle. If the cliExtras.quiet option is set to \code{TRUE}, \code{\link[=quiet_cli_inform]{quiet_cli_inform()}}
does not trigger a message. It is used by \code{\link[=cli_inform_ifnot]{cli_inform_ifnot()}} and
\code{\link[=cli_inform_if]{cli_inform_if()}} but not by most other functions in cliExtras.
}
