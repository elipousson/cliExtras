% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cli_alert_ifnot.R
\name{cli_alert_ifnot}
\alias{cli_alert_ifnot}
\alias{cli_danger_ifnot}
\alias{cli_info_ifnot}
\alias{cli_success_ifnot}
\alias{cli_warning_ifnot}
\alias{cli_alert_if}
\alias{cli_danger_if}
\alias{cli_info_if}
\alias{cli_success_if}
\alias{cli_warning_if}
\title{CLI conditional alerts}
\usage{
cli_alert_ifnot(
  text = NULL,
  condition = NULL,
  .fn = cli::cli_alert,
  id = NULL,
  class = NULL,
  wrap = TRUE,
  .envir = parent.frame(),
  call = parent.frame()
)

cli_danger_ifnot(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_info_ifnot(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_success_ifnot(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_warning_ifnot(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_alert_if(
  text = NULL,
  condition = NULL,
  .fn = cli::cli_alert,
  id = NULL,
  class = NULL,
  wrap = TRUE,
  .envir = parent.frame(),
  call = parent.frame()
)

cli_danger_if(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_info_if(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_success_if(text = NULL, condition = NULL, ..., .envir = parent.frame())

cli_warning_if(text = NULL, condition = NULL, ..., .envir = parent.frame())
}
\arguments{
\item{text}{Text of the alert.}

\item{condition}{If \code{TRUE}, display alert for "if" functions. If \code{FALSE},
display alert for "ifnot" functions.}

\item{.fn}{cli function to use for alert. Defaults to \code{cli::cli_alert}.
Supported options also include "danger", "info", "success", and "warning".}

\item{id}{Id of the alert element. Can be used in themes.}

\item{class}{Class of the alert element. Can be used in themes.}

\item{wrap}{Whether to auto-wrap the text of the alert.}

\item{.envir}{Environment to evaluate the glue expressions in.}

\item{call}{Caller environment. Used to improve error messages for argument
checks.}

\item{...}{Additional parameters passed to cli_alert_if or cli_alert_ifnot by
functions like \code{cli_info_ifnot()}.}
}
\description{
Alerts are typically short status messages. Note: These functions use \code{wrap = TRUE} by default. Alert messages can be muffled by \code{\link[=set_cli_quiet]{set_cli_quiet()}} while
\code{\link[cli:cli_abort]{cli::cli_inform()}} messages are not.
}
\examples{
\dontrun{
if (interactive()) {
  cli_alert_if(text = "Example text", condition = TRUE)

  cli_alert_ifnot(text = "Example text", condition = FALSE)

  cli_alert_success_if(text = "Success!", condition = TRUE)

  cli_alert_danger_ifnot(text = "Danger!", condition = FALSE)
}
}
}
\seealso{
\code{\link[cli:cli_alert]{cli::cli_alert()}}
}
