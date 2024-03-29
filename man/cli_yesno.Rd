% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cli_yesno.R
\name{cli_yesno}
\alias{cli_yesno}
\alias{check_yes}
\title{Yes No with Variable Responses using cli}
\usage{
cli_yesno(
  message,
  yes = c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely"),
  no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
  n_yes = 2,
  n_no = 1,
  call = .envir,
  .envir = rlang::caller_env()
)

check_yes(
  prompt = NULL,
  yes = c("", "Y", "Yes", "Yup", "Yep", "Yeah"),
  message = "Aborted. A yes is required.",
  .envir = rlang::caller_env(),
  call = .envir
)
}
\arguments{
\item{message}{Passed to \code{\link[=cli_inform]{cli_inform()}}}

\item{yes, no}{Character strings with yes and no options.}

\item{n_yes, n_no}{Number of yes and no options to provide in user prompt.}

\item{call}{The execution environment of a currently running
function, e.g. \code{call = caller_env()}. The corresponding function
call is retrieved and mentioned in error messages as the source
of the error.

You only need to supply \code{call} when throwing a condition from a
helper function which wouldn't be relevant to mention in the
message.

Can also be \code{NULL} or a \link[rlang:topic-defuse]{defused function call} to
respectively not display any call or hard-code a code to display.

For more information about error calls, see \ifelse{html}{\link[rlang:topic-error-call]{Including function calls in error messages}}{\link[rlang:topic-error-call]{Including function calls in error messages}}.}

\item{.envir}{Environment to evaluate the glue expressions in.}

\item{prompt}{For \code{\link[=check_yes]{check_yes()}}, the prompt is always preceded by "? " and
followed by "(Y/n)" and padded with non-breaking spaces on both sides.}
}
\value{
\code{TRUE} if yes response and \code{FALSE} if no response.
}
\description{
Adapted from \code{\link[yesno:yesno]{yesno::yesno()}} and \code{\link[usethis:ui-questions]{usethis::ui_yeah()}} to work with
\code{\link[=cli_inform]{cli_inform()}}. This function does not respect the cliExtras.quiet option and
aborts if the session is not interactive.
}
\details{
The yesno and usethis packages are both available under an MIT license
(\href{https://github.com/poissonconsulting/yesno/blob/main/LICENSE.md}{yesno LICENSE} and
\href{https://github.com/r-lib/usethis/blob/main/LICENSE.md}{usethis LICENSE}) and
are the work of the yesno and usethis authors.
}
