% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rr_register.R
\name{rr_register}
\alias{rr_register}
\title{Download a register}
\usage{
rr_register(name = NULL, phase = c("beta", "alpha", "discovery"),
  file = NULL, write = FALSE, dest_path = NULL, parse_datetimes = FALSE,
  quiet = TRUE)
}
\arguments{
\item{name}{Character, the name of the register.}

\item{phase}{Character, one of \code{"beta"}, \code{"alpha"}, and \code{"discovery"}}

\item{file}{Character, file path or URL, passed on to \code{\link[readr:read_lines]{readr::read_lines()}}
if \code{name} is not provided.}

\item{write}{Logical, whether to write the RSF file to disk.  If \code{TRUE},
either \code{name} or \code{dest_path} must be provided.}

\item{dest_path}{Character, path and file name to write the RSF to.}

\item{parse_datetimes}{Logical, whether to parse ISO8601 strings as datetimes
with \code{\link[parsedate:parse_iso_8601]{parsedate::parse_iso_8601()}}, otherwise leave as a string.  Partial
datetimes are parsed as the earliest possible datetime, e.g. \code{"2018"}
becomes \code{"2018-01-01 UTC"}.}

\item{quiet}{Logical, if \code{TRUE} does not print messages to the console.}
}
\value{
An S3 object of class \code{register}
}
\description{
Downloads a whole register (or reads it from a file) and
constructs an object that can be interrogated for its records, entries,
items, schema, links to other registers, etc.

You should probably run \code{\link[=rr_snapshot]{rr_snapshot()}} on the output before using it.
}
\examples{
rr_register("country")
rr_register("country", "beta")
path <- tempfile()
download.file("https://country.register.gov.uk/download-rsf", path)
rr_register(file = path)
unlink(path)
}
