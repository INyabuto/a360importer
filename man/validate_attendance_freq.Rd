% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation.R
\name{validate_attendance_freq}
\alias{validate_attendance_freq}
\title{Validate MMA attendance freq.}
\usage{
validate_attendance_freq(x = NULL)
}
\arguments{
\item{x}{A vector of character or string}
}
\value{
An existing frequency in DHIS2
}
\description{
\code{validate_attendance_freq} validates the different frequencies provided in MMA attendance sheets
against the current set up in NG RH A360 - 9ja and MMA events program.
}
