\name{disc_to_french}
\alias{disc_to_french}
\title{Converts a discount factor curve to a french loan rate curve}
\usage{
disc_to_french(disc, search_interval = c(1e-04, 1), tol = 1e-08)
}
\arguments{
  \item{disc}{The discount factor curve.}

  \item{search_interval}{A length 2 vector. The interval to
  use for the root finding algorithm. Your rates should be
  inside this interval.}

  \item{tol}{The tolerance for the root finding algorithm.}
}
\description{
Converts a discount factor curve to a french loan rate
curve
}

