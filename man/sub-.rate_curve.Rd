\name{[.rate_curve}
\alias{[.rate_curve}
\title{Returns a particular rate or rates from a curve}
\usage{
\method{[}{rate_curve}(r, x = NULL, rate_type = "spot")
}
\arguments{
  \item{r}{The rate_curve object}

  \item{x}{The points in time to return}

  \item{rate_type}{The rate type}
}
\value{
If \code{x} is \code{NULL}, then returns a rate function of
\code{rate_type} type. Else, it returns the rates of
\code{rate_type} type and corresponding to time \code{x}
}
\description{
Returns a particular rate or rates from a curve
}

