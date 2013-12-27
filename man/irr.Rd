\name{irr}
\alias{irr}
\title{Internal Rate of Return of a cashflow (IRR)}
\usage{
irr(cf, t = seq(from = 0, by = 1, along.with = cf))
}
\arguments{
  \item{cf}{The cashflow}

  \item{t}{The times on which the cashflow ocurrs. It is
  assumed that cf[idx] happens at moment t[idx]}
}
\description{
The IRR is returned as an effective rate with periodicity
equal to that of the cashflow
}

