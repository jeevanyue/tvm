\name{getRate}
\alias{getRate}
\title{The rate of a loan with constant payments (french type amortization)}
\usage{
getRate(amt, maturity, paymt, extrema = c(1e-04, 1e+09), tol = 1e-04)
}
\arguments{
  \item{amt}{The amount of the loan}

  \item{maturity}{The maturity of the loan}

  \item{paymt}{The payments of the loan}
}
\description{
The periodicity of the maturity and the payment must match,
and this will be the periodicity of the rate (which is
returned as an effective rate)
}

