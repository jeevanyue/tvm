\name{find_rate}
\alias{find_rate}
\title{Find the rate for a loan given the discount factors}
\usage{
find_rate(m, d, loan_type, interval = c(1e-06, 2), tol = 1e-08)
}
\arguments{
  \item{m}{The maturity of the loan}

  \item{d}{The discount factors}

  \item{loan_type}{One of the loan types}

  \item{interval}{The interval for the root finding
  process}

  \item{tol}{The tolerance for the root finding process}
}
\description{
Thru a root finding process, this function finds the rate
that corresponds to a given set of discount factors, as to
have the same present value
}

