\name{cft}
\alias{cft}
\title{Calculates the Total Financial Cost (CFT)}
\usage{
cft(amt, maturity, rate, up_fee = 0, per_fee = 0)
}
\arguments{
  \item{amt}{The amount of the loan}

  \item{maturity}{}

  \item{rate}{The loan rate, in effective rate}

  \item{upFee}{The fee that the loan taker pays upfront}

  \item{perFee}{The fee that the loan payer pays every
  period}
}
\description{
This is the IRR of the loan's cashflow, after adding all
the extra costs
}
\details{
It is assumed that the loan has monthly payments The CFT is
returned as an effective rate of periodicty equal to that
of the maturity and the rate The interest is calculated
over amt + fee
}

