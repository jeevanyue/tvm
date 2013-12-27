\name{valueConstCf}
\alias{valueConstCf}
\title{Present value of a constant cashflow}
\usage{
valueConstCf(f, maturity, fd)
}
\arguments{
  \item{f}{The periodic and constant cashflow}

  \item{maturity}{The number of periods it repeats}

  \item{fd}{The discount factors. f[x] is used to discount
  the cashflow in the x period}
}
\description{
Discounted value of a constant cashflow that ocurrs every
period from here to maturity
}

