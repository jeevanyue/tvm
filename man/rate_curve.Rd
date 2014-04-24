\name{rate_curve}
\alias{rate_curve}
\title{Creates a rate curve instance}
\usage{
rate_curve(fun_d = NULL, fun_r = NULL, rate_type = NULL, knots)
}
\arguments{
  \item{fun_d}{A discount factor function. fun_d(x) returns
  the discount factor for time x}

  \item{fun_r}{A rate function. fun_r(x) returns the EPR
  for time x}

  \item{rate_type}{The rate type. Must be on of
  c("french","fut","german","spot","swap")}

  \item{knots}{The nodes to bootstrap the rates}
}
\description{
Creates a rate curve instance
}

