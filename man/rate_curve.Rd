\name{rate_curve}
\alias{rate_curve}
\title{Creates a rate curve instance}
\usage{
rate_curve(rates = NULL, rate_type = "spot", pers = 1:length(rates),
  fun_d = NULL, fun_r = NULL, knots = 1:length(rates))
}
\arguments{
  \item{rates}{A rate vector}

  \item{rate_type}{The rate type. Must be on of
  c("french","fut","german","spot","swap")}

  \item{pers}{The periods the rates correspond to}

  \item{fun_d}{A discount factor function. fun_d(x) returns
  the discount factor for time x}

  \item{fun_r}{A rate function. fun_r(x) returns the EPR
  for time x}

  \item{knots}{The nodes used to bootstrap the rates}
}
\description{
Creates a rate curve instance
}

