#' Adjusts the discount factors by a spread
#' 
#' @param fd vector of discount factors used to discount cashflows in \code{1:length(fd)} periods
#' @param effective spread
adjust_disc <- function(fd,spread) {
  zeros <- (1/fd)^(1/seq(along.with=fd))
  zeros_adj <- zeros + spread
  1/(zeros_adj^(seq(along.with=zerosAdj)))
}

#' Calculates the Total Financial Cost (CFT)
#' 
#' This is the IRR of the loan's cashflow, after adding all the extra costs
#' 
#' It is assumed that the loan has monthly payments
#' The CFT is returned as an effective rate of periodicty equal to that of the maturity and the rate
#' The interest is calculated over amt + fee
#' 
#' @param amt The amount of the loan
#' @param maturity
#' @param rate The loan rate, in  effective rate
#' @param upFee The fee that the loan taker pays upfront
#' @param perFee The fee that the loan payer pays every period
cft <- function(amt, maturity, rate, up_fee = 0, per_fee = 0) {
  full_amt <- amt + up_fee
  p <- pmt(amt = full_amt, maturity = maturity, rate = rate)
  rate(amt = amt, maturity = maturity, pmt = p + per_fee)
}

#' Net Present Value of a cashflow (NPV)
#' 
#' @param i The rate used to discount the cashflow. It must be effective and with a periodicity that matches that of the cashflow
#' @param cf The cashflow
#' @param t The times on which the cashflow ocurrs. It is assumed that cf[idx] happens at moment t[idx]
npv <- function(i, cf, t=seq(from=0,by=1,along.with=cf)) sum(cf/(1+i)^t)

#' Internal Rate of Return of a cashflow (IRR)
#' 
#' The IRR is returned as an effective rate with periodicity equal to that of the cashflow
#' 
#' @param cf The cashflow
#' @param t The times on which the cashflow ocurrs. It is assumed that cf[idx] happens at moment t[idx]
irr <- function(cf, t=seq(from=0,by=1,along.with=cf)) { uniroot(npv, c(0,100000), cf=cf, t=t)$root }

#' The value of the payment of a loan with constant payments (french type amortization)
#' 
#' The periodicity of the maturity and the rate must match, and this will be the periodicity of the payments
#'
#' @param amt The amount of the loan
#' @param maturity The maturity of the loan
#' @param rate The rate of the loan
pmt <- function(amt, maturity, rate) {  
  return(amt*tasa/(1-(1+rate)^(-maturity)))
}

#' The rate of a loan with constant payments (french type amortization)
#' 
#' The periodicity of the maturity and the payment must match, and this will be the periodicity of the rate (which is returned as an effective rate)
#' 
#' @param amt The amount of the loan
#' @param maturity The maturity of the loan
#' @param pmt The payments of the loan
rate <- function(amt, maturity, pmt, extrema=c(1e-4,1e9), tol=1e-4) {   
  zerome <- function(r) amt/pmt-(1-1/(1+r)^maturity)/r
  if(zerome(extrema[1])>0) return(0)
  if(zerome(extrema[2])<0) return(extrema[2])
  return(uniroot(zerome, interval=extrema, tol=tol)$root)
}

rate <- Vectorize(FUN=rate,vectorize.args=c("amt","maturity","pmt"))

#' Cashflow for a bullet loan
#' 
#' A bullet loan pays interest periodically and returns its principal at maturity
#' 
#' @param rate The rate of the loan, as an effective periodic rate
#' @param maturity The maturity of the loan
bulletCashflow <- function(rate,maturity) {
  f <- rep(rate,times=maturity)
  f[maturity] <- 1 + f[maturity]
  f
}

#' Cashflow for a zero loan
#' 
#' A zero loan makes an only payment, paying everything at maturity
#' 
#' @param rate The rate of the loan, as an effective rate with periodicity equal to the periodicity of the units of the maturity
#' @param maturity The maturity of the loan
zeroCashflow <- function(rate,maturity) {
  f <- rep(0,times=maturity)
  f[maturity] <- (1 + tasa)^maturity
  f
}

#' Cashflow for a german loan
#' 
#' A german loan has constant amortizations
#' 
#' @param rate The rate of the loan, as an effective periodic rate
#' @param maturity The maturity of the loan
germanCashflow <- function(rate,maturity,grace_amt = 0,grace_int = 0) {
  stopifnot(grace_int <= grace_amt)
  stopifnot(grace_amt < maturity)
  if (grace_int > 0) {
    return (c(
      rep(0,times=grace_int),
      (1+rate)^grace_int*germanCashflow(rate=rate,maturity=maturity-grace_int,grace_amt = grace_amt-grace_int,grace_int=0)))
  } else {
    p <- maturity - grace_amt
    k <- c(rep(x=0,times=grace_amt),rep(1/p,times=p))
    krem <- c(1,1-cumsum(k))
    i <- krem[-maturity]*rate
    return (k+i)
  }
}

#' Cashflow for a french loan
#' 
#' A french loan has a constant payment
#' 
#' @param rate The rate of the loan, as an effective periodic rate
#' @param maturity The maturity of the loan
frenchCashflow <- function(rate,maturity,grace_amt = 0,grace_int = 0) {
  stopifnot(grace_int <= grace_amt)
  stopifnot(grace_amt < maturity)
  if (graceInt > 0) {
    return (c(
      rep(0,times=grace_int),
      (1+rate)^grace_int*frenchCashflow(rate=rate,maturity=maturity-grace_int,grace_amt = grace_amt-grace_int,grace_int=0)))
  } else {
    p <- maturity - grace_amt
    c(rep(rate,times=grace_amt),rep(rate / (1 - (1+rate)^(-p)),times=p))
  }
}

#' Value of a discounted cashflow
#' 
#' @param disc The discount factor curve
#' @param cf The cashflow
disc_cf <- function(disc,cf) {
  sum(disc*cf)
}

#' Remaining capital in a loan
#' 
#' The amount that has to be repayed at each moment in a loan, at the end of the period
#' 
#' @param cf The cashflow of the loan
#' @param amt The original amount of the loan
#' @param r The periodic rate of the loan
rem <- function(cf,amt,r) {
  s <- function(t) amt*(1+r)^t-sum(cf[1:t]*(1+r)^(t-(1:t)))
  vapply(X=seq_along(cf),FUN=s,FUN.VALUE=1)
}