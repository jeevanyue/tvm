#' Converts a futures curve to a spot curve
#' 
#' @param fut The futures curve.
#' @export
fut_to_spot <- function(fut) {
  (cumprod(1+fut))^(1/(seq_along(fut)))-1
}

#' Converts a discount factor curve to a swap rate curve
#' 
#' @param disc The discount curve.
#' @export
disc_to_swap <- function(disc) {
  (1-disc) / cumsum(disc)
}

#' Converts a swap curve to a discount factor curve
#' 
#' @param swap The swap curve.
#' @export
swap_to_disc <- function(swap) {
  d = swap
  d[1] = 1 / (1 + swap[1])
  for (j in 2:length(swap)) {
    d[j] = (1 - sum(d[1:(j-1)])*swap[j]) / (1 + swap[j])
  }
  d
}

#' Converts a discount factor curve to a spot rate curve
#' 
#' @param disc The discount factor curve.
#' @export
disc_to_spot <- function(disc) {
  (1 / disc)^(1/(seq_along(disc)))-1
}

#' Converts a spot curve to a futures curve
#' 
#' @param spot The spot curve.
#' @export
spot_to_disc <- function(spot) {
  1 / ( (1 + spot)^(seq_along(spot)) )
}

#' Converts a discount factor curve to a futures curve
#' 
#' @param disc The discount factor curve.
#' @export
disc_to_fut <- function(disc) {  
  exp(-diff(log(c(1,disc))))-1  
}

#' Converts a futures curve to a discount factor curve
#' 
#' @param fut The futures curve. A vector.
#' @export
fut_to_disc <- function(fut) {
  1 / cumprod(1+fut)
}

#' Converts a discount factor curve to a german loan rate curve
#' 
#' @param disc The discount factor curve.
#' @export
disc_to_german <- function(disc) {
  vapply(
    1:length(disc),
    function(i) ( 1 - 1 / i * sum(disc[1:i]) ) / sum( disc[1:i] * (1 - ( 1:i - 1 ) / i ) ),
    1)  
}

#' Converts a discount factor curve to a french loan rate curve
#' 
#' @param disc The discount factor curve.
#' @param search_interval A length 2 vector. The interval to use for the root finding algorithm.
#' Your rates should be inside this interval.
#' @param tol The tolerance for the root finding algorithm.
#' @export
disc_to_french <- function(disc, search_interval = c(0.0001,1), tol = 1e-8) {  
  zerome = function(r,i,disc) 1/r * ( 1 - (1+r)^(-i) ) - sum(disc[1:i])
  vapply(
    1:length(disc),
    function(i) uniroot(function(r) zerome(r,i,disc), interval = search_interval, tol = tol)$root,
    1)
}

#' Converts an effective rate into a direct rate
#' 
#' @param r The effective rate
#' @export
eff_to_dir <- function(r) {
  (1 + r) ^ seq_along(r) - 1
}

#' Converts a direct rate into an effective rate
#' 
#' @param r The direct rate
#' @export
dir_to_eff <- function(r) {
  (1 + r) ^ (1 / seq_along(r)) - 1
}

#' Creates a rate curve instance
#' @param fun_d A discount factor function. fun_d(x) returns the discount factor for time x
#' @param fun_r A rate function. fun_r(x) returns the EPR for time x
#' @param rate_type The rate type. Must be on of c("french","fut","german","spot","swap")
#' @param knots The nodes to bootstrap the rates
#' @export
rate_curve <- function(fun_d = NULL, fun_r = NULL, rate_type = NULL, knots) {
  if (!(!is.null(fun_d) || (!is.null(fun_r) & !is.null(r_type))))
    stop("A rate or discount function must be given to create a rate_curve")
  if (is.null(fun_d)) {
    y <- do.call(what = paste0(r_type,"_to_disc"), args = list(fun_r(knots)))
    f <- approxfun(x = knots, y = y, method = "linear", rule = 2)
  }
  else {
    f <- fun_d
  }
  r <- structure(list(), class = "rate_curve")
  r$f <- f
  r$knots <- knots
  r
}

#' Returns a function f such that f(x) is the rate of type rate_type for time x
#' @param r A rate curve object
#' @param type The rate type
#' @export
get_rate_fun <- function(r, rate_type) {
  stopifnot(type %in% c("french","fut","german","spot","swap"))
  x <- (r$f)(r$knots)
  do.call(what = paste0("disc_to_",rate_type), args = list(x))
}

# TODO: Add a subsetting operator to get specific rates or discount factors maybe?