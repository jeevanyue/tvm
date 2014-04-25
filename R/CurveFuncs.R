#' @import ggplot2

#' @title Converts a futures curve to a spot curve
#' 
#' @param fut The futures curve.
#' @export
fut_to_spot <- function(fut) {
  (cumprod(1+fut))^(1/(seq_along(fut)))-1
}

#' @title Converts a discount factor curve to a swap rate curve
#' 
#' @param disc The discount curve.
#' @export
disc_to_swap <- function(disc) {
  (1-disc) / cumsum(disc)
}

#' @title Converts a swap curve to a discount factor curve
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

#' @title Converts a discount factor curve to a spot rate curve
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

#' @title Converts a discount factor curve to a futures curve
#' 
#' @param disc The discount factor curve.
#' @export
disc_to_fut <- function(disc) {  
  exp(-diff(log(c(1,disc))))-1  
}

#' @title Converts a futures curve to a discount factor curve
#' 
#' @param fut The futures curve. A vector.
#' @export
fut_to_disc <- function(fut) {
  1 / cumprod(1+fut)
}

#' @title Converts a discount factor curve to a german loan rate curve
#' 
#' @param disc The discount factor curve.
#' @export
disc_to_german <- function(disc) {
  vapply(
    1:length(disc),
    function(i) ( 1 - 1 / i * sum(disc[1:i]) ) / sum( disc[1:i] * (1 - ( 1:i - 1 ) / i ) ),
    1)  
}

#' @title Converts a discount factor curve to a french loan rate curve
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

#' @title Converts an effective rate into a direct rate
#' 
#' @param r The effective rate
#' @export
eff_to_dir <- function(r) {
  (1 + r) ^ seq_along(r) - 1
}

#' @title Converts a direct rate into an effective rate
#' 
#' @param r The direct rate
#' @export
dir_to_eff <- function(r) {
  (1 + r) ^ (1 / seq_along(r)) - 1
}

#' @title Creates a rate curve instance
#' 
#' @param rates A rate vector
#' @param rate_type The rate type. Must be on of c("french","fut","german","spot","swap")
#' @param pers The periods the rates correspond to
#' @param fun_d A discount factor function. fun_d(x) returns the discount factor for time x
#' @param fun_r A rate function. fun_r(x) returns the EPR for time x
#' @param knots The nodes used to bootstrap the rates
#' @export
rate_curve <- function(
  rates = NULL,
  rate_type = "spot",
  pers = 1:length(rates),
  fun_d = NULL,
  fun_r = NULL,
  knots = 1:length(rates)) {
  if (!(
    (!is.null(fun_d) &&  !is.null(knots)) ||
      (!is.null(fun_r) && !is.null(rate_type) && !is.null(knots)) ||
      (!is.null(rates) && !is.null(rate_type))
    ))
    stop("A rate or discount function must be given to create a rate_curve")
  if (!is.null(fun_d)) {    
    r <- structure(list(), class = "rate_curve")
    r$f <- fun_d
    r$knots <- knots
    r
  } else if (!is.null(fun_r)) {
    d <- do.call(what = paste0(rate_type,"_to_disc"), args = list(fun_r(knots)))
    log_f <- approxfun(x = knots, y = log(d), method = "linear", rule = 2)
    f <- function(x) exp(log_f(x))
    rate_curve(fun_d = f, knots = knots)
  } else if (!is.null(rates)) {
    f <- approxfun(x = pers, y = rates, method = "linear", rule = 2)
    rate_curve(fun_r = f, rate_type = rate_type, knots = knots)
  } else {
    stop("The rate_curve constructor lacks arguments")
  }  
}

#' @title Returns a function f such that f(x) is the rate of type rate_type for time x
#' 
#' @param r A rate curve object
#' @param type The rate type
#' @export
get_rate_fun <- function(r, rate_type = "spot") {
  stopifnot(rate_type %in% c("french","fut","german","spot","swap"))
  d <- (r$f)(r$knots)
  y <- do.call(what = paste0("disc_to_",rate_type), args = list(d))
  approxfun(x = r$knots, y = y, method = "linear", rule = 2)
}

#' @title Returns a particular rate or rates from a curve
#' 
#' @param r The rate_curve object
#' @param x The points in time to return
#' @param rate_type The rate type
#' 
#' @return If \code{x} is \code{NULL}, then returns a rate function of \code{rate_type} type.
#' Else, it returns the rates of \code{rate_type} type and corresponding to time \code{x}
#' @method [ rate_curve
#' @export
`[.rate_curve` <- function(r,x = NULL,rate_type = "spot") {
  f <- get_rate_fun(r = r, rate_type = rate_type)
  if(is.null(x))
    f
  else
    f(x)
}

#' @title Plots a rate curve
#' 
#' @param r The rate curve
#' @param rate_types The rate types to plot
#' @method plot rate_curve
#' @export
plot.rate_curve <- function(r, rate_types = c("french","fut","german","spot","swap")) {
  df <- as.data.frame(lapply(
    X = rate_types,
    FUN = function(x)  r[r$knots, x]
    ))
  names(df) <- rate_types
  df$Time = r$knots    
  dfm <- reshape2::melt(data = df, id.vars = "Time", variable.name = "RateType", value.name = "Rate")
  ggplot2::ggplot(data = dfm) +
    ggplot2::geom_line(mapping = ggplot2::aes_string(x = "Time", y = "Rate", color = "RateType"))
}