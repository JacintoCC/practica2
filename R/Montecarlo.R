#' Full Blind Search Method
#'
#' \code{fsearch} evaluates a function in the given points and returns the optimum
#'
#' @param search Matrix with solutions x D
#' @param FUN Evaluation function
#' @param type Type of the searched optimum
#' @param ... Extra arguments for FUN function
#'
#' @return List
#'    \item{index}{Index of element with min/max result}
#'    \item{sol}{Optimum location}
#'    \item{eval}{Optimum}
#'
#' @export
fsearch <- function(search, FUN, type = "min",...) {
  x <- apply(search,1,FUN,...) # run FUN over all search rows
  ib <- switch(type, min=which.min(x), max=which.max(x))
  return(list(index=ib,sol=search[ib,],eval=x[ib]))
}
#' Montecarlo Uniform Search Method
#'
#' \code{mcsearch} evaluates a function using Montecarlo method and returns the optimum
#'
#' @param N Number of samples
#' @param lower Vector with lowest values for each dimension
#' @param upper Vector with highest values for each dimension 
#' @param type Type of the searched optimum
#' @param FUN Evaluation function
#' @param type Type of the searched optimum
#' @param ... Extra arguments for FUN function
#' @importFrom stats runif
#'
#' @return List
#'    \item{index}{Index of element with min/max result}
#'    \item{sol}{Optimum location}
#'    \item{eval}{Optimum}
#'
#' @export
mcsearch <- function(N,lower,upper,FUN,type="min",...){ 
  D <- length(lower)
  s <- t(sapply(1:N, function(x) stats::runif(D,lower,upper)))
  
  fsearch(s,FUN,type,...)
}

