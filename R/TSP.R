#' Travel Salesperson Problem Function
#' @param x Proposed solution
#' @param coordinates Matrix with coordinates
#' @return Length of the travel
#' @export
tsp.evaluation <- function(x, coordinates){
 
}

#' Change solution TSP
#' @param s Solution
#' @return modified solution
#' @export
change.tsp <- function(s){
  n <- length(s)
  to.change <- sample(1:n, 2)
  aux <- s[to.change[1]]
  s[to.change[1]] <- s[to.change[2]]
  s[to.change[2]] <- aux
  return(s)
}

#' Travel Salesperson Problem Function
#' @param x Proposed solution
#' @param coordinates Matrix with coordinates
#' @return Length of the travel
#' @export
plotTSP <- function(x, coordinates){
  coordinates <- as.data.frame(coordinates[x, ])
  coordinates <- rbind(coordinates, coordinates[1, ])
  names(coordinates) <- c("X", "Y")
  ggplot2::ggplot(coordinates, ggplot2::aes(X, Y)) +
    ggplot2::geom_path()
}


#' Hill Climbing Method
#' @param par Initial solution
#' @param change Function to generate the next candidate
#' @param coordinates Coordinates
#' @param control List with stopping and monitoring method: maxit (maximum number of iterations), REPORT (frequency of monitoring information)
#' @return List with solution and evaluation
#' @export
hclimbingTSP <- function(par,change, coordinates, control,...){ 
  fpar <- tsp.evaluation(par, coordinates)
  for(i in 1:control$maxit) {
    par1 <- change(par)
    fpar1 <- tsp.evaluation(par1, coordinates)
    # Report
    if(control$REPORT>0 &&(i==1||i%%control$REPORT==0))
      cat("i:",i,"s:",par,"f:",fpar,"s'",par1,"f:",fpar1,"\n")
    # Update value
    if(fpar1<fpar){
      par <- par1
      fpar <- fpar1
    }
  }
  if(control$REPORT>=1) 
    cat("best:", par, "f:", fpar, "\n")
  return(list(sol=par,eval=fpar))
}




#' Montecarlo Uniform Search Method for TSP
#'
#' \code{mcsearchTSP} evaluates a function using Montecarlo method and returns the optimum
#'
#' @param N Number of samples
#' @param coordinates TSP coordinates
#' @param ... Extra arguments for FUN function
#' @importFrom stats runif
#'
#' @return List
#'    \item{index}{Index of element with min result}
#'    \item{sol}{Optimum location}
#'    \item{eval}{Optimum}
#'
#' @export
mcsearchTSP <- function(N, coordinates,...){
  dim <- nrow(coordinates)
  sample <- t(sapply(1:N, function(i) sample(1:dim, dim)))
  evaluation <- apply(sample, 1, function(x) tsp.evaluation(x, coordinates))
  i.min <- which.min(evaluation)
  return(list(index=which.min(evaluation),sol=sample[i.min,],eval=evaluation[i.min]))
}