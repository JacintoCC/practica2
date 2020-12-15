#' Hill Climbing Method
#' @param par Initial solution
#' @param fun Evaluation function
#' @param change Function to generate the next candidate
#' @param lower Vector with lowest values for each dimension
#' @param upper Vector with highest values for each dimension
#' @param control List with stopping and monitoring method: maxit (maximum number of iterations), REPORT (frequency of monitoring information)
#' @param type Optimization type "min" or "max"
#' @param ... Extra parameters for FUN
#' @return List with solution and evaluation
#' @export
hclimbing <- function(par,fun,change,lower,upper,control,
                      type="min",...){ 
  fpar <- fun(par,...)
  for(i in 1:control$maxit) {
    par1 <- change(par,lower,upper)
    fpar1 <- fun(par1,...)
                                        # Report
    if(control$REPORT>0 &&(i==1||i%%control$REPORT==0))
      cat("i:",i,"s:",par,"f:",fpar,"s'",par1,"f:",fpar1,"\n")
                                        # Update value
    if( (type=="min" && fpar1<fpar) || (type=="max" && fpar1>fpar)){
      par <- par1
      fpar <- fpar1
    }
  }
  if(control$REPORT>=1) 
    cat("best:", par, "f:", fpar, "\n")
  return(list(sol=par,eval=fpar))
}


#' Hill Climbing Slight Random Change Method
#' @param par Initial solution
#' @param lower Vector with lowest values for each dimension
#' @param upper Vector with highest values for each dimension
#' @param dist random distribution function
#' @param round use integer (TRUE) or continuous (FALSE) search
#' @param ... Extra parameters for dist
#' @return Updated solutions
#' @export
hchange <- function(par,lower,upper,dist,round=TRUE,...){
  step <- dist(length(par),...) # slight step
  if(round) 
    step <- round(step)
                                        # par1 within [lower,upper]:
  par1 <- mapply(FUN = function(v,high,low){ min(max(v, low), high) }, 
                 par+step, upper, lower)
  return(par1)
}
