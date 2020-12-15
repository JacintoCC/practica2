#' Auxiliar function to transform a number to a binary vector
#' @param x Integer to transform
#' @param D Significant digits
#' @return Binary vector with binary representation of the integer
binint <- function(x, D = ceiling(log(x,2))){ 
  x <- rev(intToBits(x)[1:D]) # get D bits
  # remove extra 0s from raw type:
  as.numeric(unlist(strsplit(as.character(x),""))[(1:D)*2])
}

#' Auxiliar function to transform a binary vector into its decimal representation
#' @param x Binary vector
#' @return Decimal representation
intbin <- function(x){
  sum(2^(which(rev(x==1))-1))
} 

#' sumbin of binary raw object x
#' @param x Binary vector
#' @return sum of the bits of the vector
#' @export
sumbin=function(x) sum(as.numeric(x))

#' MaxSin of binary raw object x
#' @param x Binary vector
#' @return maxsin function
#' @export
maxsin <- function(x){
  sin(pi*(intbin(x))/(2^length(x)))
}


#' Sphere Function
#' @param x Real vector
#' @return Sum of sqared values
#' @export
sphere <- function(x){
  return(sum(x^2))
}

#' Rastrigin Function
#' @param x Real vector
#' @return Rastrigin function
#' @export
rastrigin <- function(x){
  return(sum(x^2 - 10*cos(2*pi*x) + 10))
}

