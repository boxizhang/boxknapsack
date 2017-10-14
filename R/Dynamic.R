#' @title knapsack_dynamic
#'
#' @description The 'knapsack_dynamic' function is a approach that use the a heuristic or approximation for knapsack package problem#' @param x is a 2dim matrix containing the weights and values.
#' @param x is a matrix containing the weights and values
#' @param W a numeric string.
#' @return A list cointaining a value and a element
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithm}
#' @examples
#' set.seed(42)
#'n <- 2000
#'knapsack_objects <-
#'  data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'   v=runif(n = n, 0, 10000)
#'  )
#' knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500)
#' @export


knapsack_dynamic <- function(x,W) {
  if (!is.data.frame(x))
    stop("x is not a dataframe")
  else if (any(colnames(x) != c("w","v")))
    stop("columns from dataframe x must be named w and v")
  else if(any(!is.numeric(x$w)) || any(x$w < 0))
    stop("column w must be a vector of positive numeric values")
  else if(any(!is.numeric(x$v)) || any(x$v < 0))
    stop("column v must be a vector of positive numeric values")
  else if(!is.numeric(W) || W < 0)
    stop("W must be a positive numeric value")
  else {
    w<-x$w
    v<-x$v
    lapply(w,function(x){stopifnot(is.integer(x))})


    n<-nrow(x)
    m<-matrix(rep(0,times=(n+1)*(W+1)),nrow=n+1)

    for (i in 2:(n+1)){
      for (j in 2:(W+1)){
        if (w[i-1]<=j){
          m[i,j]<-max(m[i-1,j],m[i-1,j-w[i-1]]+v[i-1])
        } else{
          m[i,j]<-m[i-1,j]
        }
      }
    }
    value<-round(m[nrow(m),ncol(m)])
    amount = rep(0, length(w))
    a = m[nrow(m), ncol(m)]
    j = length(w)
    Y = W

    while(a > 0) {
      while(m[j+1,Y+1] == a) {
        j = j - 1
      }
      j = j + 1
      amount[j] = 1
      Y = Y - w[j]
      j = j - 1
      a = m[j+1,Y+1]
    }
    elements<-amount*c(1:length(amount))
    elements<-elements[elements>0]
    return(list(value=value,elements=elements))
  }
}
