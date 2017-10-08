#' @title knapsack_dynamic
#'
#' @description The 'knapsack_dynamic' function is a approach that use the a heuristic or approximation for knapsack package problem#' @param x is a 2dim matrix containing the weights and values.
#' @param x is a matrix containing the weights and values
#' @param W a numeric string.
#' @return A list cointaining a value and a element
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithm}
#' @examples
#' knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500)
#'
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
    n <- nrow(x)
    m <- matrix(0, nrow = n+1, ncol = W+1)
    elements <- rep(0,W+1)

    for (i in 2:(n+1)) {
      for (j in 1:(W+1)) {
        if (x$w[i-1] > j)
          m[i, j] <- m[i-1, j]
        else
          m[i, j] <- max(m[i-1, j], m[i-1, j-x$w[i-1]] + x$v[i-1])
      }
    }

    result <- c()
    limit <- W+1
    for (j in (n+1):2){
      was_added <- m[j,limit] != m[j-1,limit]

      if (was_added) {
        result <- append(result, j-1)
        limit <- limit - x$w[j-1]
      }
    }

    return(list(value = m[n+1,W+1], elements = sort(result)))
  }
}
