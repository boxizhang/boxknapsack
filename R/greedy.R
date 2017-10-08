#' @title greedy_knapsack
#'
#' @description The 'greedy_knapsack' function is a approach that use the a heuristic or approximation for knapsack package problem.
#' @param x is a matrix containing the weights and values
#' @param W a numeric string.
#' @return A list cointaining a value and a element
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#' @examples
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#'
#' @export




greedy_knapsack <- function(x,W) {
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
    x.sorted <- x[ order(-x$v/x$w),]
    elements <- c()
    total.weight <- 0
    total.value <- 0
    i <- 1
    repeat {
      if (total.weight + x.sorted$w[i] <= W) {
        total.weight <- total.weight + x.sorted$w[i]
        total.value <- total.value + x.sorted$v[i]
        elements <- append(elements, as.numeric(rownames(x.sorted[i,])))
        i <- i + 1
      } else
        break
    }
    return(list(value = total.value, elements = elements))
  }
}


