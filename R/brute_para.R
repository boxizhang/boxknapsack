#' @title brute_force_knapsack_para
#' @description tests all combinations and finds the one with max value under the restriction total weight<W.
#' @param x is a matrix containing the weights and values
#' @param W a numeric string.
#' @return a list with the _value_ of the optimally packed knapsack and the _elements_ that gives this value.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @export
#' @examples
#' set.seed(42)
#'n <- 2000
#'knapsack_objects <-
#'  data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'   v=runif(n = n, 0, 10000)
#'  )
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)



brute_force_knapsack_para<-function(x, W){
  cores<-parallel::detectCores()-2

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
    len<-length(x$w)

    cl <- makeCluster(cores)

    clusterExport(cl, c("x"), envir = environment())
    Box <- parLapplyLB(cl, 1:len, fun =  function(y) {
        combn(rownames(x), y, paste0, collapse = " ")

    })
    weights <- parLapplyLB(cl, 1:len, fun =  function(y) {
        combn(x$w, y, sum)

    })
    values<- parLapplyLB(cl,1:len, fun =  function(y) {
        combn(x$v, y , sum)

    })

    stopCluster(cl)

    Box <- unlist(Box)
    weights <- unlist(weights)
    values<- unlist(values)
    value <- max(values[which(weights <= W)])
    elements <- Box[which(values == value)]
    return(list(value = value, elements = as.numeric(strsplit(elements, " ")[[1]])))


  }
}
