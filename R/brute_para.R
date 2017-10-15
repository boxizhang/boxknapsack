#' @title brute_force_knapsack
#' @description tests all combinations and finds the one with max value under the restriction total weight<W.
#' @param x is a matrix containing the weights and values
#' @param W a numeric string.
#' @param parallel is the logical
#' @return a list with the _value_ of the optimally packed knapsack and the _elements_ that gives this value.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @export
#' @importFrom utils combn
#' @import parallel

# set.seed(42)
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# brute_force_knapsack_para(x = knapsack_objects[1:8,], W = 3500)



brute_force_knapsack<-function(x, W, parallel = FALSE){
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
  else if (parallel == FALSE){
    len<-length(x$w)
    Box<-sapply(c(1:2^len),function(x){
    as.integer(intToBits(x)[1:len])
  })
  values<-c()
  max_val<-function(col){
    OK_weights<-c()
    weight<-t(x$w)%*%col
    if (weight<=W){
      values<-c(values,t(x$v)%*%col)
    } else {
      values<-c(values,0)
    }
    return(values)
  }
  m<-apply(Box,2,max_val)
  index_max<-which.max(m)
  index_elements<-Box[,index_max]
  elements<-c(c(1:len)*index_elements)
  elements<-elements[elements>0]
  return(list(value=round(max(m)),elements=elements))
  } else {
    requireNamespace("parallel")
    cores<-parallel::detectCores()
    cores=1
    len<-length(x$w)

    cl <- parallel::makeCluster(cores)

    parallel::clusterExport(cl, c("x"), envir = environment())
    Box <- parallel::parLapplyLB(cl, 1:len, fun =  function(y) {
        combn(rownames(x), y, paste0, collapse = " ")

    })
    weights <- parallel::parLapplyLB(cl, 1:len, fun =  function(y) {
        combn(x$w, y, sum)

    })
    values<- parallel::parLapplyLB(cl,1:len, fun =  function(y) {
        combn(x$v, y , sum)

    })

    parallel::stopCluster(cl)

    Box <- unlist(Box)
    weights <- unlist(weights)
    values<- unlist(values)
    value <- max(values[which(weights <= W)])
    elements <- Box[which(values == value)];
    return(list(value = value, elements = as.numeric(strsplit(elements, " ")[[1]])))
  }
}
