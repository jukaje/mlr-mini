#' @title make hyperparameter space
#' @description These functions returns the type, and start and end of a range for dbl and int, or a vector of factors for fct
#' @param range1 start (left) value
#' @param range2 end (right) value
#' @param factors vector of factors
#' 
#' @examples 
#' x <- p_dbl(1,3)
#' 
#' x
#' 
#' #> $type
#' #> [1] "dbl"
#' 
#' #> $start
#' #> [1] 1
#' 
#' #> $end
#' #> [1] 3

#' @export

x <- p_dbl(1,3)

p_dbl <- function(range1, range2) {
  return(list(type = "dbl", start = range1, end = range2))
}

p_int <- function(range1, range2) {
  # checkIntegerish
  return(list(type = "int", start = range1, end = range2))
}

p_fct <- function(factors) {
  return(list(type = "fct", factors = factors))
}