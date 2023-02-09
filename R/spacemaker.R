#' @title make hyperparameter space
#' @description This function returns the type, and start and end of a range for dbl
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

p_dbl <- function(range1, range2) {
  checkmate::assertNumeric(range1, len = 1)
  checkmate::assertNumeric(range2, len = 1)
  return(list(type = "dbl", start = range1, end = range2))
}

#' @title make hyperparameter space
#' @description This function returns the type, and start and end of a range for int
#' @param range1 start (left) value
#' @param range2 end (right) value
#' @param factors vector of factors
#' 
#' @examples 
#' x <- p_int(1,3)
#' 
#' x
#' 
#' #> $type
#' #> [1] "int"
#' 
#' #> $start
#' #> [1] 1
#' 
#' #> $end
#' #> [1] 3

#' @export

p_int <- function(range1, range2) {
  # checkIntegerish
  checkmate::assertIntegerish(range1, len = 1)
  checkmate::assertIntegerish(range2, len = 1)
  return(list(type = "int", start = range1, end = range2))
}

#' @title make hyperparameter space
#' @description This function returns the type, and start and end of a vector of factors
#' @param factors vector of factors
#' 
#' @examples 
#' x <- p_fct(c("a", "b"))
#' 
#' x
#' 
#' #> $type
#' #> [1] "fct"
#' 
#' #> $factors
#' #> [1] "a" "b"


#' @export

p_fct <- function(levels) {
  checkmate::assertAtomic(levels)
  return(list(type = "fct", levels = as.character(levels)))
}

