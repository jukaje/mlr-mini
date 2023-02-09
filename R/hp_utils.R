


# Hyperparameters

# generics of the Inducer S3-class

#Idea: hpx <- hp(x = p_dbl(0, 1), y = p_int(1, 10), z = p_fct(letters))
# this will be the inducer hyperparameter space
# the function will be called within the inducer to construct the inducers hyperparameterspace (an S3 object)

# hp() is the constructor for the S3 object of the hyperparameters

# hpx
#>    name type                range
#> 1:    x  dbl               [0, 1]
#> 2:    y  int              [1, 10]
#> 3:    z  fct {"a", "b", "c", ...}



# hp <- # class constructor
# hp: this is the constructor for the hyperparameters S3 object. 
#> It takes a variable number of arguments, which correspond to the different hyperparameters to be included in 
#> the space. It uses the p_dbl, p_int, and p_fct functions to create the lists for each hyperparameter 
#> and store the type, range, and name of each hyperparameter in a data.table object



#' Constructor for class 'hp'
#'
#' @param ... 
#
#' @export
#'
#' @examples
#' 
#' hpx <- hp(x = p_dbl(0, 1), y = p_int(1, 10), z = p_fct(letters))
#' hpx
#' 
#' #>    name type                range
#' #> 1:    x  dbl               0, 1
#' #> 2:    y  int              1, 10
#' #> 3:    z  fct "a", "b", "c", ...


hp <- function(...) {
  
  # define the list elements necessary for the result
  hpnames <- names(list(...))
  hptype <- character()
  hprange <- list()
  
  # for each given Hyperparameter, store the type and the given range, or the factors for factors
  for (hpcurrent in seq_len(nargs())) {
    hpcurrent <- list(...)[[hpcurrent]]
    # store the type
    hptype[length(hptype) + 1] <- hpcurrent$type
    
    # store the range
    if (hpcurrent$type == ("dbl")) {
      hprange[[length(hprange) + 1]] <- c(hpcurrent$start, hpcurrent$end)
    }
    
    # store the range
    if (hpcurrent$type == ("int")) {
      hprange[[length(hprange) + 1]] <- c(hpcurrent$start, hpcurrent$end)
    }
    
    # store the factors
    if (hpcurrent$type == "fct") {
      hprange[[length(hprange) + 1]] <- hpcurrent$factors
    }
  }
  
  # to check if value is in range:
  # dplyr::between(value, start, end)
  
  structure(list(
    name = hpnames, type = hptype, range = hprange # hprange = vector // result better as data.table??
  ), class = "hp")
}


# example: hpx <- hp(x = p_dbl(0, 1), y = p_int(1, 10), z = p_fct(letters))

#############################################

# define the print method for hp objects
#' Print method for hp objects
#'
#' @param x 
#' @param ... 
#'
#' @export

print.hp <- function(x, ...) { # Why is there ... here?
  print(data.table(name = x$name, type = x$type, range = x$range))
  invisible(x)
}




##############################################


# space contained in: hyperparameters(ind$xgboost)

#' check if Hyperparameters are valid for given hyperparameter space
#'
#' @param hps 
#' @param hpx 
#'
#' @export
#'
#' @examples
#' 
#' hpx <- hp(x = p_dbl(0, 1), y = p_int(1, 10), Z = p_fct(letters))
#' hpx
#' #>    name type                range
#' #> 1:    x  dbl               [0, 1]
#' #> 2:    y  int              [1, 10]
#' #> 3:    z  fct {"a", "b", "c", ...}
#' 
#' checkHyperparameter(list(x = 1, y = 1, z = "a"), hpx)
#' #> [1] TRUE
#' 
#' checkHyperparameter(list(z = "a"), hpx)
#' #> [1] TRUE
#' 
#' checkHyperparameter(list(z = "A"), hpx)
#' #> [1] "Must be element of set {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'}, but is 'A'"

checkHyperparameter <- function(hps, hpx) {
  # check that hps are a list
  
  # check that hpspace is a datatable
  # Initialize logical vectors to store the results
  result <- logical()
  endresult <- logical()
  
    for (hpcurrent in seq_len(length(hps))) {
      
      if (typeof(hps[[hpcurrent]]) == "double") {
        # Find the index of the current hyperparameter in the 'hpx' datatable
        entryhp <-  which(hpx$name == names(hps[hpcurrent]))
        # Check if the current hyperparameter value is within the specified range
        result <- dplyr::between(hps[[hpcurrent]], hpx$range[[entryhp]][[1]], hpx$range[[entryhp]][[2]])
        
        if (!result) {
          # If not within range, print an error message and store 'FALSE' in the 'endresult' vector
          endresult[length(endresult) + 1] <- FALSE
          print(sprintf("must be within range between %s and %s, but is %s", hpx$range[[entryhp]][[1]], hpx$range[[entryhp]][[2]], hps[[hpcurrent]]))
        } else { 
          # If within range, store 'TRUE' in the 'endresult' vector
          endresult[length(endresult) + 1] <- TRUE
        }
      }
    
  
  if (typeof(hps[[hpcurrent]]) == "integer") {
    
    entryhp <-  which(hpx$name == names(hps[hpcurrent]))
    # check if the current hp value is an Integer
    checkmate::checkIntegerish(hps[hpcurrent])
    # Check if the current hyperparameter value is within the specified range
    result <- dplyr::between(hps[[hpcurrent]], hpx$range[[entryhp]][[1]], hpx$range[[entryhp]][[2]])
    
    if (!result) {
      # If not within range, print an error message and store 'FALSE' in the 'endresult' vector
      endresult[length(endresult) + 1] <- FALSE
      print(sprintf("must be an integer between %s and %s, but is %s", hpx$range[[entryhp]][[1]], hpx$range[[entryhp]][[2]], hps[[hpcurrent]]))
    } else {
      # If within range, store 'TRUE' in the 'endresult' vector
      endresult[length(endresult) + 1] <- TRUE
    }
  }
      if (typeof(hps[[hpcurrent]]) == "character") {
        entryhp <-  which(hpx$name == names(hps[hpcurrent]))
        
        # Check if the current hyperparameter value is within the set of valid values
        result <- hps[[hpcurrent]] %in% hpx$range[[entryhp]]
        
        if (!result) {
          # If not within set, print an error message and store 'FALSE' in the 'endresult' vector
          endresult[length(endresult) + 1] <- FALSE
          print(sprintf("must be element of set {%s}, but is '%s'", paste(hpx$range[[entryhp]], collapse = ", "), hps[[hpcurrent]]))
        } else {
          # If within set, store 'TRUE' in the 'endresult' vector
          endresult[length(endresult) + 1] <- TRUE
        }
      }
      
    }
  
  if (all(endresult)) {
    return(TRUE)
  } else {
    invisible(FALSE)
  }
  
 }


#############################################
hyperparameters <- function(object, ...) {
  UseMethod("hyperparameters")
}


# ind$xgboost$hyperparameters <- hp(eta = dbl(0,1), ...)

# hyperparameters(ind$xgboost)
# Hyperparameter Space:
  #>                 name type    range
  #> 1:               eta  dbl   [0, 1]
  #> 2:           nrounds  dbl [1, Inf]
  #> 3:         max_depth  dbl [0, Inf]
  #> 4:  colsample_bytree  dbl   [0, 1]
  #> 5: colsample_bylevel  dbl   [0, 1]
  #> 6:            lambda  dbl [0, Inf]
  #> 7:             alpha  dbl [0, Inf]
  #> 8:         subsample  dbl   [0, 1]
  #> 9:           verbose  int   [0, 2]
  