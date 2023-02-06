


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

hp <- function(...) {
  
  # result <- data.table("name" = character(), "type" = character(), "range" = c())
  #colnames(result) <- c("name_type", "range")
  result <- setNames(data.table(matrix(nrow = nargs(), ncol = 3)), c("name", "type", "range")) # nargs() RETURNS THE NUMBER of arguments given
  
  input <- c(...)
  hpnames <- names(list(...))
  hptype <- character()
  hprange <- list()
  
  # save the given hyperparameter spaces
  for (hpcurrent in seq_len(nargs())) {
    hpcurrent <- list(...)[[hpcurrent]]
    hptype[length(hptype) + 1] <- hpcurrent$type  # typeof(hpcurrent)

    if (hpcurrent$type == ("dbl")) {
      # append(hprange, list(hpcurrent$start, hpcurrent$end))
      hprange[[length(hprange) + 1]] <- c(hpcurrent$start, hpcurrent$end)
    }
    
    if (hpcurrent$type == ("int")) {
      hprange[[length(hprange) + 1]] <- c(hpcurrent$start, hpcurrent$end)
    }

    if (hpcurrent$type == "fct") {
      #checkIntegerish
      hprange[[length(hprange) + 1]] <- hpcurrent$factors
    }
  }
  
  # to check if value is in range:
  # dplyr::between(value, start, end)
  
  structure(list(
    name = hpnames, type = hptype, range = hprange # hprange = vector
  ), class = "hp")
}

hpx <- hp(x = p_dbl(0, 1), y = p_int(1, 10), z = p_fct(letters))

#############################################

print.hp <- function(x, ...) {
  print(data.table(name = x$name, type = x$type, range = x$range))
  invisible(x)
}

#hpx


##############################################

# checkHyperparameter(list(x = 1, y = 1, z = "a"), hpx)
# #> [1] TRUE
# 
# checkHyperparameter(list(z = "a"), hpx)
# #> [1] TRUE
# 
# checkHyperparameter(list(z = "A"), hpx)
# #> [1] "Must be element of set {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'}, but is 'A'"


# space contained in: hyperparameters(ind$xgboost)


checkHyperparameter <- function(hps, hpx) {
  # check that hps are a list
  
  # check that hpspace is a datatable
  
#     for (hpcurrent in hps) {
#       if (typeof(hpcurrent) == "dbl") {
#         result[length(result) + 1] <- dplyr::between(hpcurrent, hpx[[names(hpcurrent)]]$start, hpx[[names(hpcurrent)]]$end)
#       }
#     }
#   
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
  