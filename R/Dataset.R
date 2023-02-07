Dataset <- function(data, target, type = "regression", name = deparse(substitute(data), 20)[[1]]){
  
  #class(data) <- c("Dataset", "Regression")
  #class(target) <- c("Dataset", class(target))
  
  structure(list(data = data, target = target, type = type, name = name),
            class = c("DatasetRegression", "Dataset"))
}

print.Dataset <- function(object) {
  cat(c("Dataset ", '"',object$name, '"', " predicting ", '"',object$target, '"', ":", "\n\n"), sep = "")
  print(data.frame(object$data))
}

'[.Dataset' <- function(obj, i, j = names(obj$data)) {
  #print(names(obj$data))
  if (is.character(j) & !(obj$target %in% j)) {
    cat(c("Error: Cannot remove target column ", 
         '"', obj$target, '"', "\n"), sep = "")
  } else if (is.numeric(j) & !(obj$target %in% names(obj$data)[j])) {
    cat(c("Error: Cannot remove target column ", 
          '"', obj$target, '"', "\n"), sep = "")
  } else {
    data.frame(unclass(obj$data))[i,j]
  }
  
}



metainfo <- function(data) {
  features <- names(data$data)[names(data$data) != data$target]
  targets <- data$target
  nrow <- nrow(data$data)
  type <- data$type
  missings <- any(is.na(data$data))
  #print(class(data$data[,features]))
  info <- list(feature = c(lapply(features, function(x) { c(x, class(data$data[,x])) })), 
               target = c(targets, class(data$data[,targets])),
               nrow = nrow, type = type, missings = missings)
  class(info) <- "DatasetInfo"
  return(info)
}



t <- Dataset(cars, target = "dist")
t[1,1]
#dataframe
names(t$data) 



class(Dataset(cars, target = "dist", type = "dd"))


cars_info <- metainfo(t)
print(cars_info)


