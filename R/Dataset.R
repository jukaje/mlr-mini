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

t <- Dataset(cars, target = "dist")
unclass(t)


class(Dataset(cars, target = "dist", type = "dd"))
