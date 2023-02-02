Dataset <- function(data, target, type = "regression", name = deparse(substitute(data), 20)[[1]]){
  
  class(data) <- c("Dataset", class(data))
  class(target) <- c("Dataset", class(target))
  
  structure(list(data = data, target = target, type = type, name = name),
            class = "Dataset")
}


Dataset(cars, target = "dist")
class(Dataset(cars, target = "dist", type = "dd"))
