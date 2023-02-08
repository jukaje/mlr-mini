#' @title Dataset Config
#' @description How is the dataset structured and how can it be manipulated
#' @param data: Dataframe for target and features
#' @param target: Character, which represent the target name
#' @param type: "regression", "classification"
#' @param name: Character
#' @examples. 
#' Dataset(cars, target = "dist", type = "regression")
#' @export

Dataset <- function(data, target, type = character(0), name = deparse(substitute(data), 20)[[1]]){
  structure(list(data = data, target = target, type = type, name = name),
            class = c("DatasetRegression", "Dataset"))
}

#' @title Dataset Print
#' @param object: Element of class Dataset
#' @examples. 
#' print(Dataset(cars, target = "dist", type = "regression"))
#' Dataset "cars", predicting "dist" (regression)
#' 
#'    speed dist
#' 1      4    2
#' 2      4   10
#' 3      7    4
#' @export
print.Dataset <- function(object) {
  cat(c("Dataset ", '"',object$name, '"', ",", " predicting ", '"',object$target, '"', " (", object$type,")", "\n\n"), sep = "")
  print(data.frame(object$data))
}

#' @title Dataset column and row selection
#' @param obj: Element of class Dataset
#' @param i: row 
#' @param j: column 
#' @examples. 
#' Dataset(cars, target = "dist")[c(1,2),]
#' 
#' speed dist
#' 1     4    2
#' 2     4   10
#' @export

'[.Dataset' <- function(obj, i, j = names(obj$data)) {
  
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

#' @title Dataset Metainfo
#' @param data: Element of class Dataset
#' @examples. 
#' metainfo(Dataset(cars, target = "dist"))
#' 
#' $feature
#' $feature[[1]]
#' [1] "speed"   "numeric"
#' 
#' 
#' $target
#' [1] "dist"    "numeric"
#' 
#' $nrow
#' [1] 50
#' 
#' $type
#' character(0)
#' 
#' $missings
#' [1] FALSE
#' 
#' attr(,"class")
#' [1] "DatasetInfo"
#' @export

metainfo <- function(data) {
  features <- names(data$data)[names(data$data) != data$target]
  targets <- data$target
  nrow <- nrow(data$data)
  type <- data$type
  missings <- any(is.na(data$data))
  info <- list(feature = c(lapply(features, function(x) { c(x, class(data$data[,x])) })), 
               target = c(targets, class(data$data[,targets])),
               nrow = nrow, type = type, missings = missings)
  class(info) <- "DatasetInfo"
  return(info)
}


#' @title Dataset in Matrix and data.frame
#' @param Dataset: Element of class Dataset
#' @examples. 
#' as.matrix(Dataset(cars, target = "dist"))
#' as.dataframe(Dataset(cars, target = "dist"))
#' @export

as.matrix.Dataset <- function(Dataset) {
  as.matrix(Dataset$data)
}

as.data.frame.Dataset <- function(Dataset) {
  as.data.frame(Dataset$data)
}



