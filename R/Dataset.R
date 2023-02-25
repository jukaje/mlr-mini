#' @title Dataset Config
#' @description How is the dataset structured and how can it be manipulated
#' @param data: Dataframe for target and features
#' @param target: Character, which represent the target name
#' @param task: "regression", "classification"
#' @param name: Character
#' @examples. 
#' Dataset(cars, target = "dist", task = "Regression")
#' @export

Dataset <- function(data, target, task, name = deparse(substitute(data), 20)[[1]]){
  if (missing(task)) {
    task_in = readline('Which task ("0 = Classification" / " 1 = Regression"): ')
    if (task_in == 0) task <- "Classification" else task <- "Regression"
  } 
  task_out <- paste("Dataset", task, sep = "")
  structure(list(data = data, target = target, type = task, name = name),
            class = c(task_out, "Dataset"))
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

'[.Dataset' <- function(obj, i , j) {
  #= rownames(obj$data)
  out_str <- nargs()
  if (out_str == 2) {
    j <- i
    i <- rownames(obj$data)
  } else if (missing(i) & missing(j)) {
    i <- rownames(obj$data)
    j <- names(obj$data)
  } else if (missing(j)) {
    j <- names(obj$data)
  } else if (missing(i)) {
    i <- rownames(obj$data)
  }
  if (is.character(j) & !(obj$target %in% j)) {
    stop(sprintf('Cannot remove target column "%s" \n', obj$target))
  } else if (is.numeric(j) & !(obj$target %in% names(obj$data)[j])) {
    stop(sprintf('Cannot remove target column "%s" \n', obj$target))
  } 
    else if (out_str == 2) {
    as.data.frame(obj$data)[j]
    } else {
      as.data.frame(obj$data)[i,j]
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
  info <- list(feature = sapply(data$data[features],class), 
               target = sapply(data$data[targets],class),
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


