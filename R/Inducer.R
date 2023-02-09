library(data.table)
library(xgboost)
#' Inducer for XGBoost
#' 
#' @param .data dataset object of class 'Dataset'
#' @param eta control the learning rate. Default: 0.3
#' @param nrounds max number of boosting iterations
#' @param max_depth maximum depth of a tree. Default: 6
#' @param colsample_bytree subsample ratio of columns when constructing each tree. Default: 1
#' @param colsample_bylevel
#' @param lambda L2 regularization term on weights. Default: 1
#' @param alpha L1 regularization from on weights. Default: 0
#' @param subsample subsample ratio of the training instance. Default: 1
#' @param verbose If 0, no informations will be printed. If 1, it will print informations about performances. If 2,additional information will be printed.
#' @export
#' @examples 
#' cars.data <- Dataset(cars, "dist", task = "Regression")
#' xgb <- InducerXGBoost(nrounds = 10)
#' mod <- xgb(cars.data)
#' 
#' mod <- InducerXGBoost(cars.data, nrounds = 10)
InducerXGBoost <- function(.data, eta = NULL, nrounds = NULL, max_depth = NULL, colsample_bytree = NULL,
                           colsample_bylevel = NULL, lambda = NULL, alpha = NULL, subsample = NULL, verbose = 0) {
  .f <- function(.data) {
    if (is.null(nrounds)) {nrounds <- 1}
    start <- Sys.time()
    output <- xgboost(data = as.matrix(.data$data[, names(metainfo(.data)$feature)]),
                      label = .data$data[, names(metainfo(.data)$target)], verbose = verbose, nrounds = nrounds,
                      params = configuration(.f)[!names(configuration(.f)) %in% c("verbose", "nrounds")])
    end <- Sys.time()
    
    structure(list(output = output, data = .data, config = configuration(.f), model = "xgboost",
                   training.time.sec = round(difftime(end, start, units = "s"))[[1]]),
              class = c("ModelXGBoost", "ModelRegression", "Model"))
  }
  class(.f) <- c("InducerXGBoost", "Inducer")
  if (!missing(.data)) {
    .f(.data)
  }
  .f
}
class(InducerXGBoost) <- c("InducerXGBoost", "Inducer")


#' Inducer for Random Forest
#' 
#' @param .data dataset object of class 'Dataset'
#' @param min.node.size Minimal node size. Default 1 for classification, 5 for regression, 3 for survival and 10 for probability
#' @param max.depth Maximal tree depth. NULL or 0 corresponds to unlimited depth, 1 to tree stumps
#' @param num.trees Number of trees. Default 500 trees
#' @param mtry Number of variables to potentially split in each node. Default rounded down square root of the number variables
#' @param verbose If 0, no informations will be printed. If 1, it will print informations about performances. If 2,additional information will be printed.
#' @param replace Sample with replacement
#' @export
#' @examples 
#' iris.data <- Dataset(iris, "Species", task = "Classification")
#' rforest <- InducerRandomForest(num.trees = 400)
#' mod <- rforest(cars.data)
#' 
#' mod <- InducerRandomForest(cars.data, num.trees = 400)
InducerRandomForest <- function(.data, min.node.size = NULL, max.depth = NULL, num.trees = 500, mtry = NULL, verbose = 0,
                                replace = TRUE) {
  .f <- function(.data) {
    start <- Sys.time()
    output <- ranger::ranger(as.formula(paste(names(metainfo(.data)$target), "~",
                                      paste(names(metainfo(.data)$feature), collapse = "+"))), data = .data$data,
                     min.node.size = min.node.size, max.depth = max.depth, num.trees = num.trees, mtry = mtry,
                     verbose = verbose, replace = replace)
    end <- Sys.time()
    structure(list(output = output, data = .data, config = configuration(.f), model = "rf",
                   training.time.sec = round(difftime(end, start, units = "s"))[[1]]),
              class = c("ModelRandomForest", paste0("Model", output$forest$treetype), "Model"))
  }
  class(.f) <- c("InducerRandomForest", "Inducer")
  
  if (!missing(.data)) {
    .f(.data)
  }
  .f
}
class(InducerRandomForest) <- c("InducerRandomForest", "Inducer")



hyperparameters.InducerXGBoost <- function(object, ...) {
  cat("Hyperparameter Space:\n")
  data.table(name = c("eta", "nrounds", "max_depth", "colsample_bytree", "colsample_bylevel", "lambda",
                             "alpha", "subsample", "verbose"),
                    type = c(rep("dbl", 8), "int"),
                    range = paste0("[", c(0, 1, rep(0, 7)), ", ", c(1, Inf, Inf, 1, 1, Inf, Inf, 1, 2), "]"))
}


hyperparameters.InducerRandomForest <- function(object, ...) {
  cat("Hyperparameter Space:\n")
  data.table(name = c("min.node.size", "max.depth", "num.trees", "mtry", "verbose", "replace"),
                    type = c(rep("int", 5), "log"),
                    range = c(paste0("[", c(1, 0, 1, 1, 0), ", ", c(Inf, 1, Inf, Inf, 1), "]"), "[FALSE, TRUE]"))
}


#' @title confuguration
#' @description configuration shows the defined hyperparameters of the inducer
#' @param x Inducer or Model object
#' @param ... Further arguments passed to or from other methods (currently ignored)
#' @export
configuration <- function(x, ...) {
  UseMethod("configuration")
}

configuration.Inducer <- function(x, ...) {
  hypernames <- ls(environment(x))
  defhyp <- lapply(hypernames, function(name) {
    environment(x)[[name]]
    })
  names(defhyp) <- hypernames
  defhyp[!vapply(defhyp, is.null, FUN.VALUE = TRUE)]
}

`configuration<-` <- function(x, value) {
  for (i in seq_along(value)) {
    environment(x)[[names(value)[[i]]]] <- value[[i]]
  }
  x
}

configuration.Model <- function(x, ...) {
  x$config
}

#' @title Inducer Print
#' @param x object of class Inducer
#' @param ... Further arguments passed to or from other methods (currently ignored)
#' @export
print.Inducer <- function(x, ...) {
 cat("Inducer:", regmatches(class(x)[[1]], regexpr("(?<=Inducer).*", class(x)[[1]], perl = TRUE)), "\n")
 cat("Configuartion:", paste0(paste(names(configuration(x)), "=", configuration(x)), collapse = ", "))
 invisible(x)
}

#' @title Model Print
#' @param x object of class Model
#' @param ... Further arguments passed to or from other methods (currently ignored)
#' @export
print.Model <- function(x, ...) {
  cat(strsplit(class(x)[[2]], "(?<=Model)", perl = TRUE)[[1]][c(2, 1)], ": '",
      regmatches(class(x)[[1]], regexpr("(?<=Model).*", class(x)[[1]], perl = TRUE)),
      "' fitted on '", x$data$name, "' dataset.\n", sep = "")
  invisible(x)
}

# environment mit allen inducer
ind <- new.env()
ind$xgboost <- InducerXGBoost
ind$rf <- InducerRandomForest


# Ausgabe des tatsächlichen Modell Outputs
modelObject <- function(x) {
  UseMethod("modelObject")
}

modelObject.Model <- function(x) {
  x$output
}


# anzeigen der Trainingsdauer
modelInfo <- function(x) {
  UseMethod("modelInfo")
}
modelInfo.Model <- function(x) {
  x["training.time.sec"]
}

# zugriff vom modell auf den inducer
inducer <- function(x) {
  UseMethod("inducer")
}
inducer.Model <- function(x) {
  do.call(ind[[x$model]], x$config)
}

# predict funktionen
predict.ModelXGBoost <- function(x, newdata) {
  if (any(colnames(newdata) %in% x$data$target)) {
    data.table("prediction" = predict(x$output,
                                       newdata = as.matrix(newdata[, !colnames(newdata) %in% x$data$target])),
               "truth" = newdata[, x$data$target])
  } else {
    predict(x$output, newdata = as.matrix(newdata))
  }
}

predict.ModelRandomForest <- function(x, newdata) {
  if (any(colnames(newdata) %in% x$data$target)) {
    data.table("prediction" = predict(x$output, newdata[, !colnames(newdata) %in% x$data$target])$predictions,
               "truth" = newdata[, x$data$target])
  } else {
    predict(x$output, newdata)$predictions
  }
}

