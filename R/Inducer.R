library(data.table)
library(xgboost)
xgb <- function(.data, eta = NA, nrounds = NA, max_depth = NA, colsample_bytree = NA, colsample_bylevel = NA,
                lambda = NA, alpha = NA, subsample = NA, verbose = 0) {
 .f <- function(.data) {
   if (is.na(nrounds)) {nrounds <- 1}
   start <- Sys.time()
   output <- xgboost(data = as.matrix(.data$data[, names(metainfo(.data)$features)]),
                     label = .data$data[, names(metainfo(.data)$targets)], verbose = verbose, nrounds = nrounds,
                     params = configuration(.f)[!names(configuration(.f)) %in% c("verbose", "nrounds")])
   end <- Sys.time()
   
   structure(list(output = output, data = .data, config = configuration(.f), model = "xgboost",
                  training.time.sec = round(difftime(end, start, units = "s"))[[1]]),
             class = c("ModelXGBoost", "ModelRegression", "Model"))
 }
  
  if (!missing(.data)) {
   .f(.data)
 }
 structure(.f, class = c("InducerXGBoost", "Inducer"))
}

InducerXGBoost <- structure(xgb, class = c("InducerXGBoost", "Inducer"))

hyperparameters <- function(x) {
  cat("Hyperparameter Space:\n")
  if (any(class(x) == "InducerXGBoost")) {
    return(data.table(name = c("eta", "nrounds", "max_depth", "colsample_bytree", "colsample_bylevel", "lambda",
                               "alpha", "subsample", "verbose"),
                      type = c(rep("dbl", 8), "int"),
                      range = paste0("[", c(0, 1, rep(0, 7)), ", ", c(1, Inf, Inf, 1, 1, Inf, Inf, 1, 2), "]")))
  }
}

configuration <- function(x, ...) {
  UseMethod("configuration")
}
configuration.default <- function(x) {
  hypernames <- ls(environment(x))
  defhyp <- lapply(hypernames, function(name) {
    environment(x)[[name]]
    })
  names(defhyp) <- hypernames
  defhyp[!is.na(defhyp)]
}

`configuration<-` <- function(x, value) {
  for (i in seq_along(value)) {
    environment(x)[[names(value)[[i]]]] <- value[[i]]
  }
  x
}

print.Inducer <- function(x, ...) {
 cat("Inducer:", regmatches(class(x)[[1]], regexpr("(?<=Inducer).*", class(x)[[1]], perl = TRUE)), "\n")
 cat("Configuartion:", paste0(paste(names(configuration(x)), "=", configuration(x)), collapse = ", "))
 invisible(x)
}

ind <- new.env()
ind$xgboost <- InducerXGBoost

print.ModelRegression <- function(x, ...) {
  cat("Regression Model: '", regmatches(class(x)[[1]], regexpr("(?<=Model).*", class(x)[[1]], perl = TRUE)),
      "' fitted on '", x$data$name, "' dataset.\n", sep = "")
  invisible(x)
}

modelObject <- function(x) {
  x$output
}

modelInfo <- function(x) {
  x["training.time.sec"]
}
configuration.Model <- function(x) {
  x$config
}

inducer <- function(x) {
  do.call(ind[[x$model]], x$config)
}

predict.ModelXGBoost <- function(x, newdata) {
  if (any(colnames(newdata) %in% x$data$target)) {
    data.table("predicition" = predict(x$output,
                                       newdata = as.matrix(newdata[, !colnames(newdata) %in% x$data$target])),
               "truth" = newdata[, x$data$target])
  } else {
    predict(x$output, newdata = as.matrix(newdata))
  }
}

rm(xgb)
