library(data.table)
xgb <- function(.data, eta = NA, nrounds = NA, max_depth = NA, colsample_bytree = NA, colsample_bylevel = NA,
                lambda = NA, alpha = NA, subsample = NA, verbose = 0) {
 .f <- function(.data) {
   output
   structure()
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

configuration <- function(x) {
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
rm(xgb)
