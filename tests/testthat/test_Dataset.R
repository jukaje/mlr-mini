data <- Dataset(cars, target = "dist", task = "Regression")
data
testthat::expect_equal(data[1,2], 2)
testthat::expect_error(data[1,1])