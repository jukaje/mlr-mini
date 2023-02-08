testthat::expect_equal(Dataset(cars, target = "dist")[1,2], 2)
testthat::expect_error(Dataset(cars, target = "dist")[1,1])