test_that("hp() returns the correct object", {
  hp1 <- hp(hp1 = p_dbl(0, 10))
  expect_true(class(hp1) == "hp")
  expect_equal(hp1$name, "hp1")
  expect_equal(hp1$type, "dbl")
  expect_equal(hp1$range[[1]], c(0, 10))
})


test_that("hp() returns the correct object", {
  hp1 <- hp(hp1 = p_dbl(0, 10), hp2 = p_int(5, 10))
  expect_true(class(hp1) == "hp")
  expect_equal(hp1$name[[2]], "hp2")
  expect_equal(hp1$type[[1]], "dbl")
  expect_equal(hp1$type[[2]], "int")
  expect_equal(hp1$range[[1]], c(0, 10))
  expect_equal(hp1$range[[2]], c(5, 10))
})


test_that("checkHyperparameter() returns the correct result", {
  hpx <- hp(hp1 = p_dbl(0, 10))
  expect_true(checkHyperparameter(hps = list(hp1 = 2.5), hpx))
  expect_false(checkHyperparameter(hps = list(hp1 = 11), hpx))
})

