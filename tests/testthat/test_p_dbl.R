testthat::expect_equal(p_dbl(1, 3), list(type = "dbl", start = 1, end = 3))

test_that("p_fct() returns a list with the correct type and levels", {
  expect_equal(p_fct(c("A", "B", "C")), list(type = "fct", levels = c("A", "B", "C")))
})



test_that("p_int works correctly", {
  expect_equal(p_int(1, 10), list(type = "int", start = 1, end = 10))
  expect_error(p_int("a", 10), "range1 is not an integer")
  expect_error(p_int(1, "b"), "range2 is not an integer")
})