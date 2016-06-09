context("euclid dist works")

test_that("returns errors when expected",{
	  expect_error(euclid_dist(c(1,2,3), c(1,2)))
	  expect_error(euclid_dist(c("a", "b"), c(1,2)))
	  expect_error(euclid_dist(c(1,2,3,4)))
})

test_that("returns expected answer",{
	  expect_equal(euclid_dist(c(1,2,3,4), c(1,2,3,4)), 0)
	  expect_equal(euclid_dist(c(1, 0), c(0, 1)), 1.414214, tolerance = 1e-5)
})
