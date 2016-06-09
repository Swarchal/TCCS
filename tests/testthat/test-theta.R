context("theta returns expected angles")

test_that("theta returns error when expected",{
	expect_error(theta(c(1,2,1), c(1,2)))
})

test_that("theta returns expected values",{
	expect_equal(theta(c(1,2), c(1,2)), 0, tolerance=1e-3)
	expect_equal(theta(c(1,0), c(-1, 0)), 180, tolerance=1e-3)
	expect_equal(theta(c(1,0), c(0, 1)), 90, tolerance=1e-3)
})
