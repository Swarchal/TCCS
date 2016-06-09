context("theta0 works as expected")

test_that("theta0 returns expected values",{
	expect_equal(theta0(c(1, 0)), 0, tolerance = 1e-3)
	expect_equal(theta0(c(0, 1)), 90, tolerance = 1e-3)
	expect_equal(theta0(c(-1, 0)), 180, tolerance = 1e-3)
	expect_equal(theta0(c(0, -1)), 270, tolerance = 1e-3)
	expect_error(theta0(c(0, 1, 1)))

}
