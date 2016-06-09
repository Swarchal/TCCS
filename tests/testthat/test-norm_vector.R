context("norm_vector")

test_that("norm vector returns errors when needed",{
	expect_error(norm_vector("string"))
	expect_error(norm_vector(0))
})

test_that("norm vector returns expected lengths",{
	expect_equal(norm_vector(c(1, 1)), 1.414214, tolerance = 1e-3)
	expect_equal(norm_vector(c(1, 0)), 1, tolerance = 1e-3)
	expect_equal(norm_vector(c(0,0)), 0, tolerance = 1e-3)
})
