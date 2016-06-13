context("average_vector works as expected")

test_that("Stops when given incorrect input", {
	expect_error(average_vector(1,2,3))
	expect_error(average_vector(c(1, 2, 3)))
	expect_error(average_vector("string"))
	expect_error(average_vector(iris))
})

test_that("Returns expected output", {
	simple <- colMeans(iris[, 1:4])
	out <- average_vector(iris[, 1:4])
	expect_equal(as.vector(simple), out)
})

test_that("fun argument works", {
	x <- matrix(c(1,5,2,10,3,10), ncol = 2)
	mean_out <- c(2.66667, 7.66667)
	median_out <- c(2, 10)
	expect_equal(average_vector(x, fun = "mean"), mean_out, tolerance = 1e-4)
	expect_equal(average_vector(x, fun = "median"), median_out, tolerance = 1e-4)
	expect_error(average_vector(x, fun = "invalid"),
		"invalid is not a valid function, use mean of median",
		fixed = TRUE)
})

test_that("additional arguments are passed", {
	x <- matrix(c(1,5,2,10,NA,10), ncol = 2)
	expect_equal(average_vector(x), c(2.66667, NA), tolerance = 1e-4)
	expect_equal(average_vector(x, fun = "mean", na.rm = TRUE),
		c(2.66667, 10), tolerance = 1e-4)
})