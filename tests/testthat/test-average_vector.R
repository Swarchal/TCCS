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
