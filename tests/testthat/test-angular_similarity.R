context("angular_similarity")

test_that("angular similarity stops when given incorrect input", {
	expect_error(angular_similarity(1, 2))
	expect_error(angular_similarity(c(1, 2), 1))
	expect_error(angular_similarity(c("string"), c(1, 2)))
	expect_error(angular_similarity(c("string", "string"), c("string", "string")))
	expect_error(angular_similarity(c(1, 2, 3), c(1, 2)))
	expect_error(angular_similarity(list(1,3,4), c(1,2,3)))
	expect_error(angular_similarity(list(1,2), list(1,2)))
	expect_error(angular_similarity(iris[, 1:4], iris[, 1:4]))
})

test_that("angular similarity returns expected answers", {
	a <- angular_similarity(c(1, 2), c(1, 2))
	expect_equal(a ,1)

	b <- angular_similarity(c(10, 10, 10), c(10, 10, 10))
	expect_equal(a, b)

	expect_equal(angular_similarity(c(1, 0), c(-1, 0)), 0)
})
