context("cosine_sim")

test_that("cosine_sim_vector returns errors when expected",{
	a10 <- rnorm(10)
	b20 <- rnorm(20)
	expect_error(cosine_sim_vector(a10, b20))
	expect_error(cosine_sim_vector(c(1,2), c("a", "b")))
	expect_error(cosine_sim_vector(list(1,2), list(1,2)))
	expect_error(cosine_sim_vector(iris, mtcars))
	expect_error(cosine_sim_vector(c("a, b"), c("c", "d")))
	expect_error(cosine_sim_vector(iris[, 1:4], iris[1, 1:4]))
	expect_error(cosine_sim_vecot(iris[1:2, 1:4], iris[1, 1:4]))
	expect_error(cosine_sim_vector(iris[1:2, 1:4], iris[2:4, 1:4]))
})

test_that("cosine_sim_matrix returns errors when expected",{
	expect_error(cosine_sim_mat(iris))
	expect_error(cosine_sim_mat("string"))
})

test_that("cosine_sim returns errors when expected",{
	expect_error(cosine_sim(c(1,3), c(1,2), c(2,33)))

})

test_that("cosine_sim_vector returns expected answers",{
	expect_equal(cosine_sim_vector(iris[1, 1:4], iris[1, 1:4]), 1)
	expect_equal(cosine_sim_vector(c(1,2), c(1,2)), 1)
	expect_equal(cosine_sim_vector(c(1,0), c(-1,0)), -1)
	expect_equal(cosine_sim_vector(c(1,0), c(0,1)), 0)
})

test_that("cosine_sim_mat returns expected answers",{
	mat <- matrix(rep(seq(1, 10, 1), 10), ncol = 10)
	mat_out <- matrix(rep(1, 100), ncol = 10)
	expect_equal(cosine_sim_mat(mat), mat_out)

	set.seed(123)
	mat <- matrix(rnorm(100), ncol = 10)
	out <- cosine_sim_mat(mat)
	diag_out <- diag(out)
	expect_equal(diag_out, rep(1, 10))
	expect_is(out, 'matrix')
	expect_equal(dim(out), c(10, 10))
})

test_that("cosine_sim returns expected answers",{
	out_mat <- cosine_sim(iris[, 1:4])
	expect_is(out_mat, 'matrix')

	out_vector <- cosine_sim(c(1,2,3), c(3,2,1))
	expect_true(is.vector(out_vector))
})
