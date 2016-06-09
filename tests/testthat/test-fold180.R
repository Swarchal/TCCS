context("fold_180")

test_that("fold_180 stops when given incorrect input",{
	expect_error(fold_180(c("string1", "string2")))
	expect_error(fold_180(iris))
	expect_warning(fold_180(seq(1, 500, 1)))
	expect_warning(fold_180(c(1, 120, -2)))
})

test_that("fold 180 returns expected values",{
	numbers <- c(170, 180, 190, 0)
	outcome <- c(170, 180, 170, 0)
	expect_equal(fold_180(numbers), outcome)
})
