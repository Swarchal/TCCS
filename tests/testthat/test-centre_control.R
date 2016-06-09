context("test centre_control")

pca <- prcomp(iris[,1:4])$x[,1:2]
df <- data.frame(pca, name = iris[,5])
pca_shift <- centre_control(df, cols = 1:2,
			    cmpd_col = 'name', cmpd = 'setosa')

test_that("centre_control returns errors when expected", {

	expect_error(
		centre_control(as.matrix(df, x = 'PC1', y = 'PC2',
			cmpd_col = 'name', cmpd = 'setosa'))
		)

	expect_error(
		centre_control(df, x = 'PC10', y = 'PC2',
			cmpd_col = 'name', cmpd = 'setosa')
		)

	expect_error(
		centre_control(df, x = 'PC1', y = 'PCnone',
			cmpd_col = 'name', cmpd = 'setosa')
		)


	expect_error(
		centre_control(df, x = 'PC1', y = 'PC2',
			cmpd_col = 'not here', cmpd = 'setosa')
		)

	expect_error(
		centre_control(df, x = 'PC1', y = 'PC2',
			cmpd_col = 'name', cmpd = 'not there')
		)
})


test_that("centre_control returns expected", {

	expect_true(is.data.frame(pca_shift))
	expect_equal(nrow(pca_shift), 150)
	expect_equal(ncol(pca_shift), 3)

	setosa_av <- pca_shift[pca_shift$name == 'setosa', c("PC1", "PC2")]
	expect_equal(as.numeric(colMeans(setosa_av)), c(0, 0), tolerance = 1e-4)
})
