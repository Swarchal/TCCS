context("check cosine pairs")

cmpds <- c(rep('a', 100), rep('b', 100), rep('c', 100))
replicate <- rep(1:100, 3)
PC1 <- rnorm(300)
PC2 <- rnorm(300)

df <- data.frame(cmpds, replicate, PC1, PC2)

df_split <- split(df, df$cmpds)
out <- cosine_pairs(df_split, 3:4)

# works with unequal replicate sizes
df_split$a <- df_split$a[-c(1:10), ]
out_unequal <- cosine_pairs(df_split, 3:4)


test_that("cosine_pairs returns expected output",{
	expect_true(is.data.frame(out_unequal))
	expect_equal(nrow(out), 60000)
	expect_equal(ncol(out), 3)
	expect_equal(nrow(out_unequal), 56100)
})


test_that("cosine_pairs returns correct results", {

    # known answers:
    A <- c('a', 'a', 'b', 'b')
    PC1 <- c(1, 1, -1, -1)
    PC2 <- c(2, 2, -2, -2)
    df <- data.frame(A, PC1, PC2)
    split_df <- split(df, df$A)
    test <- cosine_pairs(split_df, 2:3)

	expect_equal(test$val, c(1, 1, 1, 1,
                             -1, -1, -1, -1,
                             1, 1, 1, 1),
                 tolerance = 1e-5)
})


test_that("cosine_pairs returns correct results", {

    # known answers
    # with orthogonal vectors
    A <- c("a", "a", "b", "b", "c", "c")
    PC1 <- c(1, 1, -1, -1, 2, 2)
    PC2 <- c(2, 2, -2, -2, -1, -1)
    df <- data.frame(A, PC1, PC2)
    df_split <- split(df, df$A)
    test <- cosine_pairs(df_split, 2:3)

    ans <- c(1, 1, 1, 1,
            -1, -1, -1, -1,
            0, 0, 0, 0,
            1, 1, 1, 1,
            0, 0, 0, 0,
            1, 1, 1, 1)

    expect_equal(test$vals, ans, tolerance = 1e-7)
})

test_that("cosine_pairs works with more than 2 dimensions", {

    # known answers
    # three principal components
    A <- c("a", "a", "b", "b")
    PC1 <- c(1, 1, -1, -1)
    PC2 <- c(2, 2, -2, -2)
    PC3 <- c(1, 1, -1,  -1)
    df <- data.frame(A, PC1, PC2, PC3)
    df_split <- split(df, df$A)
    test <- cosine_pairs(df_split, 2:4)

    ans <- c(1, 1, 1, 1,
            -1, -1, -1, -1,
             1, 1, 1, 1)

    expect_equal(ncol(test), 3)
    expect_is(test, "data.frame")
    expect_equal(test$vals, ans)

})

# new algorithm alters the order of results
# answer from original algorithm
# set.seed(12321)
# cmpds <- c(rep('a', 100), rep('b', 100), rep('c', 100))
# replicate <- rep(1:100, 3)
# PC1 <- rep(1, 300)
# PC2 <- rep(1, 300)
#
# df <- data.frame(cmpds, replicate, PC1, PC2)
#
# df_split <- split(df, df$cmpds)
#
# # works with unequal replicate sizes
# df_split$a <- df_split$a[-c(1:10), ]
# #
# cosine_pairs_orig <- function(x, a, b){
#
#     if (!is.list(x) || is.data.frame(x)){
# 	stop("Expecting a list", call. = FALSE)
#     }
#
#     # initialise empty vectors
#     vals <- numeric()
#     A <- character()
#     B <- character()
#
#     # get pairs of compounds
#     name <- names(x)
#     pairs_names <- t(combn(name, 2))
#
#     for (i in 1:nrow(pairs_names)){
# 	tmp1 <- x[[pairs_names[i, 1]]]
# 	tmp2 <- x[[pairs_names[i, 2]]]
#
# 	# loop through rows in cmpd A and cmpd B
# 	# calculate the cosine similarity between the two vectors
# 	for (j in 1:nrow(tmp1)){
# 	    for (k in 1:nrow(tmp2)){
# 		vals <- c(vals, cosine_sim_vector(
# 				    c(tmp1[j, a], tmp1[j, b]),
# 				    c(tmp2[k, a], tmp2[k, b])))
#
# 		A <- c(A, pairs_names[i, 1])
# 		B <- c(B, pairs_names[i, 2])
# 	  }
# 	}
#       }
#     data.frame(A, B, vals)
# }
#
# out_orig <- cosine_pairs_orig(x = df_split, a = 'PC1', b = 'PC2')
#
# # check same as original
# out_current <- cosine_pairs(df_split, 3:4)
#
# test_that("current alg returns same answers as original version",{
#     expect_true(all(out_orig == out_current))
# })
