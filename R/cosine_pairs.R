#'Calculate pairs of cosine similarities between replicates
#'
#' Given a list, with an element per compound, \code{cols}
#' are the elements of the vector with which to calculate the cosine
#' similarity
#' @param x list
#' @param cols integer, column indices
#'
#' @importFrom utils combn
#'
#' @return dataframe of compound combinations across replicates with a column
#'    of cosine similarity values
#'
#' @export
#'
#' @examples
#' cmpds <- c(rep('a', 100), rep('b', 100), rep('c', 100))
#' replicate <- rep(1:100, 3)
#' PC1 <- rnorm(300)
#' PC2 <- rnorm(300)
#'
#' df <- data.frame(cmpds, replicate, PC1, PC2)
#'
#' df_split <- split(df, df$cmpds)
#'
#' # works with unequal replicate sizes
#' df_split$a <- df_split$a[-c(1:10), ]
#'
#' cosine_pairs(df_split, 3:4)


cosine_pairs <- function(x, cols) {

    if (!is.list(x) || is.data.frame(x)) {
       stop(paste(substitute(x), "should be a list"),
            call. = FALSE)
    }


    # initialise empty vectors
    # TODO pre-allocate vector lengths
    vals <- numeric()
    A <- character()
    B <- character()

    # get pairs of compounds
    name <- names(x)
    pairs_names <- t(combn(name, 2))

    for (i in 1:nrow(pairs_names)) {
       tmp1 <- x[[pairs_names[i, 1]]]
       tmp2 <- x[[pairs_names[i, 2]]]

        # loop through rows in cmpd A and cmpd B
        # calculate the cosine similarity between the two vectors
        for (j in 1:nrow(tmp1)) {

            val <- numeric(length = nrow(tmp2))

           for (k in 1:nrow(tmp2)) {
               val[k] <- cosine_sim_vector(tmp1[j, cols], tmp2[k, cols]))

      }

        vals <- append(vals, val)
        A <- append(A, rep(pairs_names[i, 1]), nrow(tmp2))
        B <- append(B, rep(pairs_names[i, 2]), nrow(tmp2))

    }
  }
    data.frame(A, B, vals)
}