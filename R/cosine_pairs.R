#'Calculate pairs of cosine similarities between replicates
#'
#' Given a list, with an element per compound, \code{cols}
#' are the elements of the vector with which to calculate the cosine
#' similarity. \code{cosine_pairs} will calculate the theta value between all
#' replicates for each possible pairing of compounds and return the result
#' in a long-format dataframe.
#'
#' @param x list, each element being a separate compound
#' @param cols integer, column indices that match numeric data (e.g principal
#'      comonents)
#' @param degrees boolean, if true will convert cosine similarity to degrees
#'
#' @importFrom utils combn
#' @importFrom gtools combinations
#'
#' @return dataframe of compound combinations across replicates with a column
#'    of cosine similarity values
#'
#' @export
#' @examples
#' cmpds <- c(rep('compound_1', 100),
#'            rep('compound_2', 100),
#'            rep('compound_3', 100))
#' replicate <- rep(1:100, 3)
#' PC1 <- rnorm(300)
#' PC2 <- rnorm(300)
#' df <- data.frame(cmpds, replicate, PC1, PC2)
#' df_split <- split(df, df$cmpds)
#' # works with unequal replicate sizes
#' df_split$a <- df_split$a[-c(1:10), ]
#' cosine_pairs(df_split, 3:4)

cosine_pairs <- function(x, cols, degrees = TRUE) {


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
    pairs_names <- combinations(n = length(name), 2, name,
                                repeats.allowed = TRUE)

    for (i in 1:nrow(pairs_names)) {

       tmp1 <- x[[pairs_names[i, 1]]]
       tmp2 <- x[[pairs_names[i, 2]]]


        # store values in a matrix
        mat <- matrix(nrow = nrow(tmp1),
                      ncol = nrow(tmp2))

        # loop through rows in cmpd A and cmpd B
        # calculate the cosine similarity between the two vectors
        # TODO write this loop in c++
        for (j in 1:nrow(tmp1)) {
            for (k in 1:nrow(tmp2)) {
                mat[j, k] <- cosine_sim_vector(tmp1[j, cols],
                                               tmp2[k, cols])
            }

        }

        # flatten the matrix into a single vector
        val <- as.vector(mat)

        # append theta values from each pair
        vals <- append(vals, val)

        # compound names
        A <- append(A, rep(pairs_names[i, 1], length(val)))
        B <- append(B, rep(pairs_names[i, 2], length(val)))

    }

    # convert cosine similarity to 0 -> 180 degrees
    if (degrees) {
        vals <- (1 - cossim_to_angsim(vals)) / (1/180)
    }
    data.frame(A, B, vals)
}


