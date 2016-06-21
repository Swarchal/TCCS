#' Creates a cosine similarity matrix
#'
#' For a given matrix, will create a cosine similarity matrix. Used as an
#' internal function for \code{cosine_sim()}
#'
#' @param X matrix
#' @param ... optional arguments to sum (i.e na.rm = TRUE)
#'
#' @return C cosine similarity matrix
#'
#' @export
#'
#' @examples
#' # example data
#' mat <- iris[,1:4]
#' out <- cosine_sim_mat(mat)
#' image(out)


cosine_sim_mat <- function(X, ...) {

	cos_sim <- function(ix, ...) {
	    A <- X[ix[1],]
	    B <- X[ix[2],]
	    sum(A*B) / sqrt(sum(A^2, ...) * sum(B^2, ...))
	}

	# if given a dataframe, will try and convert to a matrix
	if (is.data.frame(X)) {
		X <- as.matrix(X)
	}

	# check input
	if (!is.matrix(X)) stop("X needs to be a matrix")
	if (!is.numeric(X)) stop("X needs to be numeric")


    n <- nrow(X)
    cmb <- expand.grid(i = 1:n, j = 1:n)
    matrix(apply(cmb, 1, cos_sim), n, n)

}


#' Cosine similarity between two vectors
#'
#' Given two vectors, this function will calculate a cosine similarity.
#' Used as an internal function for \code{cosine_sim()}
#'
#' @param a vector
#' @param b vector
#' @param ... optional arguments to sum (i.e na.rm = TRUE)
#'
#' @return out cosine similarity
#'
#' @export
#'
#' @examples
#' set.seed(12321)
#' x <- rnorm(20)
#' y <- rnorm(20)
#' cosine_sim_vector(x, y)

cosine_sim_vector <- function(a, b, ...) {

    # is single row dataframe, convert to a numeric vector
    if (is.data.frame(a) && nrow(a) == 1) {
	    a <- as.numeric(a)
    }
    if (is.data.frame(b) && nrow(b) == 1) {
	    b <- as.numeric(b)
    }

	cosine_sim_vec_check_input(a, b)

    sum(a * b, ...) / sqrt(sum(a^2, ...) * sum(b^2, ...))

}

#' Check inputs for cosine_sim_vector
#'
#' helper function for \code{cosine_sim_vector}
#'
#' @param a vector
#' @param b vector

cosine_sim_vec_check_input <- function(a, b) {

	if (!is.vector(a) || !is.vector(b)) {
		stop("Inputs need to be vectors")
	}
	if (!is.numeric(a) || !is.numeric(b)) {
		stop("Inputs need to be numeric")
	}
	if (length(a) != length(b)) {
		stop("'a' and 'b' need to be the same length")
	}

}

#' Cosine similarity
#'
#' Calculates a cosine similarity of either a matrix or between two vectors.
#' If given a matrix, will return a cosine-similarity matrix. If given two
#' vectors will calculate the cosine-similarity between the two. Specific
#' functions for cosine vectors or matrices can be called with
#' \code{cosine_sim_vector} and \code{cosine_sim_mat} respectively.
#'
#' @param ... Either a numerical matrix, or two numerical vectors of the same
#' 		length.
#'
#' @return out A matrix if given a matrix, or a single number if given two
#' 		vectors.
#'
#' @export
#'
#' @examples
#' # example matrix
#' mat <- as.matrix(iris[,1:4])
#' out <- cosine_sim(mat)
#' image(out)
#'
#' # two vectors
#' a <- rnorm(20)
#' b <- rnorm(20)
#' cosine_sim(a, b)

cosine_sim <- function(...) {

    dots <- list(...)
    if (length(dots) == 1) {
        # a matrix
        cosine_sim_mat(dots[[1]])
    } else if (length(dots) == 2) {
    	# two vectors
        cosine_sim_vector(dots[[1]], dots[[2]])
    } else stop("Need either a matrix or two vectors")

}
