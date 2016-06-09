#' Calculates and average vector
#'
#' Calculates an average vector from rows of multiple vectors
#'
#' @param x Matrix of numerical data. Columns are components of the data, row per vector
#' @return Mean vector
#'
#' @export
#'
#' @examples
#' x <- matrix(c(1,5,2,10,3,10), ncol = 2)
#' average_vector(x)

average_vector <- function(x) {

	# check input
	if (!is.matrix(x) && !is.data.frame(x)){
		stop("Input needs to be a dataframe or a matrix")
	}

	# data will be in the form of rows = vectors, columns = components
	as.vector(colMeans(x))
}
