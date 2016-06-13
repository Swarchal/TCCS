#' Calculates and average vector
#'
#' Calculates an average vector from rows of multiple vectors
#'
#' @param x Matrix of numerical data. Columns are components of the data, row per vector
#' @param fun function to use, current options: mean, median
#' @param ... additional arguments to be passed to fun
#' @return Mean vector
#'
#' @export
#'
#' @examples
#' x <- matrix(c(1,5,2,10,3,10), ncol = 2)
#' average_vector(x)
#' average_vector(x, fun = "median")
#' average_vector(x, fun = "median", na.rm = TRUE)

average_vector <- function(x, fun = "mean", ...) {

	# check input
	if (!is.matrix(x) && !is.data.frame(x)){
		stop("Input needs to be a dataframe or a matrix")
	}

	# data will be in the form of rows = vectors, columns = components
	if (fun == "mean") {
		as.vector(colMeans(x, ...))
	} else if (fun == "median"){
		as.vector(colMedians(x, ...))
	} else {
		stop(paste(substitute(fun), "is not a valid function, use mean of median"))
	}
}


#' column median
#' 
#' interal function
#' 
#' @param x dataframe of matrix
#' @param ... additional arguments to be passed to median

colMedians <- function(x, ...) {
	as.numeric(apply(x, 2, median, ...))
}