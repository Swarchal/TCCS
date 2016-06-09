#' Euclidean distance between two vectors
#'
#' As \code{dist} in base R is idiotic.
#'
#' @param x numeric vector
#' @param y numeric vector
#' @importFrom stats dist
#' @export
#' @examples
#' a <- rnorm(100)
#' b <- rnorm(100)
#' euclid_dist(a, b)

euclid_dist <- function(x, y) {

    stopifnot(length(x) == length(y))
    stopifnot(is.numeric(x) && is.numeric(y))
    as.numeric(dist(rbind(as.numeric(x), as.numeric(y))))

}
