#' Angle between two vectors
#'
#' Calculates the angle between two vectors, output in degrees
#'
#' @param a Vector
#' @param b Vector
#'
#' @return Angle in degrees
#'
#' @export
#'
#' @examples
#' a <- c(1, 2)
#' b <- c(3, 1)
#'
#' theta(a,b)
#'
#' a <- c(1, 2)
#' b <- c(-1, -2)
#' theta(a, b)

theta <- function(a, b) {

    as.vector(acos(a %*% b / (norm_vector(a) * norm_vector(b)))) * 180 / pi
}




#' norm of a vector
#'
#' @param x vector

 # length/norm of a vector 'x'

norm_vec <- function(x) {

    sqrt(x %*% x)

}



#' Angle between a vector and the origin
#'
#' Calculates the angle (in degrees) between a vector and the origin (1,0)
#'
#' @param a Vector
#' @return Angle in degrees
#'
#' @export
#'
#' @examples
#' a <- c(1, 0)
#' theta0(a)
#'
#' a <- c(-1, 0)
#' theta0(a)
#'
#' a <- c(-1, -1)
#' theta0(a)


theta0 <- function(a) {

    if (length(a) != 2) {
        stop("input vector needs to have a length of 2")
    }

    # origin vector
    origin <- c(1, 0)

    theta <- as.vector(
        acos(a %*% origin / (norm_vec(a) * norm_vec(origin))) * 180/pi
        )

    # need to measure the angle from a set reference, so it's always
    # anticlockwise otherwise theta never exceed 180

    if (a[2] >= 0) {
        return(theta)
    }

    if (a[2] <0) {
        return(360 - theta)
    }

}




#' Angle between the vector and the origin
#'
#' Calculates and angle (in degrees) between the vector and the origin
#' \code{c(1, rep(0, length(vector) - 1))}
#'
#' @param a numeric vector
#' @export

theta0_ <- function(a){

    origin <- c(1, rep(0, (length(a) - 1)))
    stopifnot(length(a) == length(origin))
    as.vector(acos(a %*% origin / (norm_vec(a) * norm_vec(origin))) * 180 / pi)
}
