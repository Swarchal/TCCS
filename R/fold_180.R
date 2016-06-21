#' Constrains numbers to 180
#'
#' Limits a numerical vector to 180. If an element in the vector is greater
#'  than 180 then the amount it is greater than 180 by is subtracted from it.
#'  Useful for angles between two vectors, if the angle between two vectors
#'  is 180 degrees this is the maximum difference -- once the angle exceeds
#'  180 degrees the vectors begin to converge again.
#'
#' @param x Vector of angles (in degrees)
#' @return Numerical vector
#'
#' @export
#'
#' @examples
#' sample <- seq(1, 360, 1)
#' fold_180(sample)


fold_180 <- function(x) {

    fold_180_check(x)

    out <- ifelse(x > 180,  360 - x, x)
    return(out)

}


#' check input for fold_180
#' @param x numeric vector

fold_180_check <- function(x) {

    if (!is.vector(x)) {
        stop("Expecting a vector")
    }
    if (!is.numeric(x)) {
        stop("Expecting numerical values")
    }
    if (max(x) > 360) {
        warning("Input values exceed 360, will produce negative values")
    }
    if (min(x) < 0) {
        warning("Input values > 0")
    }

}
