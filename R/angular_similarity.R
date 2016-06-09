#' Angular similarity
#'
#' The normalised angle between two vectors, bounded between 0 and 1.
#' A value of 1 indicates the two vectors have a similarity of 0 degrees.
#' While two vectors diametrically opposed have a value of 0.
#'
#' @param a Vector
#' @param b Vector
#' @return out Angle similarity, between 0 and 1
#'
#' @export
#'
#' @examples
#' a <- c(-1, 0)
#' b <- c(-1, -1)
#' angular_similarity(a, b)
#'
#' # Vectors of length > 2
#' x <- c(1, 4, 2, 2.5)
#' y <- c(5, 200, 4, -1)
#' angular_similarity(x, y)

angular_similarity <- function(a, b) {

    ang_sim_check(a, b)
    similarity <- as.vector(a %*% b / sqrt(sum(a^2) * sum(b^2)))
    out <- 1 - ((acos(similarity)/ pi))
    return(out)
    
}

#' check inputs for angular_angular similarity
#' 
#' helper function
#' 
#' @param a vector
#' @param b vector

ang_sim_check <- function(a, b) {
    if (!is.vector(a) || !is.vector(b)){
        stop("Inputs need to be vectors")
    }
    if (!is.numeric(a) || !is.numeric(b)){
        stop("Inputs need to be numeric")
    }
    if (length(a) != length(b)){
        stop(paste(
            substitute(a), "and", substitute(b), "need to be the same length")
        )
    }
    if (length(a) < 2 || length(b) < 2){
        stop("Vectors need to be at least 2 elements long")
    }
}