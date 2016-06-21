#' Shift co-ordinates to centre on a compounds centroid
#'
#' Idea is to shift co-ordinates so that the centroid of the negative control
#' is always centered on 0,0. Useful for PCA biplots where the 0,0 denotes the
#' centroid of the entire data, and not necessarily the negative control. If
#' given a dataframe and specified columns for the x and y co-ordinates, this
#' function will shift the co-ordinates to be centered on the centroid of a
#' specified compound. The difference between \code{centre_control} and
#' \code{centre_controls_m} is that \code{centre_control_m} can centre a range
#' or columns, whereas \code{centre_control} is limited to 2 named columns.
#'
#' @param df dataframe
#' @param cols column indicies or names
#' @param cmpd_col column containing names to subset with \code{cmpd}
#' @param cmpd name of compound to subset \code{cmpd_col} with
#'
#' @return dataframe of 'df' shifted so that
#'   cmpd is centered on 0,0
#'
#' @export
#'
#' @examples
#' # artifical example using iris
#' pca <- prcomp(iris[,1:4])$x[,1:3]
#' df <- data.frame(pca, name = iris[,5])
#' pca_shift <- centre_control(df, 1:3,  cmpd_col = 'name', cmpd = 'setosa')
#'
#' par(mfrow = c(2,1))
#' plot(df$PC1, df$PC2, col = df$name, pch = 20)
#' plot(pca_shift$PC1, pca_shift$PC2, col = pca_shift$name, pch = 20)

centre_control <- function(df, cols, cmpd_col, cmpd) {

    # find DMSO centroids in each value
    centroids <- apply(df[df[, cmpd_col] == cmpd, cols], 2, mean)

    # shift from centre
    d <- 0 - centroids

    # shift values in columns
    for (i in seq_along(cols)){
        df[, cols][i] <- df[, cols][i] + d[i]
    }

    return(df)
}