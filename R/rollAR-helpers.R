#' Weighted Arithmetic Sum
#'
#' Compute a weighted sum.
#'
#' @param x an object containing the values whose weighted sum is to
#'          be computed.
#' @param w a numerical vector of weights the same length as x giving
#'            the weights to use for elements of x.
#' @param ... arguments to be passed to or from methods.
#' @param na.rm a logical value indicating whether NA values in x
#'              should be stripped before the computation proceeds.
#'
weighted.sum <- function (x, w, ..., na.rm = FALSE)
{
    # stats:::weighted.mean.default, but sum instead of mean
    if (missing(w)) {
        if (na.rm)
            x <- x[!is.na(x)]
        return(sum(x))
    }
    if (length(w) != length(x))
        stop("'x' and 'w' must have the same length")
    w <- as.double(w)
    if (na.rm) {
        i <- !is.na(x)
        w <- w[i]
        x <- x[i]
    }
    sum((x * w)[w != 0])
}
