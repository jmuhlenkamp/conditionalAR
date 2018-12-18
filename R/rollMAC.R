#' Apply an Moving Average Cross-over
#'
#' Apply a moving average crossover to one or multiple columns of an xts object
#' in a rolling fashion.
#'
#' @param x An xts object
#' @param mashort An integer indicating the number of periods in the short MA.
#' @param malong An integer indicating the number of periods in the long MA.
#'
rollMAC <- function(x, mashort, malong) {
    sign(rollmeanr(x, mashort, NA)
         - rollmeanr(x, malong, NA))
}
