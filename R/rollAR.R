#' Apply AR Coefficients
#'
#' Apply AR coefficients to one or multiple columns of an xts object
#' in a rolling fashion.
#'
#' @param x An xts object
#' @param coef Numeric vector of AR coefficients.
#'             First element corresponds to AR1.
#'
#' @examples
#' # 10-day MA crossover
#' growth <- diff(log(stocks))
#' signal <- rollAR(growth, rep(1/9,9))
rollAR <- function(x, coef){
    coef <- rev(coef) # Reverse to make coef argument more intuitive
    zoo::rollapplyr(x, width = length(coef), FUN = weighted.sum, w = coef)
}
