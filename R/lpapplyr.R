#' Apply Rolling Functions From LBStart to LBEnd
#' Wrapper around rollapplyr so that we can easily apply rolling functions
#' over lagged windows
#' @param data An `xts` object that is passed to `rollapplyr`.
#' @param FUN The function to be applied.
#' @param lbstart The start of the lookback window.
#' @param lbend The end of the lookback window.
#' @param ... Other arguments passed to rollapplyr.
#' @examples
#' growth <- diff(log(stocks))
#' signal <- rollAR(growth, rep(1/9,9))
#' lbapplyr(abs(compute_bets(signal, "all")), sum, 63, 0)
#' lbapplyr(abs(compute_bets(signal, "long")), sum, 63, 0)
#' lbapplyr(abs(compute_bets(signal, "short")), sum, 63, 0)
#' lbapplyr(abs(compute_bets(signal, "n")), sum, 63, 0)
lbapplyr <- function(data, FUN, lbstart = 63, lbend = 1, ...){
    lagn <- lbend - 1
    width <- lbstart - lbend + 1
    rollapplyr(lag(data, lagn), width = width, FUN = FUN, ...)
}
