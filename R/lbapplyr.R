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
#' # Rolling 10-day long-short hitrate
#' hits <- lbapplyr(compute_hits(growth, signal, "longshort"), sum, 9, 0)
#' bets <- lbapplyr(abs(compute_bets(signal, "longshort")), sum, 9, 0)
#' hr_ls <- hits/bets
#'
#' # Rolling 10-day long hitrate
#' hits <- lbapplyr(compute_hits(growth, signal, "long"), sum, 9, 0)
#' bets <- lbapplyr(abs(compute_bets(signal, "long")), sum, 9, 0)
#' hr_long <- hits/bets
#'
#' # Rolling 10-day uprate
#' hits <- lbapplyr(compute_hits(growth, signal, "alwayslong"), sum, 9, 0)
#' bets <- lbapplyr(abs(compute_bets(signal, "alwayslong")), sum, 9, 0)
#' hr_up <- hits/bets
#'
#' # Rolling 10-day short hitrate
#' hits <- lbapplyr(compute_hits(growth, signal, "short"), sum, 9, 0)
#' bets <- lbapplyr(abs(compute_bets(signal, "short")), sum, 9, 0)
#' hr_short <- hits/bets
#'
#' # Rolling 10-day downrate
#' hits <- lbapplyr(compute_hits(growth, signal, "alwaysshort"), sum, 9, 0)
#' bets <- lbapplyr(abs(compute_bets(signal, "alwaysshort")), sum, 9, 0)
#' hr_down <- hits/bets
lbapplyr <- function(data, FUN, lbstart = 63, lbend = 0, ...){
    lagn <- lbend
    width <- lbstart - lbend + 1
    rollapplyr(lag.xts(data, lagn), width = width, FUN = FUN, ...)
}
