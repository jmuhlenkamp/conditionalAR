#' Compute Rolling Hit Rate From LBStart to LBEnd
#' Compute Rolling Hit Rate From LBStart to LBEnd.
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
#' @param lbstart The start of the lookback window.
#' @param lbend The end of the lookback window.
#' @param position Character for whether to return \cr
#'                 \itemize{
#'                 \item All "longshort" bets (-1, 0, 1 for all non-NA)
#'                 \item Only "long" bets (0, 1 for all non-NA)
#'                 \item Only "short" bets (-1, 0 for all non-NA)
#'                 \item "alwayslong" (1 for all non-NA obs)
#'                 \item "alwaysshort" (-1 for all non-NA obs)
#'                 }
#' @examples
#' growth <- diff(log(stocks))
#' signal <- rollAR(growth, rep(1/9,9))
#'
#' # Rolling 126-day long-short hitrate
#' lbhitrate(growth, signal, 126, 0, "longshort")
lbhitrate <- function(data, x, lbstart = 63, lbend = 0, position = "longshort"){
    lbapplyr(compute_hits(data, x, position), sum, lbstart, lbend) /
        lbapplyr(abs(compute_bets(x, position)), sum, lbstart, lbend)
}
