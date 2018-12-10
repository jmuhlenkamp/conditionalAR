#' Compute Binary Bets
#' Takes predictions and creates lagged -1, 0, 1 values based on sign.
#' @param x An `xts` object containing predictions.
#' @param position Character for whether to return \cr
#'                 \itemize{
#'                 \item "all" bets (-1, 0, 1 for all non-NA)
#'                 \item Only "long" bets (0, 1 for all non-NA)
#'                 \item Only "short" bets (-1, 0 for all non-NA)
#'                 \item n (1 for all non-NA obs)
#'                 }
compute_bets <- function (x, position = c("all", "long", "short", "n")) {
    position <- position[1]
    data <- xts::lag.xts(sign(x))
    if (position == "all"){
        return(data)
    } else if (position == "long"){
        return(base::pmax(data, 0))
    } else if (position == "short"){
        return(base::pmin(data, 0))
    } else if (position == "n"){
        return(base::pmin(data + 2, 1))
    } else {
        stop("position argument must be: all, long, short, or n")
    }
}
#' Compute Hits
#' Returns 1 TRUE if binary bet == sign(log growth).
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
compute_hits <- function (data, x) {
    compute_bets(x) == sign(data)
}
#' Compute Long Hits
#' Returns 1 TRUE if binary bet == sign(log growth).
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
compute_hits_long <- function (data, x) {
    compute_bets_long(x) == sign(data)
}
#' Compute Short Hits
#' Returns 1 TRUE if binary bet == sign(log growth).
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
compute_hits_short <- function (data, x) {
    compute_bets_short(x) == sign(data)
}
#' Compute Return
#' Returns return based on bet.
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
compute_return <- function (data, x) {
    compute_bets(x) * data
}
#' Compute Long Return
#' Returns long return based on bet.
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
compute_return_long <- function (data, x) {
    compute_bets_long(x) * data
}
#' Compute Short Return
#' Returns short return based on bet.
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
compute_return_short <- function (data, x) {
    compute_bets_short(x) * data
}
