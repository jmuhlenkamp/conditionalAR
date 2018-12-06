#' Compute Binary Bets
#' Takes predictions and creates lagged -1, 0, 1 values based on sign.
#' @param x An `xts` object containing predictions.
compute_bets <- function (x) {
    xts::lag.xts(sign(x))
}
#' Compute Binary Long Bets
#' Takes predictions and creates lagged 0, 1 values based on sign.
#' @param x An `xts` object containing predictions.
compute_bets_long <- function (x) {
    base::pmax(compute_bets(x), 0)
}
#' Compute Binary Short Bets
#' Takes predictions and creates lagged -1, 0 values based on sign.
#' @param x An `xts` object containing predictions.
compute_bets_short <- function (x) {
    base::pmin(compute_bets(x), 0)
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
