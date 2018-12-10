#' Compute Days
#' Takes predictions and returns N.
#' @param x An `xts` object containing predictions.
summary_days <- function (x) {
    colSums(abs(pmax(x, 1)), na.rm = TRUE)
}
#' Compute Bets
#' Takes predictions and returns bet rate.
#' @param x An `xts` object containing predictions.
summary_bets <- function (x) {
    colSums(abs(compute_bets(x)), na.rm = TRUE)
}
#' Compute Long Bets
#' Takes signals and returns bet count for long predictions.
#' @param x An `xts` object containing predictions.
summary_bets_long <- function (x) {
    colSums(abs(compute_bets_long(x)), na.rm = TRUE)
}
#' Compute Short Bets
#' Takes signals and returns bet count for short predictions.
#' @param x An `xts` object containing predictions.
summary_bets_short <- function (x) {
    colSums(abs(compute_bets_short(x)), na.rm = TRUE)
}
#' Compute Bet Rate
#' Takes predictions and returns bet rate.
#' @param x An `xts` object containing predictions.
summary_betrate <- function (x) {
    colMeans(abs(compute_bets(x)), na.rm = TRUE)
}
#' Compute Long Bet Rate
#' Takes signals and returns bet rate for long predictions.
#' @param x An `xts` object containing predictions.
summary_betrate_long <- function (x) {
    colMeans(abs(compute_bets_long(x)), na.rm = TRUE)
}
#' Compute Short Bet Rate
#' Takes signals and returns bet rate for short predictions.
#' @param x An `xts` object containing predictions.
summary_betrate_short <- function (x) {
    colMeans(abs(compute_bets_short(x)), na.rm = TRUE)
}
#' Compute Hit Rate
#' Takes predictions and growths and returns hit rate.
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
summary_hitrate <- function (data, x) {
    colSums(compute_hits(data, x), na.rm = TRUE) /
    colSums(abs(compute_bets(x)), na.rm = TRUE)
}
#' Compute Long Hit Rate
#' Takes predictions and growths and returns hit rate.
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
summary_hitrate_long <- function (data, x) {
    colSums(compute_hits_long(data, x), na.rm = TRUE) /
        colSums(abs(compute_bets_long(x)), na.rm = TRUE)
}
#' Compute Short Hit Rate
#' Takes predictions and growths and returns hit rate.
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
summary_hitrate_short <- function (data, x) {
    colSums(compute_hits_short(data, x), na.rm = TRUE) /
        colSums(abs(compute_bets_short(x)), na.rm = TRUE)
}
#' Compute Up Rate
#' Takes predictions and growths and returns hit rate.
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
summary_uprate <- function (data, x) {
    a <- pmax(x, 1)
    colSums(compute_hits(data, a), na.rm = TRUE) /
        colSums(abs(compute_bets(a)), na.rm = TRUE)
}
#' Compute Down Rate
#' Takes predictions and growths and returns hit rate.
#' @param data An `xts` object containing log growths.
#' @param x An `xts` object containing predictions.
summary_downrate <- function (data, x) {
    a <- pmin(x, -1)
    colSums(compute_hits(data, a), na.rm = TRUE) /
        colSums(abs(compute_bets(a)), na.rm = TRUE)
}
