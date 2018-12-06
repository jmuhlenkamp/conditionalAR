#' pmax for xts Objects
#'
#' Wrapper around `pmax` in order to apply to xts objects.
#'
#' @param x An `xts` object.
#' @param ... Arguments to be passed to `pmax`.
#'
pmax.xts <- function (x, ...)
{
    m <- apply(x, 2, function(x){pmax(x, ...)})
    xts::as.xts(m, order.by = index(x))
}
#'
#' pmin for xts Objects
#'
#' Wrapper around `pmin` in order to apply to xts objects.
#'
#' @param x An `xts` object.
#' @param ... Arguments to be passed to `pmin`.
#'
pmin.xts <- function (x, ...)
{
    m <- apply(x, 2, function(x){pmin(x, ...)})
    xts::as.xts(m, order.by = index(x))
}
