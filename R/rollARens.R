#' Apply an Ensemble of AR Coefficients
#'
#' Apply an ensemble of AR coefficients to one or multiple columns of an xts object
#' in a rolling fashion.
#'
#' @param x An xts object
#' @param coef A list of numeric vectors of AR coefficients.
#'             First element in each vector corresponds to AR1.
#'
rollARens <- function(x, coef){
    stopifnot(class(coef) == "list")
    signal <- sign(rollAR(x, coef[[1]]))
    if(length(coef) == 1) return(signal)
    expr <- "signal"
    for (i in 2:length(coef)) {
        expr <- paste(expr,
                      "* (signal == sign(rollAR(x, coef[[", i, "]])))")
    }
    eval(parse(text=expr))
}
