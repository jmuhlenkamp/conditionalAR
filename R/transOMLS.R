#' Transform Raw Signals into OLOS, MLMS, etc.
#'
#' Transform raw signals into OLOS, MLMS, etc.
#'
#' @param signalcomb Character of either: olos, mlms, olml, osms.
#' @param x xts object containing the raw signals.
#'
transOMLS <- function(signalcomb, x){
    if (signalcomb == "mlms") {      return(        x  ) }
    else if (signalcomb == "olos") { return( -1*    x  ) }
    else if (signalcomb == "olml") { return(    abs(x) ) }
    else if (signalcomb == "osms") { return( -1*abs(x) ) }
    else { stop("signalcomb must be 1 of: olos, mlms, olml, osms")}
}
