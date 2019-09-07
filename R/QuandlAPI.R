#' Quandl API Connection
#'
#' Register Quandl API key. For data retreival, please see \code{Quandl} packages for details
#'
#' @return NULL
#'
#' @seealso \link{Quandl.datatable}
#' @seealso \link{Quandl}
#'
#' @examples
#' QuandlConnect()
#' mydata = Quandl.datatable("ZACKS/FC", ticker="AAPL")
#'
#' @export
#' @import
#' Quandl
QuandlConnect <- function(){
    library(Quandl)
    Quandl.api_key(.quandlEnv$API_KEY)
}
