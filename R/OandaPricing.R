#' Get Priceing Information for Oanda Instruments
#'
#' @description
#' Get pricing information for a specified list of Instruments within an Account
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param INSTRUMENTS Instrument to get candlestick data for e.g. "AUD_USD"
#'
#' @return A \code{data.frame} of current prices
#'
#' @examples
#' getOandaCurPricing(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, INSTRUMENTS = c('AUD_USD'))
#'
#' @export
getOandaCurPricing <- function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = c('AUD_USD')){

  INSTRUMENTS = paste(INSTRUMENTS, collapse = "%2C")

  URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
  URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/pricing?instruments=")
  URL = paste0(URL, INSTRUMENTS)

  HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "))

  json.data <- getURL(URL,cainfo=system.file("CurlSSL","cacert.pem",
                                             package="RCurl"),httpheader=HEADERS)

  tryCatch({
    parsed.data <- fromJSON(json.data, simplifyDataFrame = TRUE, flatten = TRUE)$prices
    parsed.data <- parsed.data[, c("instrument", "closeoutAsk", "closeoutBid", "status", "time")]
    colnames(parsed.data) <- c('instrument', 'ask', 'bid', 'status', 'time')
  }, error = function(e) e)

  return(parsed.data)
}


# getCurrentPricingStream
