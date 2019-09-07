#' Get a list of Trades for an Oanda Account
#'
#' @description
#' Get a list of Trades for an Oanda Account
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param INSTRUMENTS Instrument to get candlestick data for e.g. "AUD_USD"
#' @param OPEN Show only open trades or not
#'
#' @return A \code{data.frame} of trades
#'
#' @examples
#' getOandaTrades(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, INSTRUMENTS = c('AUD_USD'))
#'
#' @export
getOandaTrades <- function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', OPEN = FALSE){

    library(RCurl)
    library(jsonlite)

  # Generate URL ------------------------------------------------------------
  URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
  if(OPEN){
      URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/openTrades")
  }else{
      URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/trades?instrument=", INSTRUMENTS)
  }

  # Headers -----------------------------------------------------------------
  HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "), "Content-Type" = "application/json")

  tryCatch({
    json.data <- getURL(URL,cainfo=system.file("CurlSSL","cacert.pem",
                                               package="RCurl"),httpheader=HEADERS)
    parsed.data <- fromJSON(json.data, simplifyDataFrame = TRUE, flatten = TRUE)
  }, error = function(e) e)

  return(parsed.data)
}


#' Close a trade in Oanda
#'
#' @description
#' Close a trade in Oanda
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param TRADE_ID Identifier of trade to be closed
#'
#' @examples
#' cloaseOandaTrade(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, "XXXXX")
#'
#' @export
cloaseOandaTrade = function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, TRADE_ID, ...){
    library(httr)

    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/trades/", TRADE_ID, "/close")

    # Headers -----------------------------------------------------------------
    HEADERS <- add_headers(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "), "Content-Type" = "application/json")

    # Parameters
    tryCatch({
        resp <- PUT(URL, config = HEADERS, encode = "json")
    }, error = function(e) e)

    if(resp$status_code == 200) {
        cat("HTTP 200 – The Trade was closed as specified\n")
    }else{
        cat(paste0(resp$status_code, " | Failed to close trade!\n"))
    }

    return(resp)
}
