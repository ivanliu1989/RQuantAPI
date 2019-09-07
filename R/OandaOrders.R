#' Create an Order for an Oanda Account
#'
#' Create an Order for an Oanda Account
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param INSTRUMENTS Instrument to get candlestick data for e.g. "AUD_USD"
#' @param UNITS Number of units of the instrument to be executed for the order
#' @param POSITIONFILL "MARKET_IF_TOUCHED", "MARKET", "LIMIT" or "DEFAULT"
#' @param STOPLOSS prices of stop loss
#' @param TAKEPROFIT prices of take profits
#' @param ORDERTYPE order types including 'MARKET', 'LIMIT', 'STOP'
#' @param PRICE for limit order which only will be filled when the prices are better
#' @param TRAILINGSTOP Trailing stop
#' @param TIMEINFORCE GTC: The Order is “Good unTil Cancelled” | GTD: The Order is “Good unTil Date” and will be cancelled at the provided time | FOK: The Order must be immediately “Filled Or Killed” | IOC: The Order must be “Immediatedly paritally filled Or Cancelled” | GFD: The Order is “Good For Day” and will be cancelled at 5pm New York time
#' @param GTDTIME The date/time when the Limit Order will be cancelled if its timeInForce (e.g. 2014-07-02T04:00:00.000000Z)
#'
#' @return A \code{HTTP Response}
#'
#' @examples
#' createOandaOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = -10000)
#' # HTTP 201 – The Order was created as specified
#' # Response [https://api-fxpractice.oanda.com/v3/accounts/101-011-4686012-003/orders]
#' # Date: 2016-11-28 11:38
#' # Status: 201
#' # Content-Type: application/json
#' # Size: 775 B
#'
#' @export
createOandaOrder = function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS, POSITIONFILL = 'DEFAULT',
                            STOPLOSS = NULL, TAKEPROFIT = NULL, TRAILINGSTOP = NULL, ORDERTYPE = 'MARKET', TIMEINFORCE = 'FOK',
                            GTDTIME = NULL, PRICE = NULL, ...){
    library(httr)
    # LimitOrder: Only be filled by a price that is equal to or better than the threshold
    # StopOrder: Only be filled by a price that is equal to or worse than the threshold
    # MarketOrder: Be filled inmmediately upon creation

    # priceBound: The worst price that the client is willing to have the Market Order
    # longPositionCloseout|shortPositionCloseout: MarketOrderPositionCloseout: instrument | units: ALL or numbers

    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/orders")

    # Headers -----------------------------------------------------------------
    HEADERS <- add_headers(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "), "Content-Type" = "application/json")

    # Parameters
    PARAMS <- list(order = list(units = as.character(UNITS),
                                instrument = INSTRUMENTS,
                                timeInForce = TIMEINFORCE,
                                type = ORDERTYPE,
                                positionFill = POSITIONFILL))
    if(!is.null(STOPLOSS)){
        PARAMS[['order']][['stopLossOnFill']] = list(price = as.character(round(STOPLOSS,5)))
    }
    if(!is.null(GTDTIME)){
        PARAMS[['order']][['gtdTime']] = as.character(GTDTIME)
    }
    if(!is.null(TAKEPROFIT)){
        PARAMS[['order']][['takeProfitOnFill']] =list(price = as.character(round(TAKEPROFIT,5)))
    }
    if(!is.null(PRICE) & ORDERTYPE == 'LIMIT'){
        PARAMS[['order']][['price']] = as.character(round(PRICE,5))
    }
    if(!is.null(TRAILINGSTOP)){
        PARAMS[['order']][['trailingStopLossOnFill']] = list(distance = as.character(round(TRAILINGSTOP,5)))
    }

    tryCatch({
        resp <- POST(URL, config = HEADERS, body = PARAMS, encode = "json")
    }, error = function(e) e)

    if(resp$status_code == 201) {
        cat("HTTP 201 – The Order was created as specified\n")
    }else{
        cat(paste0(resp$status_code, " | Failed to create order!\n"))
    }

    return(resp)
}



#' Get a list of Orders of an Account
#'
#' Get a list of Orders of an Account
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param INSTRUMENTS Instrument to get candlestick data for e.g. "AUD_USD"
#' @param PENDING return only pending orders or not
#'
#' @return A \code{list} of order histories
#'
#' @examples
#' getOandaOrderList(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, INSTRUMENTS = 'USD_CAD')
#' # $lastTransactionID
#' # [1] "2584"
#' # $orders
#' # list()
#'
#' @export
getOandaOrderList <- function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', PENDING = FALSE){
    library(RCurl)
    library(jsonlite)
    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    if(PENDING){
        URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/pendingOrders")
    }else{
        URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/orders?instrument=", INSTRUMENTS)
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


#' Close out a Oanda Position
#'
#' Close out a Oanda Position
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param INSTRUMENTS Instrument to get candlestick data for e.g. "AUD_USD"
#' @param LONGUNITS Number of units of the instrument to be executed, 'ALL' or numbers
#' @param SHORTUNITS Number of units of the instrument to be executed
#'
#'
#' @return A \code{HTTP Response}
#'
#' @examples
#' createOandaPositionClose(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', LONGUNITS = 'ALL')
#' # HTTP 201 – The Order was created as specified
#' # Response [https://api-fxpractice.oanda.com/v3/accounts/101-011-4686012-003/orders]
#' # Date: 2016-11-28 11:38
#' # Status: 201
#' # Content-Type: application/json
#' # Size: 775 B
#'
#' @export
createOandaPositionClose <- function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', LONGUNITS = NULL, SHORTUNITS = NULL){
    library(httr)
    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/positions/", INSTRUMENTS, "/close")

    # Headers -----------------------------------------------------------------
    HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "), "Content-Type" = "application/json", "longUnits" = as.character(LONGUNITS))

    # Parameters
    if(!is.null(LONGUNITS)){
        PARAMS <- list(longUnits = as.character(LONGUNITS))
    }else if(!is.null(SHORTUNITS)){
        PARAMS <- list(shortUnits = as.character(SHORTUNITS))
    }

    tryCatch({
        resp <- PUT(URL, config = HEADERS, body = PARAMS, encode = "json")
    }, error = function(e) e)

    if(resp$status_code == 200) {
        cat("HTTP 200 – The Position closeout request has been successfully processed.\n")
    }else{
        cat(paste0(resp$status_code, " | Failed to closeout positions!\n"))
    }

    return(resp)
}


# createTakeProfit
# createEntryOrder
# createLimitOrder

#' Create Stoploss or Takeprofit Order for an Oanda Account
#'
#' Create Stoploss or Takeprofit Order for an Oanda Account
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param TRADEID trade id
#' @param PRICE prices to trigger the action
#' @param ORDERTYPE "TAKE_PROFIT", "STOP_LOSS"
#'
#'
#' @return A \code{HTTP Response}
#'
#' @examples
#' createOandaTPSLOrder(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, TRADEID = '1111', PRICE = 1.3475, ORDERTYPE = 'TAKE_PROFIT')
#' # HTTP 201 – The Order was created as specified
#' # Response [https://api-fxpractice.oanda.com/v3/accounts/101-011-4686012-003/orders]
#' # Date: 2016-11-28 11:38
#' # Status: 201
#' # Content-Type: application/json
#' # Size: 775 B
#'
#' @export
createOandaTPSLOrder = function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, TRADEID, PRICE = NULL, ORDERTYPE = 'TAKE_PROFIT', ...){
    library(httr)
    # LimitOrder: Only be filled by a price that is equal to or better than the threshold
    # StopOrder: Only be filled by a price that is equal to or worse than the threshold
    # MarketOrder: Be filled inmmediately upon creation


    # priceBound: The worst price that the client is willing to have the Market Order
    # longPositionCloseout|shortPositionCloseout: MarketOrderPositionCloseout: instrument | units: ALL or numbers

    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/orders")

    # Headers -----------------------------------------------------------------
    HEADERS <- add_headers(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "), "Content-Type" = "application/json")

    # Parameters
    PARAMS <- list(order = list(price = as.character(round(PRICE,5)),
                                timeInForce = 'GTC',
                                type = ORDERTYPE,
                                tradeID = TRADEID))

    tryCatch({
        resp <- POST(URL, config = HEADERS, body = PARAMS, encode = "json")
    }, error = function(e) e)

    if(resp$status_code == 201) {
        cat("HTTP 201 – The Order was created as specified\n")
    }else{
        cat(paste0(resp$status_code, " | Failed to create order!\n"))
    }

    return(resp)
}



#' Cancel a pending Oanda offer
#'
#' Cancel a pending Oanda offer
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param ORDER order id
#'
#' @return A \code{HTTP Response}
#'
#' @export
cancelOandaOrder <- function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, ORDER){
    library(httr)
    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/orders/", ORDER, "/cancel")

    # Headers -----------------------------------------------------------------
    # HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "), "Content-Type" = "application/json")

    # Parameters
    tryCatch({
        resp <- PUT(URL, config = add_headers(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "), 'Content-Type' = "application/json"), encode = "json")
    }, error = function(e) e)

    if(resp$status_code == 200) {
        cat("HTTP 200 – The Position closeout request has been successfully processed.\n")
    }else{
        cat(paste0(resp$status_code, " | Failed to closeout positions!\n"))
    }

    return(resp)
}
