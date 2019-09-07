#' IG API Utilities
#' @import jsonlite
#' @import httr
robustFunction <- function(FUN, n=3, ...) {
    error <- NULL
    for(i in 1:n) {
        x <- tryCatch(FUN(...), error=function(x){
            print(x)
            error <<- x
            return(NULL)
        })
        if (!is.null(x)) return(x)
        print(paste("Retrying...",i))
    }
    stop(error)
}

#' IG API GET request
#' @import jsonlite
#' @import httr
rGET <- function(...,retries=3) robustFunction(GET,retries,...)
#' IG API POST request
#' @import jsonlite
#' @import httr
rPOST <- function(...,retries=3) robustFunction(POST,retries,...)
#' IG API DELETE request
#' @import jsonlite
#' @import httr
rDELETE <- function(...,retries=3) robustFunction(DELETE,retries,...)
#' IG API GET Snapshot
#' @import jsonlite
#' @import httr
rGetSnapshot <- function(...,retries=3) robustFunction(getSnapshot,retries,...)


#' IG API Login
#'
#' @description
#' Log into IG API
#'
#' @param id IG Account ID
#' @param p API Names
#' @param k API key
#' @param url API URL
#' @param timeo number of tries
#'
#' @return A \code{http} object of api connection
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#'
#' @export
#' @import jsonlite
#' @import httr
IG_Auth <- function(id, p, k, url="https://demo-api.ig.com/gateway/deal/session", timeo=5) {
    # HEADERS = IG_Auth(" ","APIdemo1", " ")
    d <- list(identifier=id, password=p)
    h <- add_headers("X-IG-API-KEY"=k, "Content-Type"="application/json; charset=UTF-8", Accept="application/json; charset=UTF-8", Version=1)
    r <- rPOST(url, body = toJSON(d, auto_unbox=TRUE), h, timeout(timeo))

    h <- add_headers("X-IG-API-KEY"=k, "X-SECURITY-TOKEN"=r$headers["x-security-token"][[1]],
                     "CST" = r$headers["cst"][[1]], "Content-Type"="application/json; charset=UTF-8",
                     Accept="application/json; charset=UTF-8")
    h
}

#' IG API V2 Login
#'
#' @description
#' Log into IG API
#'
#' @param id IG Account ID
#' @param p API Names
#' @param k API key
#' @param url API URL
#' @param timeo number of tries
#'
#' @return A \code{http} object of api connection
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#'
#' @export
#' @import jsonlite
#' @import httr
IG_Auth2 <- function(id, p, k, url="https://demo-api.ig.com/gateway/deal/session", timeo=5) {
    d <- list(identifier=id, password=p)
    h <- add_headers("X-IG-API-KEY"=k, "Content-Type"="application/json; charset=UTF-8", Accept="application/json; charset=UTF-8", Version=2)
    r <- rPOST(url, body = toJSON(d, auto_unbox=TRUE), h, timeout(timeo))

    h <- add_headers("X-IG-API-KEY"=k, "X-SECURITY-TOKEN"=r$headers["x-security-token"][[1]], Version=2,
                     "CST" = r$headers["cst"][[1]], "Content-Type"="application/json; charset=UTF-8",
                     Accept="application/json; charset=UTF-8")
    h
}


#' IG API Fetch Account Information
#'
#' @description
#' Fetch IG Account Information
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param timeo number of tries
#'
#' @return A \code{data.frame} of account details
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_accounts(HEADERS)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_accounts = function(headers, url ="https://demo-api.ig.com/gateway/deal/accounts", timeo=5){
    # IG_fetch_accounts(HEADERS)
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    ret <- NULL
    for(i in 1:NROW(r$accounts)) {
        x <- r$accounts[[i]]
        d <- unlist(x)
        ret <- rbind(ret,d)
    }
    as.data.frame(ret, row.names = 1:nrow(ret))
}


#' IG API Fetch Account Activities
#'
#' @description
#' Fetch IG Account Activities
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param milliseconds lookback periods
#' @param timeo number of tries
#'
#' @return A \code{data.frame} of account activities
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_account_act(HEADERS)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_account_act = function(headers, url ="https://demo-api.ig.com/gateway/deal/history/activity", milliseconds = 3600000, timeo=5){
    # IG_fetch_account_act(HEADERS)
    url <- paste(url,milliseconds,sep="/")
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    ret <- NULL
    for(i in 1:NROW(r$activities)) {
        x <- r$activities[[i]]
        d <- unlist(x)
        ret <- rbind(ret,d)
    }
    as.data.frame(ret, row.names = 1:nrow(ret))
}


#' IG API Fetch Account Historical Transactions
#'
#' @description
#' Fetch IG Account Historical Transactions
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param trans_type Types of transactions to retreive
#' @param milliseconds lookback periods
#' @param timeo number of tries
#'
#' @return A \code{data.frame} of account historical txns
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_txns_hist(HEADERS)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_txns_hist = function(headers, url ="https://demo-api.ig.com/gateway/deal/history/transactions", trans_type = "ALL", milliseconds = 3600000, timeo=5){
    # IG_fetch_txns_hist(HEADERS)
    url <- paste(url,trans_type,milliseconds,sep="/")
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    ret <- NULL
    for(i in 1:NROW(r$transactions)) {
        x <- r$transactions[[i]]
        d <- unlist(x)
        ret <- rbind(ret,d)
    }
    as.data.frame(ret, row.names = 1:nrow(ret))
}



#' IG API Returns a deal confirmation for the given deal reference
#'
#' @description
#' Returns a deal confirmation for the given deal reference
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param deal_reference deal reference
#'
#' @return A \code{data.frame} deal confirmation
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_deal(HEADERS, deal_reference = '')
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_deal = function(headers, url ="https://demo-api.ig.com/gateway/deal/confirms", deal_reference, timeo=5){
    # IG_fetch_deal(HEADERS)
    url <- paste(url,deal_reference,sep="/")
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    # ret <- NULL
    # for(i in 1:NROW(r$activities)) {
    #     x <- r$activities[[i]]
    #     d <- unlist(x)
    #     ret <- rbind(ret,d)
    # }
    # as.data.frame(ret, row.names = 1:nrow(ret))
}



#' IG API Returns all open positions for the active account.
#'
#' @description
#' Returns all open positions for the active account.
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param timeo number of tries
#'
#' @return A \code{data.frame} open positions
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_open_pos(HEADERS)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_open_pos = function(headers, url ="https://demo-api.ig.com/gateway/deal/positions", timeo=5){
    # IG_fetch_open_pos(HEADERS)
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    ret <- NULL
    for(i in 1:NROW(r$positions)) {
        x <- r$positions[[i]]
        d <- unlist(x)
        ret <- rbind(ret,d)
    }
    as.data.frame(ret, row.names = 1:nrow(ret))
}


#' IG API Closes one or more OTC positions
#'
#' @description
#' Closes one or more OTC positions
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param deal_id 	Deal identifier
#' @param direction Deal direction ('BUY' or 'SELL')
#' @param epic Instrument epic identifier
#' @param expiry Instrument expiry
#' @param level Closing deal level
#' @param order_type 'LIMIT', 'MARKET', 'QUATE'
#' @param quote_id 	Lightstreamer price quote identifier
#' @param size Deal size
#' @param timeo number of tries
#'
#' @return A \code{data.frame} Close position response
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_close_open_pos(headers = HEADERS, direction = 'BUY', epic = 'CS.D.AUDCAD.CFD.IP', size = 1)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_close_open_pos = function(headers, url ="https://demo-api.ig.com/gateway/deal/positions/otc",
                             deal_id = '', direction, epic, expiry = "-", level = "",timeInForce = "",
                             order_type = 'MARKET', quote_id = "", size, timeo=5){
    params = list(
        dealId = deal_id,
        direction = direction,
        epic = epic,
        expiry = expiry,
        level = level,
        orderType = order_type,
        quoteId = quote_id,
        size = size,
        timeInForce = timeInForce
    )
    r <- rPOST(url, body = toJSON(params, null = "null", auto_unbox=TRUE), headers, timeout(timeo))
    # content(r)
    if(r$status_code == 200){
        deal_reference = content(r)['dealReference']
    }else{
        deal_reference = r$text
    }
    deal_reference
    # need to test
}

#' IG API Create one or more OTC positions
#'
#' @description
#' Create one or more OTC positions
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param dealReference A user-defined reference identifying the submission of the order
#' @param currency_code Currency. Restricted to available instrument currencies
#' @param direction Deal direction ('BUY' or 'SELL')
#' @param epic Instrument epic identifier
#' @param expiry Instrument expiry
#' @param force_open True if force open is required
#' @param guaranteed_stop True if a guaranteed stop is required
#' @param level Closing deal level
#' @param limit_distance Limit distance
#' @param limit_level Limit level
#' @param order_type 'LIMIT', 'MARKET', 'QUATE'
#' @param size Deal size
#' @param stop_distance Stop distance
#' @param stop_level Stop level
#' @param timeInForce 'EXECUTE_AND_ELIMINATE' or 'FILL_OR_KILL'
#' @param trailingStop Whether the stop has to be moved towards the current level in case of a favourable trade
#' @param trailingStopIncrement increment step in pips for the trailing stop
#' @param timeo number of tries
#'
#' @return A \code{data.frame} 	Deal reference of the transaction
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' order = IG_create_open_pos(headers = HEADERS, url ="https://demo-api.ig.com/gateway/deal/positions/otc",
#' dealReference = 'audcad001', currency_code = 'AUD', direction = 'BUY', epic = 'CS.D.AUDUSD.CFD.IP',
#' expiry = '-', force_open = 'true', guaranteed_stop = 'false', level = '', limit_distance = '', limit_level = '',
#' order_type = 'MARKET', size = 3,
#' stop_distance = 10, stop_level = '', trailingStop = 'false', trailingStopIncrement = '',
#' timeInForce = 'FILL_OR_KILL', timeo=5)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_create_open_pos = function(headers, url ="https://demo-api.ig.com/gateway/deal/positions/otc",
                              dealReference = '', currency_code = 'AUD', direction = 'BUY', epic, expiry = '-', force_open = 'true',
                              guaranteed_stop = 'false', level = '', limit_distance = '', limit_level = '', order_type = 'MARKET', size,
                              stop_distance = '', stop_level = '', trailingStop = 'false', trailingStopIncrement = '',
                              timeInForce = 'FILL_OR_KILL', timeo=5){
    params = list(
        dealReference = dealReference,
        currencyCode = currency_code,
        direction = direction,
        epic = epic,
        expiry = expiry,
        forceOpen = force_open,
        guaranteedStop = guaranteed_stop,
        level = level,
        limitDistance = limit_distance,
        limitLevel = limit_level,
        orderType = order_type,
        size = size,
        stopDistance = stop_distance,
        stopLevel = stop_level,
        # timeInForce = timeInForce
        trailingStop = trailingStop,
        trailingStopIncrement = trailingStopIncrement
    )
    r <- rPOST(url, body = toJSON(params, auto_unbox=TRUE), headers, timeout(timeo))

    if(r$status_code == 200){
        deal_reference = content(r)['dealReference']
    }else{
        deal_reference = r$text
    }
    deal_reference
    # need to test
}


#' IG API Updates an OTC position.
#'
#' @description
#' Updates an OTC position.
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param limit_level Limit level
#' @param stop_level Stop level
#' @param deal_id deal identifier
#' @param trailingStop Whether the stop has to be moved towards the current level in case of a favourable trade
#' @param trailingStopIncrement increment step in pips for the trailing stop
#' @param trailingStopDistance 	Trailing stop distance
#' @param timeo number of tries
#'
#' @return A \code{data.frame} 	Deal reference of the transaction
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_update_open_pos(HEADERS) # to be updated
#'
#' @export
#' @import jsonlite
#' @import httr
IG_update_open_pos = function(headers, url ="https://demo-api.ig.com/gateway/deal/positions/otc",
                              limit_level, stop_level, deal_id,
                              trailingStop, trailingStopIncrement, trailingStopDistance, timeo=5){
    params = list(
        limitLevel = limit_level,
        stopLevel = stop_level,
        trailingStop = trailingStop,
        trailingStopIncrement = trailingStopIncrement,
        trailingStopDistance = trailingStopDistance
    )
    url <- paste(url,deal_id,sep="/")
    r <- rPOST(url, body = toJSON(params, auto_unbox=TRUE), headers, timeout(timeo))

    if(r$status_code == 200){
        deal_reference = content(r)['dealReference']
    }else{
        deal_reference = r$text
    }
    deal_reference
    # need to test
}


#' IG API Returns all open working orders for the active account.
#'
#' @description
#' Returns all open working orders for the active account.
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param timeo number of tries
#'
#' @return A \code{data.frame} List working orders response
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_working_orders(HEADERS) # to be updated
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_working_orders = function(headers, url ="https://demo-api.ig.com/gateway/deal/workingorders", timeo=5){
    # IG_fetch_working_orders(HEADERS)
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    ret <- NULL
    for(i in 1:NROW(r$workingOrders)) {
        x <- r$workingOrders[[i]]
        d <- unlist(x)
        ret <- rbind(ret,d)
    }
    as.data.frame(ret, row.names = 1:nrow(ret))
}

#' IG API Creates an OTC working order.
#'
#' @description
#' Creates an OTC working order.
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param dealReference A user-defined reference identifying the submission of the order
#' @param currency_code Currency. Restricted to available instrument currencies
#' @param direction Deal direction ('BUY' or 'SELL')
#' @param epic Instrument epic identifier
#' @param expiry Instrument expiry
#' @param good_till_date This accepts two possible formats either yyyy/mm/dd hh:mm:ss in UTC Time or Unix Timestamp in milliseconds
#' @param guaranteed_stop True if a guaranteed stop is required
#' @param level Closing deal level
#' @param limit_distance Limit distance
#' @param limit_level Limit level
#' @param order_type 'LIMIT', 'MARKET', 'QUATE'
#' @param size Deal size
#' @param stop_distance Stop distance
#' @param stop_level Stop level
#' @param timeInForce 'EXECUTE_AND_ELIMINATE' or 'FILL_OR_KILL'
#' @param timeo number of tries
#'
#' @return A \code{data.frame} 	OTC working order.
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_create_working_order(HEADERS) # to be updated
#'
#' @export
#' @import jsonlite
#' @import httr
IG_create_working_order = function(headers, url ="https://demo-api.ig.com/gateway/deal/workingorders/otc",
                                   dealReference, currency_code, direction, epic, expiry, good_till_date,
                                   guaranteed_stop, level, limit_distance, limit_level, size, stop_distance, stop_level,
                                   time_in_force, order_type, timeo=5){
    params = list(
        dealReference = dealReference,
        currencyCode = currency_code,
        direction = direction,
        epic = epic,
        expiry = expiry,
        goodTillDate = good_till_date,
        guaranteedStop = guaranteed_stop,
        level = level,
        limitDistance = limit_distance,
        limitLevel = limit_level,
        size = size,
        stopDistance = stop_distance,
        stopLevel = stop_level,
        timeInForce = time_in_force,
        type = order_type
    )
    url <- paste(url,deal_id,sep="/")
    r <- rPOST(url, body = toJSON(params, auto_unbox=TRUE), headers, timeout(timeo))

    if(r$status_code == 200){
        deal_reference = content(r)['dealReference']
    }else{
        deal_reference = r$text
    }
    deal_reference
    # need to test
}

#' IG API Deletes an OTC working order.
#'
#' @description
#' Deletes an OTC working order.
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param deal_id deal ID
#' @param timeo number of tries
#'
#' @return A \code{data.frame} Deletes OTC working order.
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_del_working_order(HEADERS, deal_id = '') # to be updated
#'
#' @export
#' @import jsonlite
#' @import httr
IG_del_working_order = function(headers, url ="https://demo-api.ig.com/gateway/deal/workingorders/otc", deal_id, timeo=5){
    url <- paste(url,deal_id,sep="/")
    params = list()
    r <- rPOST(url, body = toJSON(params, auto_unbox=TRUE), headers, timeout(timeo))

    if(r$status_code == 200){
        deal_reference = json.loads(r$text)['dealReference']
    }else{
        deal_reference = r$text
    }
    deal_reference
    # need to test
}


#' IG API Updates an OTC working order.
#'
#' @description
#' Updates an OTC working order.
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param deal_id deal ID
#' @param good_till_date This accepts two possible formats either yyyy/mm/dd hh:mm:ss in UTC Time or Unix Timestamp in milliseconds
#' @param level Closing deal level
#' @param limit_distance Limit distance
#' @param limit_level Limit level
#' @param order_type 'LIMIT', 'MARKET', 'QUATE'
#' @param stop_distance Stop distance
#' @param stop_level Stop level
#' @param timeInForce 'EXECUTE_AND_ELIMINATE' or 'FILL_OR_KILL'
#' @param timeo number of tries
#'
#' @return A \code{data.frame} 	OTC working order.
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_update_working_order(HEADERS) # to be updated
#'
#' @export
#' @import jsonlite
#' @import httr
IG_update_working_order = function(headers, url ="https://demo-api.ig.com/gateway/deal/workingorders/otc",
                                   good_till_date, level, limit_distance, limit_level,
                                   stop_distance, stop_level, time_in_force, order_type, deal_id, timeo=5){
    params = list(
        goodTillDate = good_till_date,
        level = level,
        limitDistance = limit_distance,
        limitLevel = limit_level,
        stopDistance = stop_distance,
        stopLevel = stop_level,
        timeInForce = time_in_force,
        type = order_type
    )
    url <- paste(url,deal_id,sep="/")
    r <- rPOST(url, body = toJSON(params, auto_unbox=TRUE), headers, timeout(timeo))

    if(r$status_code == 200){
        deal_reference = content(r)['dealReference']
    }else{
        deal_reference = r$text
    }
    deal_reference
    # need to test
}



#' IG API Fetch Historical Prices
#'
#' @description
#' Fetch IG Account Historical Prices
#'
#' @param epic Instrument ID
#' @param n Number of points to retrieve
#' @param res Granularity of data
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param timeo number of tries
#'
#' @return A \code{data.frame} of historical prices
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' audusd = IG_fetch_hist_prices("CS.D.AUDUSD.CFD.IP", 50, "HOUR", headers = HEADERS)
#' usdcad = IG_fetch_hist_prices("CS.D.USDCAD.CFD.IP", 50, "HOUR", headers = HEADERS)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_hist_prices <- function(epic, n=50, res="DAY", headers, url ="https://demo-api.ig.com/gateway/deal/prices", timeo=5) {
    # histp = IG_fetch_hist_prices("CS.D.AUDUSD.CFD.IP", 50, "HOUR", headers = HEADERS)
    # histp = IG_fetch_hist_prices("X.D.EURGBP.CASH.IP", 50, "DAY", headers = HEADERS)
    # CS.D.AUDUSD.CFD.IP, CS.D.USDCAD.CFD.IP
    url <- paste(url,epic,res,n,sep="/")
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    ret <- NULL
    for(i in 1:NROW(r$prices)) {
        x <- r$prices[[i]]
        d <- data.frame(date=x$snapshotTime, oBid=x$openPrice$bid,oAsk=x$openPrice$ask, cBid=x$closePrice$bid,cAsk=x$closePrice$ask, hBid=x$highPrice$bid,hAsk=x$highPrice$ask,lBid=x$lowPrice$bid,lAsk=x$lowPrice$ask)
        ret <- rbind(ret,d)
    }
    ret$epic <- epic
    ret
}


#' IG API Get Top Navigation
#'
#' @description
#' Get Top Navigation
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param timeo number of tries
#'
#' @return A \code{data.frame} of Top Navigation
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_top_navigation(HEADERS)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_top_navigation = function(headers, url ="https://demo-api.ig.com/gateway/deal/marketnavigation", timeo=5){
    # IG_fetch_top_navigation(HEADERS)
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    ret <- NULL
    for(i in 1:NROW(r$nodes)) {
        x <- r$nodes[[i]]
        d <- unlist(x)
        ret <- rbind(ret,d)
    }
    as.data.frame(ret, row.names = 1:nrow(ret))
}


#' IG API Get Sub Navigation
#'
#' @description
#' Get Sub Navigation
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param node node id get from navigation
#' @param timeo number of tries
#'
#' @return A \code{data.frame} of node Navigation
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_sub_nodes(HEADERS, node = 195235)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_sub_nodes = function(headers, url ="https://demo-api.ig.com/gateway/deal/marketnavigation", node = 195235, timeo=5){
    # IG_fetch_sub_nodes(HEADERS, node = 195235)
    url <- paste(url,node,sep="/")
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    ret <- NULL
    for(i in 1:NROW(r$nodes)) {
        x <- r$nodes[[i]]
        d <- unlist(x)
        ret <- rbind(ret,d)
    }
    as.data.frame(ret, row.names = 1:nrow(ret))
}

#' IG API Fetch Markets Information
#'
#' @description
#' Fetch Markets Information
#'
#' @param epic Instrument ID
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param timeo number of tries
#'
#' @return A \code{data.frame} of markets information
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_markets(epic = "CS.D.AUDUSD.CFD.IP", HEADERS)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_markets = function(epic = "IX.D.FTSE.IFM.IP", headers, url ="https://demo-api.ig.com/gateway/deal/markets", timeo=5){
    # IG_fetch_markets(epic = "IX.D.FTSE.IFM.IP", HEADERS)
    url <- paste(url,epic,sep="/")
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)

    instrument = r$instrument
    snapshot = r$snapshot
    dealingRules = r$dealingRules
    # need to deal with json
}


#' IG API Search Markets
#'
#' @description
#' Search Markets Information
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param search_term search term
#' @param timeo number of tries
#'
#' @return A \code{data.frame} of markets information
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_search_markets(HEADERS, search_term = "USD")
#'
#' @export
#' @import jsonlite
#' @import httr
IG_search_markets = function(headers, url ="https://demo-api.ig.com/gateway/deal/marketnavigation", search_term = "AUD", timeo=5){
    # IG_search_markets(HEADERS, search_term = "USD")
    url <- paste(url,search_term,sep="/")
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    r
}


#' IG API Get All Watchlists
#'
#' @description
#' Get All Watchlists
#'
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param timeo number of tries
#'
#' @return A \code{data.frame} of watchlists information
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_all_watchlists(HEADERS)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_all_watchlists = function(headers, url ="https://demo-api.ig.com/gateway/deal/watchlists", timeo=5){
    # IG_fetch_all_watchlists(HEADERS)
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    ret <- NULL
    for(i in 1:NROW(r$watchlists)) {
        x <- r$watchlists[[i]]
        d <- unlist(x)
        ret <- rbind(ret,d)
    }
    as.data.frame(ret, row.names = 1:nrow(ret))
}


#' IG API Get Sentiment of Market
#'
#' @description
#' Get Sentiment of Market
#'
#' @param market_id market id
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param timeo number of tries
#'
#' @return A \code{data.frame} of market sentiment information
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_sentiment(market_id = "VOD-UK", HEADERS)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_sentiment = function(market_id = "VOD-UK", headers, url ="https://demo-api.ig.com/gateway/deal/clientsentiment", timeo=5){
    # IG_fetch_sentiment(market_id = "VOD-UK", HEADERS)
    url <- paste(url,market_id,sep="/")
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    data.frame(marketId = market_id, longPos = r$longPositionPercentage, shortPos = r$shortPositionPercentage)
}


#' IG API Get Sentiment of Related Market
#'
#' @description
#' Get Sentiment of Related Market
#'
#' @param market_id market id
#' @param headers Object returned from \code{IG_Auth}
#' @param url API URL
#' @param timeo number of tries
#'
#' @return A \code{data.frame} of related market sentiment information
#'
#' @examples
#' HEADERS = IG_Auth(" ","APIdemo1", " ")
#' IG_fetch_rel_sentiment(market_id = "VOD-UK", HEADERS)
#'
#' @export
#' @import jsonlite
#' @import httr
IG_fetch_rel_sentiment = function(market_id = "VOD-UK", headers, url ="https://demo-api.ig.com/gateway/deal/clientsentiment/related", timeo=5){
    # IG_fetch_rel_sentiment(market_id = "VOD-UK", HEADERS)
    url <- paste(url,market_id,sep="/")
    r <- rGET(url=url,headers, timeout(timeo))
    r <- content(r)
    ret <- NULL
    for(i in 1:NROW(r$clientSentiments)) {
        x <- r$clientSentiments[[i]]
        d <- unlist(x)
        ret <- rbind(ret,d)
    }
    as.data.frame(ret, row.names = 1:nrow(ret))
}





















