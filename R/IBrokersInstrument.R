#' Download FX Historical Data
#'
#' Makes a request to the Interactive Brokers Trader Workstation (TWS), and returns an xts object containing the results of the request if successful.
#'
#' @param tws connection to current TWS account, if NULL, a new connection will be created within the function
#' @param duration time span the request will cover
#' @param barsize bar size to retrieve
#' @param Cur1 symbol of the first currency
#' @param Cur2 symbol of the second currency
#'
#' @details
#' Legal barSize settings are technically '1 secs','5 secs','15 secs','30 mins','1 min','2 mins', '3 mins','5 mins','15 mins', '30 mins','1 hour','1 day', '1 week','1 month','3 months', and '1 year'. They must be specified exactly and there is no guarantee from the API that all will work for all securities or durations.
#' The duration string must be of the form 'n S' where the last character may be any one of 'S' (seconds), 'D' (days), 'W' (weeks), 'M' (months), and 'Y' (year). At present the limit for years is 1.
#'
#' @return A \code{list} of Bid, Ask and Bid_Ask prices
#'
#' @examples
#' library(IBrokers)
#' tws <- twsConnect(port = 7497)
#' USDCAD <- getIBForexHist(tws, "10 Y", "1 day", "USD", "CAD")
#' USDAUD <- getIBForexHist(tws, "10 Y", "1 day", "AUD", "USD")
#'
#' @import IBrokers
#' @export
getIBForexHist <- function(tws = NULL, duration = "5 Y", barsize = "1 day", Cur1 = "USD", Cur2 = "CAD"){
    library(IBrokers)
    if(is.null(tws)){
        tws <- twsConnect(port = .ibEnv$PaperPort)
    }

    ccy <- reqContractDetails(tws, twsCurrency(Cur1, Cur2))[[1]]$contract

    BIDASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = barsize,
                                duration = duration, useRTH = "1", whatToShow='BID_ASK')

    MID <- reqHistoricalData(conn = tws, Contract = ccy, barSize = barsize,
                                duration = duration, useRTH = "1", whatToShow='MIDPOINT')

    BID <- reqHistoricalData(conn = tws, Contract = ccy, barSize = barsize,
                             duration = duration, useRTH = "1", whatToShow='BID')

    ASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = barsize,
                             duration = duration, useRTH = "1", whatToShow='ASK')

    VOLATILITY <- reqHistoricalData(conn = tws, Contract = ccy, barSize = barsize,
                             duration = duration, useRTH = "1", whatToShow='HISTORICAL_VOLATILITY')

    CleanData <- merge(BIDASK[,4], BID[,4], ASK[, 4])
    colnames(CleanData) <- c("Close.price", "Bid.price", "Ask.price")

    res <- list(MID = MID,
                BIDASK = BIDASK,
                BID = BID,
                ASK = ASK,
                VOLATILITY = VOLATILITY,
                CleanData = CleanData)

    if(is.null(tws)){
        twsDisconnect(tws)
    }

    return(res)
}



#' A Wrapper for IB Fundamental Data
#'
#' Makes a request to the Interactive Brokers Trader Workstation (TWS), and returns the fundamental data.
#'
#' @param twsconn connection to current TWS account, if NULL, a new connection will be created within the function
#' @param reqId tws request ID
#' @param contract a twsContract
#' @param reportType can be: ReportsFinSummary, ReportsOwnership, ReportSnapshot, ReportsFinStatements, RESC (Analyst Estimates), CalendarReport
#'
#' @examples
#' library(IBrokers)
#' tws <- twsConnect(port = 7497)
#' reqIBFundamentalData(tws, "1", twsSTK('AAPL'), 'ReportsFinSummary')
#'
#' @import IBrokers
#' @export
reqIBFundamentalData <- function(twsconn, reqId, contract, reportType) {
    if( !is.twsConnection(twsconn))
        stop('invalid twsConnection')
    if( !is.twsContract(contract))
        stop('invalid twsContract')

    VERSION <- "1"

    msg <- c( .twsOutgoingMSG$REQ_FUNDAMENTAL_DATA,
              VERSION,
              reqId,

              # contract fields
              contract$symbol,
              contract$sectype,
              contract$exch,
              contract$primary,
              contract$currency,
              contract$local,

              reportType)

    writeBin( as.character(msg), twsconn[[1]])

    con <- twsconn[[1]]
    eW <- eWrapper()
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        processMsg(curMsg, con, eW)
    }
}

#' A Wrapper for IB Fundamental Data Cancellation
#'
#' @param twsconn connection to current TWS account, if NULL, a new connection will be created within the function
#' @param reqId tws request ID
#'
#' @import IBrokers
#' @export
cancelIBFundamentalData <- function(twsconn, reqId) {
    if( !is.twsConnection(twsconn))
        stop('invalid twsConnection')

    VERSION <- "1"

    msg <- c( .twsOutgoingMSG$CANCEL_FUNDAMENTAL_DATA,
              VERSION,
              reqId)

    writeBin( as.character(msg), twsconn[[1]])

    con <- twsconn[[1]]
    eW <- eWrapper()
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        processMsg(curMsg, con, eW)
    }
}







#' A Wrapper for IB News Bulletins Data
#'
#' Makes a request to the Interactive Brokers Trader Workstation (TWS), and returns the Bulletins data.
#'
#' @param twsconn connection to current TWS account, if NULL, a new connection will be created within the function
#' @param allMsgs TRUE/FALSE
#'
#' @examples
#' library(IBrokers)
#' tws <- twsConnect(port = 7497)
#' reqNewsBulletins(tws, TRUE)
#'
#' @import IBrokers
#' @export
reqIBNewsBulletins <- function(twsconn, allMsgs=TRUE) {
    if( !is.twsConnection(twsconn))
        stop('requires twsConnection object')

    allMsgs <- as.character(as.integer(allMsgs))
    VERSION <- "1"

    writeBin( c(.twsOutgoingMSG$REQ_NEWS_BULLETINS, VERSION, allMsgs), twsconn[[1]])

    con <- twsconn[[1]]
    eW <- eWrapper()
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        cat(curMsg)#processMsg(curMsg, con, eW)
    }
}

#' Cancel News Bulletins of Interactive Brokers
#'
#' @param twsconn connection to current TWS account, if NULL, a new connection will be created within the function
#'
#' @import IBrokers
#' @export
cancelIBNewsBulletins <- function(twsconn) {
    if( !is.twsConnection(twsconn))
        stop('requires twsConnection object')

    VERSION <- "1"

    writeBin( c(.twsOutgoingMSG$CANCEL_NEWS_BULLETINS, VERSION), twsconn[[1]])

    con <- twsconn[[1]]
    eW <- eWrapper()
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        processMsg(curMsg, con, eW)
    }
}


#' A Wrapper for IB Executions history
#'
#'
#' @param twsconn connection to current TWS account, if NULL, a new connection will be created within the function
#' @param reqId request ID
#' @param ExecutionFilter execution criteria
#'
#' @examples
#' library(IBrokers)
#' tws <- twsConnect(port = 7497)
#' reqIBExecutions(tws, '1', ExecutionFilter)
#'
#' @import IBrokers
#' @export
reqIBExecutions = function (twsconn, reqId = "0", ExecutionFilter)
{
    if (!is.twsConnection(twsconn))
        stop("invalid 'twsConnection' object")
    con <- twsconn[[1]]
    VERSION <- "3"
    outgoing <- c(.twsOutgoingMSG$REQ_EXECUTIONS, VERSION, as.character(reqId),
                  ExecutionFilter$clientId, ExecutionFilter$acctCode,
                  ExecutionFilter$time, ExecutionFilter$symbol, ExecutionFilter$secType,
                  ExecutionFilter$exchange, ExecutionFilter$side)
    writeBin(outgoing, con)

    con <- twsconn[[1]]
    eW <- eWrapper()
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        processMsg(curMsg, con, eW)
    }
}
