#' Economic Calendar information relevant to an instrument
#'
#' Returns up to 1 year worth of economic calendar information relevant to an instrument.
#' For example, if the instrument is EUR_USD, then all economic information relevant to the Euro and the US Dollar will be included.
#' Some of the entries are strictly news about an important meeting, while other entries may contain economic indicator data.
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param INSTRUMENTS Instrument to get candlestick data for e.g. "AUD_USD"
#' @param PERIOD Required Period of time to retrieve calendar data for.
#'
#' @details
#' Period includes:\cr
#'  - 1 hour\cr
#'  - 12 hours\cr
#'  - 1 day\cr
#'  - 1 week\cr
#'  - 1 month\cr
#'  - 3 months\cr
#'  - 6 months\cr
#'  - 1 year\cr
#'
#' @return A \code{data.table}
#'
#' @examples
#' Calenda = EconomicCalendarOanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', PERIOD = '1 year')
#' Calenda
#'
#' @export
EconomicCalendarOanda <- function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'EUR_USD', PERIOD = '1 year'){
    # 3600 - 1 hour
    # 43200 - 12 hours
    # 86400 - 1 day
    # 604800 - 1 week
    # 2592000 - 1 month
    # 7776000 - 3 months
    # 15552000 - 6 months
    # 31536000 - 1 year

    library(data.table)
    library(RCurl)
    library(jsonlite)
    # curl "https://api-fxtrade.oanda.com/labs/v1/calendar?instrument=EUR_USD&period=2592000"
    # -H "Authorization: Bearer <access-token>"
    lookupPeriod <- list("1 hour" = 3600,
                         "12 hours" = 43200,
                         "1 day" = 86400,
                         "1 week" = 604800,
                         "1 month" = 2592000,
                         "3 months" = 7776000,
                         "6 months" = 15552000,
                         "1 year" = 31536000
    )

    PERIOD = lookupPeriod[[tolower(PERIOD)]]

    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL = paste0(URL, "/labs/v1/calendar?instrument=", INSTRUMENTS, '&period=', PERIOD)

    # Headers -----------------------------------------------------------------
    HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "),
                 'Content-Type' = 'application/json')

    parsed.data <- NULL
    tryCatch({
        json.data <- getURL(URL,cainfo=system.file("CurlSSL","cacert.pem", package="RCurl"),httpheader=HEADERS)
        parsed.data <- fromJSON(json.data, simplifyDataFrame = TRUE)
        # parsed.data <- subset(parsed.data, select = -c(currency,region,impact))
        # parsed.data <- parsed.data[complete.cases(parsed.data[,]),]
        parsed.data$timestamp <- as.POSIXct(parsed.data$timestamp,origin = "1970-01-01")
        parsed.data <- parsed.data[,c("timestamp", "title", "region", "unit", "currency", "previous","market", "actual", "forecast", "impact")]
        setDT(parsed.data)
    }, error = function(e) e)

    return(parsed.data)
}



#' Historical Position ratios for a supported instrument
#'
#' Returns up to 1 year worth of historical position ratios for a supported instrument.
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param INSTRUMENTS Instrument to get candlestick data for e.g. "AUD_USD"
#' @param PERIOD Required Period of time to retrieve calendar data for.
#'
#' @details
#' Period includes:\cr
#'  - 1 day\cr
#'  - 2 days\cr
#'  - 1 week\cr
#'  - 1 month\cr
#'  - 3 months\cr
#'  - 6 months\cr
#'  - 1 year\cr
#'
#' @return A \code{data.table}
#'
#' @examples
#' HistPos <- HistoricalPositionOanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', PERIOD = '1 year')
#' HistPos
#'
#' @export
HistoricalPositionOanda  <- function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'EUR_USD', PERIOD = '1 year')
{
    library(data.table)
    library(RCurl)
    library(jsonlite)
    # 86400 - 1 day - 20 minute snapshots
    # 172800 - 2 day - 20 minute snapshots
    # 604800 - 1 week - 1 hour snapshots
    # 2592000 - 1 month - 3 hour snapshots
    # 7776000 - 3 months - 3 hour snapshots
    # 15552000 - 6 months - 3 hour snapshots
    # 31536000 - 1 year - daily snapshots

    lookupPeriod <- list("1 day" = 86400,
                         "2 days" = 172800,
                         "1 week" = 604800,
                         "1 month" = 2592000,
                         "3 months" = 7776000,
                         "6 months" = 15552000,
                         "1 year" = 31536000
    )

    PERIOD = lookupPeriod[[tolower(PERIOD)]]

    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL = paste0(URL, "/labs/v1/historical_position_ratios?instrument=", INSTRUMENTS, '&period=', PERIOD)

    # Headers -----------------------------------------------------------------
    HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "),'Content-Type' = 'application/json')

    parsed.data <- NULL
    tryCatch({
        json.data <- getURL(URL,cainfo=system.file("CurlSSL","cacert.pem", package="RCurl"),httpheader=HEADERS)
        parsed.data <- data.frame(fromJSON(json.data))
        parsed.data[,2] <- as.POSIXct(parsed.data[,2], origin = "1970-01-01")
        names(parsed.data) <- c("label","timestamp","long_position_ratio","exchange_rate")
        setDT(parsed.data)
    }, error = function(e) e)

    return(parsed.data)

}


#' Spread information for a supported instrument
#'
#' Returns up to 1 year worth of spread information for a supported instrument.
#' The returned data is divided in 15 minute intervals.
#' For each period, we provide the time weighted average, minimum, and maximum spread.
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param INSTRUMENTS Instrument to get candlestick data for e.g. "AUD_USD"
#' @param PERIOD Required Period of time to retrieve calendar data for.
#'
#' @details
#' Period includes:\cr
#'  - 1 hour\cr
#'  - 12 hours\cr
#'  - 1 day\cr
#'  - 1 week\cr
#'  - 1 month\cr
#'  - 3 months\cr
#'  - 6 months\cr
#'  - 1 year\cr
#'
#' @return A \code{list}
#'
#' @examples
#' spreads = SpreadsOanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', PERIOD = '1 year')
#' spreads
#'
#' @export
SpreadsOanda <- function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'EUR_USD', PERIOD = '1 year')
{

    library(data.table)
    library(RCurl)
    library(jsonlite)
    # 86400 - 1 day - 20 minute snapshots
    # 172800 - 2 day - 20 minute snapshots
    # 604800 - 1 week - 1 hour snapshots
    # 2592000 - 1 month - 3 hour snapshots
    # 7776000 - 3 months - 3 hour snapshots
    # 15552000 - 6 months - 3 hour snapshots
    # 31536000 - 1 year - daily snapshots

    lookupPeriod <- list("1 hour" = 3600,
                         "12 hours" = 43200,
                         "1 day" = 86400,
                         "1 week" = 604800,
                         "1 month" = 2592000,
                         "3 months" = 7776000,
                         "6 months" = 15552000,
                         "1 year" = 31536000
    )

    PERIOD = lookupPeriod[[tolower(PERIOD)]]

    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL = paste0(URL, "/labs/v1/spreads?instrument=", INSTRUMENTS, '&period=', PERIOD)

    # Headers -----------------------------------------------------------------
    HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "),'Content-Type' = 'application/json')

    parsed.data <- NULL
    tryCatch({
        json.data <- getURL(URL,cainfo=system.file("CurlSSL","cacert.pem", package="RCurl"),httpheader=HEADERS)
        parsed.data <- fromJSON(json.data)
        spread.avg <- data.frame(parsed.data$avg); names(spread.avg) <- c("timestamp", "Avg_Spreads"); spread.avg$timestamp <- as.POSIXct(spread.avg$timestamp, origin = "1970-01-01")
        spread.min <- data.frame(parsed.data$min); names(spread.min) <- c("timestamp", "Min_Spreads"); spread.min$timestamp <- as.POSIXct(spread.min$timestamp, origin = "1970-01-01")
        spread.max <- data.frame(parsed.data$max); names(spread.max) <- c("timestamp", "Max_Spreads"); spread.max$timestamp <- as.POSIXct(spread.max$timestamp, origin = "1970-01-01")
        setDT(spread.max)
        setDT(spread.min)
        setDT(spread.avg)
    }, error = function(e) e)

    return(list(spread.avg = spread.avg,
                spread.min = spread.min,
                spread.max = spread.max))
}





#' Commitments of Traders data from the CFTC for supported currencies
#'
#' Returns up to 4 years worth of Commitments of Traders data from the CFTC for supported currencies.
#' This is essentially CFTC’s Non-Commercial order book data.
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param INSTRUMENTS Instrument to get candlestick data for e.g. "AUD_USD"
#'
#' @return A \code{data.table}
#'
#' @examples
#' cot = COT.Oanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = 'AUD_USD')
#' cot
#'
#' @export
COT.Oanda <- function(ACCOUNT_TYPE, ACCESS_TOKEN, INSTRUMENTS = 'EUR_USD')
{

    library(data.table)
    library(RCurl)
    library(jsonlite)
    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL = paste0(URL, "/labs/v1/commitments_of_traders?instrument=", INSTRUMENTS)

    # Headers -----------------------------------------------------------------
    HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "),'Content-Type' = 'application/json')

    parsed.data <- NULL
    tryCatch({
        json.data <- getURL(URL,cainfo=system.file("CurlSSL","cacert.pem", package="RCurl"),httpheader=HEADERS)
        parsed.data <- fromJSON(json.data)[[1]]
        names(parsed.data) <- c("Contract_Size", "Non_Commercial_Short", "Date", "Price", "Non_Commercial_Long", "Overall Interest")
        parsed.data <- parsed.data[, c("Date", "Price", "Non_Commercial_Long", "Non_Commercial_Short", "Overall Interest", "Contract_Size")]
        setDT(parsed.data)
        parsed.data$Date <- as.POSIXct(parsed.data$Date,origin = "1970-01-01")

    }, error = function(e) e)

    return(parsed.data)
}



#' Returns ‘Our Favourites’ signals from Autochartist.
#'
#' Returns ‘Our Favourites’ signals from Autochartist. These signals can be a Chart Pattern, or a Key Level.
#' Chart Patterns consists of an upper and a lower line forming a pattern, the pattern’s end time, and the prediction price box.
#' Key Levels have only a price level.
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param INSTRUMENTS Instrument to get candlestick data for e.g. "AUD_USD"
#' @param PERIOD Required Period of time to retrieve calendar data for.
#'
#' @details
#' Period includes:\cr
#'  - 4 hours\cr
#'  - 8 hours\cr
#'  - 12 hours\cr
#'  - 1 day\cr
#'  - 1 week\cr
#'
#' @return A \code{list}
#'
#' @examples
#' signals = Autochartist.Oanda(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = 'AUD_USD', PERIOD = '1 week')
#' signals
#'
#' @export
Autochartist.Oanda <- function(ACCOUNT_TYPE, ACCESS_TOKEN, INSTRUMENTS = 'EUR_USD', PERIOD = '1 week')
{

    library(data.table)
    library(RCurl)
    library(jsonlite)
    # 14400 - 4 hours
    # 28800 - 8 hours
    # 43200 - 12 hours
    # 86400 - 1 day
    # 604800 - 1 week

    # keylevel
    # chartpattern

    lookupPeriod <- list("4 hours" = 14400,
                         "8 hours" = 28800,
                         "12 hours" = 43200,
                         "1 day" = 86400,
                         "1 week" = 604800
    )

    PERIOD = lookupPeriod[[tolower(PERIOD)]]

    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL1 = paste0(URL, "/labs/v1/signal/autochartistinstrument=", INSTRUMENTS, '&period=', PERIOD, "&type=keylevel")
    URL2 = paste0(URL, "/labs/v1/signal/autochartistinstrument=", INSTRUMENTS, '&period=', PERIOD, "&type=chartpattern")

    # Headers -----------------------------------------------------------------
    HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "),'Content-Type' = 'application/json')

    tryCatch({
        json.data1 <- getURL(URL1,cainfo=system.file("CurlSSL","cacert.pem", package="RCurl"),httpheader=HEADERS)
        json.data2 <- getURL(URL2,cainfo=system.file("CurlSSL","cacert.pem", package="RCurl"),httpheader=HEADERS)
        parsed.data1 <- fromJSON(json.data1, simplifyDataFrame = T, flatten = T)
        parsed.data2 <- fromJSON(json.data2, simplifyDataFrame = T, flatten = T)

        provider.keylevel <- parsed.data1$provider
        signals.keylevel <- data.frame(parsed.data1$signals)
        # names(signals.keylevel) <- c("type", "instrument", "id", "patternendtime", "prediction_")

        provider.chartpattern <- parsed.data2$provider
        signals.chartpattern <- data.frame(parsed.data2$signals)

    }, error = function(e) e)

    return(list(signals.keylevel = signals.keylevel,
                signals.chartpattern = signals.chartpattern))
}
