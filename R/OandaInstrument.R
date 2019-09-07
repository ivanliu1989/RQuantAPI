#' Fetch Candlestick Data for an Instrument from Oanda
#'
#' Fetch Candlestick Data for an Instrument from Oanda
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param INSTRUMENTS Instrument to get candlestick data for e.g. "AUD_USD"
#' @param count The number of candlesticks to return in the reponse. Count should not be specified if both the start and end parameters are provided
#' @param price The Price component(s) to get candlestick data for. Can contain any combination of the characters “M” (midpoint candles) “B” (bid candles) and “A” (ask candles). [default=M]
#' @param granularity The granularity of the candlesticks to fetch [default=S5]
#'
#'
#' @return A \code{data.frame} of candlesticks
#'
#' @examples
#' dat = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = 'AUD_USD', price = 'BA', granularity = 'M5', count = 500)
#'
#' @export
getOandaInstrumentCandles <- function(ACCOUNT_TYPE, ACCESS_TOKEN, INSTRUMENTS = 'AUD_USD', count = 500,
                                      price = "BA", granularity = "S5", ...){

    library(RCurl)
    library(jsonlite)
    # Must
    price = paste("price=",price,sep="")
    granularity = paste("granularity=",granularity,sep="")
    QUERY <- paste(price, granularity, sep="&")

    if(exists('count')){count = paste("count=",count,sep=""); QUERY <- paste(QUERY, count, sep="&")}

    # Optional1
    if(exists('from')){from = paste("from=",from,sep=""); QUERY <- paste(QUERY, from, sep="&")}
    if(exists('to')){to = paste("to=",to,sep=""); QUERY <- paste(QUERY, to, sep="&")}

    # Optional2
    if(exists('smooth.param')) {smooth.param = paste("smooth=",smooth.param,sep=""); QUERY <- paste(QUERY, smooth.param, sep="&")}
    if(exists('includeFirst')) {includeFirst = paste("includeFirst=",includeFirst,sep=""); QUERY <- paste(QUERY, includeFirst, sep="&")}
    if(exists('dailyAlignment')) {dailyAlignment = paste("dailyAlignment=",dailyAlignment,sep=""); QUERY <- paste(QUERY, dailyAlignment, sep="&")}
    if(exists('alignmentTimezone')) {alignmentTimezone = paste("alignmentTimezone=",alignmentTimezone,sep=""); QUERY <- paste(QUERY, alignmentTimezone, sep="&")}
    if(exists('weeklyAlignment')) {weeklyAlignment = paste("weeklyAlignment=",weeklyAlignment,sep=""); QUERY <- paste(QUERY, weeklyAlignment, sep="&")}


    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL = paste0(URL, "/v3/instruments/", INSTRUMENTS, "/candles?")
    URL = paste0(URL, QUERY)


    # Headers -----------------------------------------------------------------
    HEADERS <- c("Accept-Datetime-Format" = "RFC3339", "Authorization" = paste("Bearer",ACCESS_TOKEN,sep=" "), "Content-Type" = "application/json")

    tryCatch({
        json.data <- getURL(URL,cainfo=system.file("CurlSSL","cacert.pem",
                                                   package="RCurl"),httpheader=HEADERS)
        parsed.data <- fromJSON(json.data, simplifyDataFrame = TRUE, flatten = TRUE)$candles

    }, error = function(e) e)

    if(is.na(as.POSIXct(strptime(parsed.data$time[1], "%Y-%m-%dT%H:%M:%OS"), origin="1970-01-01",tz = "UTC"))){
        parsed.data$time <- as.POSIXct(as.numeric(parsed.data$time), origin="1970-01-01",tz = "UTC")
    }else{
        parsed.data$time <- as.POSIXct(strptime(parsed.data$time, "%Y-%m-%dT%H:%M:%OS"),
                                       origin="1970-01-01",tz = "UTC")
    }

    return(parsed.data)
}
