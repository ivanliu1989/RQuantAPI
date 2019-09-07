#' Get a list of Transactions of Oanda Account
#'
#' @description
#' Get the details of a single Oanda Account Transactions
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#'
#' @return A \code{data.frame} of transactions
#'
#' @examples
#' getOandaTxns(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID)
#'
#' @export
getOandaTxns <- function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, PAGESIZE = 1000, FROM = NULL, TO = NULL){

  # Generate URL ------------------------------------------------------------
  URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
  URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/transactions?pageSize=", PAGESIZE)

  # Headers -----------------------------------------------------------------
  HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "), "Content-Type" = "application/json")

  tryCatch({
    json.data <- getURL(URL,cainfo=system.file("CurlSSL","cacert.pem",
                                               package="RCurl"),httpheader=HEADERS)
    parsed.data <- fromJSON(json.data, simplifyDataFrame = TRUE, flatten = TRUE)
  }, error = function(e) e)

  return(parsed.data)
}



#' Get a list of Transactions of Oanda Account Since ID
#'
#' @description
#' Get the details of a single Oanda Account Transactions
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#' @param SINCE_ID Transaction ID
#'
#' @return A \code{data.frame} of transactions
#'
#' @examples
#' getOandaTxnsID(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID, 0)
#'
#' @export
getOandaTxnsID <- function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, SINCE_ID = 0){

    # Generate URL ------------------------------------------------------------
    URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
    URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/transactions/sinceid?id=", SINCE_ID)

    # Headers -----------------------------------------------------------------
    HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "), "Content-Type" = "application/json")

    tryCatch({
        json.data <- getURL(URL,cainfo=system.file("CurlSSL","cacert.pem",
                                                   package="RCurl"),httpheader=HEADERS)
        parsed.data <- fromJSON(json.data, simplifyDataFrame = TRUE, flatten = TRUE)$transactions
    }, error = function(e) e)

    if(is.na(as.POSIXct(strptime(parsed.data$time[1], "%Y-%m-%dT%H:%M:%OS"), origin="1970-01-01",tz = "UTC"))){
        parsed.data$time <- as.POSIXct(as.numeric(parsed.data$time), origin="1970-01-01",tz = "UTC")
    }else{
        parsed.data$time <- as.POSIXct(strptime(parsed.data$time, "%Y-%m-%dT%H:%M:%OS"),
                                       origin="1970-01-01",tz = "UTC")
    }

    return(parsed.data)
}
