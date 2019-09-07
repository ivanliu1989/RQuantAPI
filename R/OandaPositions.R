#' List all Positions for an Account
#'
#' @description
#' List all Positions for an Account. The Positions returned are for every instrument that has
#' had a position during the lifetime of an the Account.
#'
#' @param ACCOUNT_TYPE "practice", "real" or "sandbox"
#' @param ACCESS_TOKEN The authorization bearer token previously obtained by the client, can be found in oanda environment
#' @param ACCOUNT_ID 	ID of the Account to create the Order for.
#'
#' @return A \code{list} of account positions
#'
#' @examples
#' getOandaPositions(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, .oandaEnv$ACCOUNT_ID)
#'
#' @export
getOandaPositions <- function(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID){

  # Generate URL ------------------------------------------------------------
  URL = paste0("https://", .oandaEnv$ENVIRONMENTS$api[ACCOUNT_TYPE])
  URL = paste0(URL, "/v3/accounts/", ACCOUNT_ID, "/positions")

  # Headers -----------------------------------------------------------------
  HEADERS <- c(Authorization = paste("Bearer",ACCESS_TOKEN,sep=" "), "Content-Type" = "application/json")

  tryCatch({
    json.data <- getURL(URL,cainfo=system.file("CurlSSL","cacert.pem",
                                               package="RCurl"),httpheader=HEADERS)
    parsed.data <- fromJSON(json.data, simplifyDataFrame = TRUE, flatten = TRUE)
  }, error = function(e) e)

  return(parsed.data)
}
