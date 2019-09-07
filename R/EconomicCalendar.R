#' Fetch Forex News Feed from DailyFX
#'
#' Fetch Forex News Feed from DailyFX
#'
#' @param NEWS_TYPE the types of the latest DailyFX headlines on the topics that interest you. Default "trading_news_events"
#'
#' @details
#' News Types including (Default "trading_news_events" for the function): \cr
#'  - All\cr
#'  - ForexNewsEvents\cr
#'  - TradingNewsEvents\cr
#'  - Forecasts\cr
#'  - SpecialReports\cr
#'  - TechnicalArticles\cr
#'  - Sentiment\cr
#'  - SpecialReports\cr
#'  - Alerts\cr
#'  - AnalystPicks\cr
#'  - DailyTechnicals\cr
#'  - Gold\cr
#'  - Oil\cr
#'
#' @return A \code{data.table} of the latest DailyFX headlines
#'
#' @examples
#' dat = getHeadlinesDailyFX(NEWS_TYPE = "Sentiment")
#' View(dat)
#'
#' @export
#' @import XML
#' @import RCurl
getHeadlinesDailyFX <- function(NEWS_TYPE = "trading_news_events"){
    library(RCurl)
    library(XML)
    library(data.table)
    xml.url <- .dailyfxEnv[[NEWS_TYPE]]
    script  <- getURL(xml.url)
    doc     <- xmlParse(script)
    titles    <- xpathSApply(doc,'//item/title',xmlValue)
    authors <- xpathSApply(doc,'//item/author',xmlValue)
    descriptions    <- xpathSApply(doc,'//item/description',xmlValue)
    pubdates <- xpathSApply(doc,'//item/pubDate',xmlValue)
    dat <- data.table(titles, authors, descriptions, pubdates)

    return(dat)
}




# library(XML)
# library(RCurl)
# url="https://www.dailyfx.com/calendar"
# url="http://au.investing.com/economic-calendar/"
# html <- getURL(url)
# doc = htmlParse(html)
# doc[['/html/body/span/div[4]/div[2]/div[1]/div[7]/div[7]/table[2]']]
# # xmlGetAttr(doc, name = "class")
# xpathSApply(doc, '/html/body/span/div[4]/div[2]/div[1]/div[7]/div[7]/table[2]/tbody/tr/td', xmlValue, trim = TRUE)
#
#
#
#
# library("rvest")
# url <- "https://www.dailyfx.com/calendar"
# url <- "http://au.investing.com/economic-calendar/"
# population <- url %>%
#     html() %>%
#     html_nodes(xpath='/html/body/span/div[4]/div[2]/div[1]/div[7]/div[7]/table[2]') %>%
#     html_table()
# population <- population[[1]]



# [1] "Neumontage von Klär - und Wasseraufbereitungsanlagenanlagen (teilweise Rohrleitungsbau- Kunststoff und Verkabelungsarbeiten)"
# [2] "Schaltanlagenbau (Verdrahtungsarbeit und Umbauarbeiten)"
# [3] "Störungsbehebung an Steuerungen in Schaltanlagen Wasser- und Abwassertechnik"
# [4] "Aufbauen von Umkehrosmoseanlagen (Meerwasserentsalzung)= Vormontage in unserer Werkstätte."
# [5] "Servicearbeiten in der Abwassertechnik"
# [6] "Wartung von Anlagen"
#
#
#
#
# library(XML)
# library(RCurl)
# url="http://www.karriere.at/jobs/facharbeit-gewerbe-produktion"
# html <- getURL(url)
# doc = htmlParse(html)
#
# url <- paste0("http://www.karriere.at", xmlGetAttr(doc[['/html/body/div[2]/iframe']], "src"))
# doc = htmlParse(getURL(url))
# xpathSApply(doc, "/html/body/div/div[2]/div[3]/ul/li", xmlValue, trim = TRUE)
