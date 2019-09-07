#' initialise a new Oanda environment
#'
#' @export
.initOandaEnv <- function(){
    # Oanda Settings ----------------------------------------------------------
    .oandaEnv <- new.env(hash = TRUE)

    .oandaEnv$ACCOUNT_TYPE = 'practice'

    .oandaEnv$ENVIRONMENTS = list(
        streaming = list(
            real = "stream-fxtrade.oanda.com",
            practice = "stream-fxpractice.oanda.com",
            sandbox = "stream-sandbox.oanda.com"
        ),
        api = list(
            real = "api-fxtrade.oanda.com",
            practice = "api-fxpractice.oanda.com",
            sandbox = "api-sandbox.oanda.com"
        )
    )

    .oandaEnv$STREAM_DOMAIN = .oandaEnv$ENVIRONMENTS$streaming[.oandaEnv$ACCOUNT_TYPE]
    .oandaEnv$API_DOMAIN = .oandaEnv$ENVIRONMENTS$api[.oandaEnv$ACCOUNT_TYPE]

    .oandaEnv$ACCESS_TOKEN = 'PUT_YOUR_TOKEN_HERE'
    .oandaEnv$ACCOUNT_ID = 'PUT_YOUR_OANDA_ID_HERE'

    .oandaEnv$RISK_MANAGEMENT = 0.02
    .oandaEnv$INIT_AMOUNT = 1000000

    return(.oandaEnv)
}

#' initialise a new Quandl environment
#'
#' @export
.initQuandlEnv <- function(){
    # Quandl Settings ---------------------------------------------------------
    .quandlEnv <- new.env(hash = TRUE)

    .quandlEnv$API_KEY = ""

    return(.quandlEnv)
}


#' initialise a new DailyFX environment
#'
#' @export
.initDailyFXEnv <- function(){
    # DailyFX -----------------------------------------------------------------
    .dailyfxEnv <- new.env(hash = TRUE)

    # Forex Market News
    .dailyfxEnv$All = "https://rss.dailyfx.com/feeds/all"
    .dailyfxEnv$ForexNewsEvents = "https://rss.dailyfx.com/feeds/forex_market_news"
    .dailyfxEnv$TradingNewsEvents = "https://rss.dailyfx.com/feeds/trading_news_events"
    .dailyfxEnv$Forecasts = "https://rss.dailyfx.com/feeds/forecasts"
    .dailyfxEnv$SpecialReports = "https://rss.dailyfx.com/feeds/weekly_columns"

    # Technical Analysis
    .dailyfxEnv$DailyTechnicals = "https://rss.dailyfx.com/feeds/elliott_wave"
    .dailyfxEnv$Gold = "https://rss.dailyfx.com/feeds/dailyfx_gold_daily_forecast"
    .dailyfxEnv$Oil = "https://rss.dailyfx.com/feeds/dailyfx_oil_daily_forecast"
    .dailyfxEnv$TechnicalArticles = "https://rss.dailyfx.com/feeds/technical_articles"
    .dailyfxEnv$Sentiment = "https://rss.dailyfx.com/feeds/sentiment"
    .dailyfxEnv$SpecialReports = "https://rss.dailyfx.com/feeds/weekly_columns"

    # Alerts
    .dailyfxEnv$Alerts = "https://rss.dailyfx.com/feeds/alerts"
    .dailyfxEnv$AnalystPicks = "https://rss.dailyfx.com/feeds/all_analyst_picks"

    return(.dailyfxEnv)
}


#' initialise a new Twitter environment
#'
#' @export
.initTwitterEnv <- function(){
    .twitterEnv <- new.env(hash = TRUE)

    .twitterEnv$ckey = "" #"Your Consumer Key Here"
    .twitterEnv$csec = "" #"Your Consumer Secret Here"
    .twitterEnv$akey = "" #"Your Access Token Here"
    .twitterEnv$asec = "" #"Your Access Token Secret Here"

    return(.twitterEnv)
}


#' initialise a new Interactive Brokers environment
#'
#' @export
.initIBEnv <- function(){
    .ibEnv <<- new.env(hash = TRUE)

    .ibEnv$PaperPort = 7497
    .ibEnv$RealPort = 7496

    return(.ibEnv)
}
