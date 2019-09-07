source("./R/settings.R")
library(AutoPairTrading)
library(TTR)
library(data.table)
library(xts)

# 1. GET PRICES -----------------------------------------------------------
AUDUSD = getCandles(ACCOUNT_TYPE, ACCESS_TOKEN, INSTRUMENTS = 'AUD_USD', price = 'MBA', granularity = 'M5', count = 500)
USDCAD = getCandles(ACCOUNT_TYPE, ACCESS_TOKEN, INSTRUMENTS = 'USD_CAD', price = 'MBA', granularity = 'M5', count = 500)

AUDUSD.M = AUDUSD[, c('time', 'mid.c')]
USDCAD.M = USDCAD[, c('time', 'mid.c')]
CADUSD.M = USDCAD.M; CADUSD.M$mid.c = 1/as.numeric(USDCAD.M$mid.c)

y <- as.xts(as.numeric(AUDUSD.M$mid.c), order.by = AUDUSD.M$time); colnames(AUDUSD.M) <- "AUDUSD"
x <- as.xts(as.numeric(CADUSD.M$mid.c), order.by = CADUSD.M$time); colnames(CADUSD.M) <- "CADUSD"

pairs <- na.omit(merge(y, x))
price.ratio = getPriceRatio(y, x, FALSE)


# 2. Correlation tests ----------------------------------------------------
cor.test <- CorrelationTest(y, x)


# 3. Stationary tests -----------------------------------------------------
adf.y <- AugmentedDickeyFullerTest(y, type = "drift", lags = 1); cat(paste0("P-value: ", adf.y$signif[[1]]))
adf.x <- AugmentedDickeyFullerTest(x, type = "drift", lags = 1); cat(paste0("P-value: ", adf.x$signif[[1]]))
adf.y.ret <- AugmentedDickeyFullerTest(ROC(y)[-1], type = "drift", lags = 1); cat(paste0("P-value: ", adf.y.ret$signif[[1]]))
adf.x.ret <- AugmentedDickeyFullerTest(ROC(x)[-1], type = "drift", lags = 1); cat(paste0("P-value: ", adf.x.ret$signif[[1]]))


# 4. Cointegration tests --------------------------------------------------
adf.ratio <- AugmentedDickeyFullerTest(price.ratio, type = "drift", lags = 1); cat(paste0("P-value: ", adf.ratio$signif[[1]]))
jc.test <- JohansenCointegrationTest(merge(y,x), type = "trace", ecdet = "none", K = 2); cat(paste0("P-value: ", jc.test$signif[[1]]))


# 5. Half-life tests ------------------------------------------------------
half.life <- HalfLifeMeanReversion(price.ratio)$half.life.round; cat(paste0("Half-Life: ", half.life))
half.life <- min(nrow(price.ratio), half.life)

# 6. Hurse Exponent tests -------------------------------------------------
hurst.test <- HurstExponentTest(price.ratio, min(200, half.life)); cat(paste0("Hurse Exponent: ", mean(hurst.test$hurstKY, na.rm = T)))


# 7. Hedge Ratio Calculation ----------------------------------------------
hedgeRatio <- HedgeRatioOLS(y[(length(y)-half.life):length(y)], x[(length(x)-half.life):length(x)])
hedgeRatio


# 8. Strategies ---------------------------------------------------------
rm(list=ls());gc()
in_short = FALSE
in_long = FALSE
longTrigger = -1
shortTrigger = 1

for(i in 1:10000){

  source("R/settings.R")
  library(AutoPairTrading)
  library(TTR)
  library(data.table)
  library(xts)

  ACCOUNT_ID = '101-011-4686012-002'
  cat(paste0('Iter: ', i, ' | ', Sys.time(),'\n'))

  AUDUSD = getCandles(ACCOUNT_TYPE, ACCESS_TOKEN, INSTRUMENTS = 'AUD_USD', price = 'M', granularity = 'M5', count = 1000)
  USDCAD = getCandles(ACCOUNT_TYPE, ACCESS_TOKEN, INSTRUMENTS = 'USD_CAD', price = 'M', granularity = 'M5', count = 1000)

  AUDUSD.M = AUDUSD[, c('time', 'mid.c')]
  USDCAD.M = USDCAD[, c('time', 'mid.c')]
  CADUSD.M = USDCAD.M; CADUSD.M$mid.c = 1/as.numeric(USDCAD.M$mid.c)

  y <- as.xts(as.numeric(AUDUSD.M$mid.c), order.by = AUDUSD.M$time); colnames(AUDUSD.M) <- "AUDUSD"
  x <- as.xts(as.numeric(CADUSD.M$mid.c), order.by = CADUSD.M$time); colnames(CADUSD.M) <- "CADUSD"

  pairs <- na.omit(merge(y, x))
  price.ratio = getPriceRatio(y, x, FALSE)

  half.life <- 300

  zc <- zscores(price.ratio)
  indicator <- as.numeric(tail(zc,1))

  # golong = (!in_long & indicator < longTrigger)
  # goshort = (!in_short & indicator > shortTrigger)

  golong = indicator < longTrigger
  goshort = indicator > shortTrigger

  cat(paste0('Long: ', golong, ' | Short: ', goshort, '\n'))
  # calculate hedge ratio
  hedgeRatio <- HedgeRatioOLS(y[(length(y)-half.life):length(y)], x[(length(x)-half.life):length(x)])$beta


  if(golong){
    units = round(INIT_AMOUNT * RISK_MANAGEMENT)
    AUD = createMarketOrder(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = units)
    CAD = createMarketOrder(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_CAD', UNITS = -round(units * hedgeRatio))
  }

  if(goshort){
    units = round(INIT_AMOUNT * RISK_MANAGEMENT)
    AUD = createMarketOrder(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = -units)
    CAD = createMarketOrder(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_CAD', UNITS = round(units * hedgeRatio))
  }

  if(!goshort & !golong){
    pos = getPositions(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID)$positions
    AUD.long = as.numeric(pos[pos$instrument == 'AUD_USD', "long.units"])
    AUD.short = as.numeric(pos[pos$instrument == 'AUD_USD', "short.units"])
    CAD.long = as.numeric(pos[pos$instrument == 'USD_CAD', "long.units"])
    CAD.short = as.numeric(pos[pos$instrument == 'USD_CAD', "short.units"])

    if(AUD.long > 0) createMarketOrder(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = -AUD.long)
    if(AUD.short < 0) createMarketOrder(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = -AUD.short)
    if(CAD.long > 0) createMarketOrder(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_CAD', UNITS = -CAD.long)
    if(CAD.short < 0) createMarketOrder(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'USD_CAD', UNITS = -CAD.short)
  }

  Sys.sleep(300)
}
