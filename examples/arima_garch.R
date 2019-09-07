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
  library(quantmod)
  library(lattice)
  library(timeSeries)
  library(rugarch)
  ACCOUNT_ID = '101-011-4686012-003'

  cat(paste0('Iter: ', i, '\n'))

  AUDUSD = getCandles(ACCOUNT_TYPE, ACCESS_TOKEN, INSTRUMENTS = 'AUD_USD', price = 'M', granularity = 'M5', count = 500)

  AUDUSD.M = AUDUSD[, c('time', 'mid.c')]

  y <- as.xts(as.numeric(AUDUSD.M$mid.c), order.by = AUDUSD.M$time); colnames(AUDUSD.M) <- "AUDUSD"
  rets = na.omit(diff(log(y)))

  # Fit the ARIMA model
  final.aic <- Inf
  final.order <- c(0,0,0)
  for (p in 0:5) for (q in 0:5) {
    if ( p == 0 && q == 0) {
      next
    }
    arimaFit = tryCatch( arima(rets, order=c(p, 0, q)),
                         error=function( err ) FALSE,
                         warning=function( err ) FALSE )
    if( !is.logical( arimaFit ) ) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) {
        final.aic <- current.aic
        final.order <- c(p, 0, q)
        final.arima <- arima(rets, order=final.order)
      }
    } else {
      next
    }
  }
  # Specify and fit the GARCH model
  spec = ugarchspec(
    variance.model=list(garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(
      final.order[1], final.order[3]
    ), include.mean=T),
    distribution.model="sged"
  )
  fit = tryCatch(
    ugarchfit(
      spec, rets, solver = 'hybrid'
    ), error=function(e) e, warning=function(w) w
  )
  # If the GARCH model does not converge, set the direction to "long" else
  # choose the correct forecast direction based on the returns prediction
  # Output the results to the screen and the forecasts vector
  if(is(fit, "warning")) {
    print("Model warning. Skip for next round!")
  } else {
    fore = ugarchforecast(fit, n.ahead=1)
    ind = fore@forecast$seriesFor
    print(paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=","))

    units = round(INIT_AMOUNT * RISK_MANAGEMENT)
    if(ind[1] < 0) createMarketOrder(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = -units)
    if(ind[1] > 0) createMarketOrder(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, INSTRUMENTS = 'AUD_USD', UNITS = units)
  }

  Sys.sleep(60)
}
