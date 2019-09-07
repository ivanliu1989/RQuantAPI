# source("./R/settings.R")

# AccountType = "practice"
# Token = '450188cd62d103f23afbbee7e72b1339-d9c47c032f04ff46a65ec24786d11357'
# AccountID = '101-011-4686012-001'
# Headers = add_headers(Authorization = paste("Bearer",Token,sep=" "), "Content-Type" = "application/json")
#
# httpaccount <- "https://api-fxpractice.oanda.com"
# Queryhttp  <- paste(httpaccount,"/v3/accounts/",sep="")
# Queryhttp1 <- paste(Queryhttp,AccountID,sep="")
# Queryhttp2 <- paste(Queryhttp1,"/orders",sep="")
#
# POST(Queryhttp2, config = Headers,
#      body = list(order = list(units = '999',
#                               instrument = 'EUR_USD',
#                               timeInForce = 'FOK',
#                               type = 'MARKET',
#                               positionFill = 'DEFAULT')),
#      encode = "json")
