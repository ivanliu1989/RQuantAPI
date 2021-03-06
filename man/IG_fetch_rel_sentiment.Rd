% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IGAPI.R
\name{IG_fetch_rel_sentiment}
\alias{IG_fetch_rel_sentiment}
\title{IG API Get Sentiment of Related Market}
\usage{
IG_fetch_rel_sentiment(market_id = "VOD-UK", headers,
  url = "https://demo-api.ig.com/gateway/deal/clientsentiment/related",
  timeo = 5)
}
\arguments{
\item{market_id}{market id}

\item{headers}{Object returned from \code{IG_Auth}}

\item{url}{API URL}

\item{timeo}{number of tries}
}
\value{
A \code{data.frame} of related market sentiment information
}
\description{
Get Sentiment of Related Market
}
\examples{
HEADERS = IG_Auth(" ","APIdemo1", " ")
IG_fetch_rel_sentiment(market_id = "VOD-UK", HEADERS)

}
