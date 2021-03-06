% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IGAPI.R
\name{IG_Auth2}
\alias{IG_Auth2}
\title{IG API V2 Login}
\usage{
IG_Auth2(id, p, k, url = "https://demo-api.ig.com/gateway/deal/session",
  timeo = 5)
}
\arguments{
\item{id}{IG Account ID}

\item{p}{API Names}

\item{k}{API key}

\item{url}{API URL}

\item{timeo}{number of tries}
}
\value{
A \code{http} object of api connection
}
\description{
Log into IG API
}
\examples{
HEADERS = IG_Auth(" ","APIdemo1", " ")

}
