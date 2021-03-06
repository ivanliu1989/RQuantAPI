% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IBrokersInstrument.R
\name{reqIBExecutions}
\alias{reqIBExecutions}
\title{A Wrapper for IB Executions history}
\usage{
reqIBExecutions(twsconn, reqId = "0", ExecutionFilter)
}
\arguments{
\item{twsconn}{connection to current TWS account, if NULL, a new connection will be created within the function}

\item{reqId}{request ID}

\item{ExecutionFilter}{execution criteria}
}
\description{
A Wrapper for IB Executions history
}
\examples{
library(IBrokers)
tws <- twsConnect(port = 7497)
reqIBExecutions(tws, '1', ExecutionFilter)

}
