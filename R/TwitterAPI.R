#' Sets up the OAuth credentials for a twitteR session
#'
#' This function wraps the OAuth authentication handshake functions from the httr package for a twitteR session.
#'
#' @examples
#' setupTwitterConn()
#'
#' @export
setupTwitterConn <- function(){
    library(twitteR)
    library(NLP)
    library(tm)
    library(RColorBrewer)
    library(wordcloud)
    library(topicmodels)
    library(SnowballC)
    setup_twitter_oauth(.twitterEnv$ckey,
                        .twitterEnv$csec,
                        .twitterEnv$akey,
                        .twitterEnv$asec)
}


#' Convenience function for accessing the text part of a tweet
#'
#' Convenience function for accessing the text part of a tweet returned by the twitteR API.
#'
#' @export
tweet_text <- function(x){
    x$getText()
}


#' Submit a search query (terms separated by "+") and get a return
#'
#' Submit a search query (terms separated by "+") and get a return set of data (corpus).
#'
#' @param search Search query to issue to twitter. Use "+" to separate query terms.
#' @param n The maximum number of tweets to return
#' @param since If not NULL, restricts tweets to those since the given date. Date is to be formatted as YYYY-MM-DD
#' @param until If not NULL, restricts tweets to those up until the given date. Date is to be formatted as YYYY-MM-DD
#' @param sinceID If not NULL, returns tweets with IDs greater (ie newer) than the specified ID
#' @param maxID If not NULL, returns tweets with IDs smaller (ie older) than the specified ID
#'
#' @return A \code{list} containing a vector of all doc and a data.frame of all tweets
#' @examples
#' setupTwitterConn()
#' tweets <- tweet_corpus(search = "audusd OR aud_usd OR aud/usd", n = 1000, since = as.character(Sys.Date()-1), until = as.character(Sys.Date()))
#'
#' @export
tweet_corpus <- function(search, n = 5000, since = NULL, until = NULL, sinceID = NULL, maxID = NULL, ...) {
    payload <- searchTwitter(search, n = n, since = since, until = until, sinceID = sinceID, maxID = maxID, ...)

    tryCatch({
        v <- sapply(payload, tweet_text)
        d <- twListToDF(payload)
    })

    return(list(v = v,
                d = d))
}


#' Submit a search query by chuncks
#'
#' Submit a search query by chuncks
#'
#' @param search Search query to issue to twitter. Use "+" to separate query terms.
#' @param n The maximum number of tweets to return
#' @param until If not NULL, restricts tweets to those up until the given date. Date is to be formatted as YYYY-MM-DD
#' @param chunck_size number of days for each iteration
#' @param total_size total number of days to retrieve
#' @param sys_sleep system sleep (sec) to prevent limits rate of API
#'
#' @return A \code{list} containing a vector of all doc and a data.frame of all tweets
#' @examples
#' tweets <- tweet_corpus_chunck(search = "aud+usd", n = 50000, chunck_size = 7, total_size = 15, until = Sys.Date())
#'
#' @export
tweet_corpus_chunck <- function(search = "aud+usd", n = 50000, chunck_size = 7, total_size = 15, until = Sys.Date(), sys_sleep = 0){

    chunck_num = ceiling(total_size/chunck_size)
    cat(paste0("\nTotal ", chunck_num, "."))
    for(i in 1:chunck_num){
        cat(paste0("\nChunck ", i, "......"))
        if(i == 1){
            setupTwitterConn()
            tweets <- tweet_corpus(search = search,
                                   n = n,
                                   since = as.character(until-chunck_size),
                                   until = as.character(until),
                                   lang = 'en')

            tweetsD = tweets$d
            tweetsV = tweets$v
            lastid = min(tweets$d$id)
        }else{
            tweets <- tweet_corpus(search = search,
                                   n = n,
                                   since = as.character(until-(i)*chunck_size),
                                   until = as.character(until-(i-1)*chunck_size-1),
                                   maxID = lastid,
                                   lang = 'en')

            tweetsD = rbind(tweetsD, tweets$d)
            tweetsV = c(tweetsV, tweets$v)
            lastid = min(tweets$d$id)
        }

        Sys.sleep(sys_sleep)
    }

    return(list(d = tweetsD,
                v = tweetsV))
}


#' Extract FX Tweets
#'
#' Extract Twitter by days and save the data in specified folder
#'
#' @param inst1 instrument one
#' @param inst2 instrument two
#'
#' @examples
#' twitterExtracter("AUD", "USD")
#'
#' @export
twitterExtracter <- function(inst1, inst2){
    setupTwitterConn()
    search = paste0(tolower(inst1), "+", tolower(inst2))
    symbol = paste0(toupper(inst1), toupper(inst2))
    dir = paste0('~/analytics/common/twitter/',symbol,'/')
    file = paste0(dir, 'Tweets_',symbol, '_', format(Sys.Date(), format="%Y%m%d"),'.RDS')

    if(!dir.exists(dir)){
        dir.create(dir)
    }

    tweets <- tweet_corpus(search = search,
                           n = 100000,
                           since = as.character( Sys.Date()-10),
                           until = as.character( Sys.Date()))

    saveRDS(tweets, file)
    # tweets = readRDS(tweets.dir)
}



