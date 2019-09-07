#' Notification of IB Gateway Interruption
#'
#' Notification of IB Gateway Interruption
#'
#' @examples
#' checkIBConnection()
#'
#' @export
checkIBConnection = function(toAddress = c('ivan.liuyanfeng@gmail.com')){
    # Check connection status
    if(exists('tws')) rm(tws)
    loadQuantPackages()
    tryCatch({
        tws <- twsConnect(port = 7497, clientId = 890624)
        twsapistatus = isConnected(tws)
        twsDisconnect(tws)

    },
    warning = function(w){
        msg = w
    },
    error = function(e){
        twsapistatus = FALSE
        msg = e
    },
    finally = {
        if(!exists('twsapistatus')) twsapistatus = FALSE
        print(twsapistatus)
    }
    )

    # Send Email
    if(!twsapistatus){

        # Try to reconnect
        Sys.sleep(60)
        tryCatch({
            tws <- twsConnect(port = 7497, clientId = 890624)
            twsapistatus = isConnected(tws)
            twsDisconnect(tws)

        },
        warning = function(w){
            msg = w
        },
        error = function(e){
            twsapistatus = FALSE
            msg = e
        },
        finally = print(twsapistatus)
        )

        if(twsapistatus){
            recoverStatus = 'The connections have been recovered by IBController'
        }else{
            recoverStatus = 'IBController failed to recover connections in last 60 secs, please mannually restart by using following commands<br>
            cd ~/ibcontroller.paper<br>
            DISPLAY=:0 ./IBControllerStart.sh<br>
            '
        }


        # Prepare log content
        logContent = readLines("/home/ibpaperlog/tws.log")
        logDisconnect = logContent[unique(c(grep(pattern = 'disconnected', ignore.case = TRUE, x = logContent),
                                            grep(pattern = 'disconnection', ignore.case = TRUE, x = logContent)))]




        my.body <- c(
            toHTML("Interactive Brokers TWS / Gateway API Disconnected", "h2")
            ,toHTML(hr())

            ,toHTML("Summary", "h3")
            ,paste0("<p>
                    <b>Time: </b> ",Sys.time(),"
                    </p>")
            ,paste0("<p>
                    <b>Connection Status:  </b>", twsapistatus,"
                    </p>")
            ,paste0("<p>
                    <b>Solutions: </b>", recoverStatus,"
                    </p>")
            ,toHTML(hr())
            ,toHTML("Log file details", "h3")
            ,"<p><b>Last 20 Log Messages: </b></p>"
            ,toHTML(tail(logContent, 20))
            ,"<br><p><b>Last 20 Log Messages about Disconnections: </b></p>"
            ,toHTML(tail(logDisconnect, 20))
            )

        my.msg <- buildhtmlmsg(
            my.body
        )

        my.subject<-paste0("Interactive Brokers TWS / Gateway API Disconnected - ", Sys.Date())

        tryCatch({
            result<-RQuantSendMail(
                to = toAddress
                ,subject=my.subject
                ,msg=my.msg$html)
        })

        }
    }































