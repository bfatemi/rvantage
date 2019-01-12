library(rvantage)
library(data.table)
library(parallel)

KEYS_LIST <- c("PN5S1JFY4613LZT4", "3L2VN6WKNEJXITRI", "CMBGS5H0T00HD9C2", "8NG3T31DRYU1SY80", "5BA0JYYUY8EN2Q33", 
               "39312CMUAT3DG6NX", "T1WP5QOLPS2AOVN0", "C5TG3CKGZ2LQ0WGO", "NJZTX3C5O9ZAOLKR", "XPOBKTJ4QIL7CJ80",
               "I6XZIC27QIA0QRB3", "UHCLRFT6732VQ7S0", "2HUDO9JFGUOIEONH", "TVOMCKTKUBFXC5W6", "LCXOMMHNPGXOX0Z5",
               "9QB8Z6LUOBK6M9NU", "ZH13H5XE4U6BBS24", "4VIVIDWP5FZ23BGJ", "VS69V4MKZ614DNJ3", "4LI1GDLBQI4HMJTX",
               "4Z871AMPAXD3HN46", "PISV201EWV5524DH", "836Y8KPUDNPX8JBE", "N77I7BVYOS40JS14", "2XHLSKSVKCCTL03Z")

## DEFINE FUNCTION ##

getQuote <- function(i){
  k <- sample( KEYS_LIST, 1 )
  res <- GetTimeSeriesDaily(tickers = i,
                            adj = TRUE, 
                            outsize = "full", 
                            apikey = k)
  if(is.null( res[[i]] )){
    count_errors <<- count_errors + 1
    
    if( count_errors > 100 ){
      nll <- list(NULL)
      names(nll) <- i
      return(nll)
    }
    return( getQuote(i) ) ## try again
  }
  return(res)
}

## INIT CLUSTER ENV AND EXECUTE CALL ##

cl <- parallel::makeCluster(8)
clusterExport(cl, list("KEYS_LIST", "getQuote"))

invisible( clusterEvalQ(cl, {
  library(rvantage)
  count_errors <- 0
}))

tk <- GetStockIndex()[["Symbol"]][1:20]
res  <- clusterApply(cl, tk, getQuote)
stopCluster(cl)

sll    <- unlist(res, recursive = FALSE, use.names = TRUE)
tk.err <- names(sll[ sapply(sll, is.null) ])
sDT <- rbindlist(sll)


sDT
tk.err

