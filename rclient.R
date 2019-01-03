library(httr)
library(data.table)



# GET INTRADAY PRICES -----------------------------------------------------


GetIntraDay <- function(interval = c("1min", "5min", "15min", "30min", "60min"),
                        outsize = c("compact", "full"),
                        symbols,
                        apikey){
  url <- "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=&interval=&outputsize=&apikey="
  ll <- parse_url(url)
  
  resList <- lapply(symbols, function(i, ll){
    ll$query$symbol     <- i
    ll$query$interval   <- interval
    ll$query$apikey     <- apikey
    ll$query$outputsize <- outputsize
    jsonlite::fromJSON(content(GET(build_url(ll)), "text"))
  }, ll)
  names(resList) <- symbols
  return(resList)
}


# GET STOCK INDEX TABLE ---------------------------------------------------


GetStockIndex <- function(){
  ll <- parse_url("https://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=&render=download")
  stkDT <- rbindlist(lapply(c("nasdaq", "nyse", "amex"), function(i, ll){
    ll$query$exchange <- i
    resp <- GET(build_url(ll))
    parsed <- content(resp, "text")
    dt <- fread(parsed, header = TRUE)[, !"V9"]
    dt[, "Exchange" := i ]
    dt[]
  }, ll))
  return(stkDT)
}



# DEVELOPMENT SCRIPT ------------------------------------------------------



apikey   <- "3L2VN6WKNEJXITRI"
symbols  <- c("MSFT", "ISRG", "BAC")
interval <- "15min"
outsize  <- "full"
GetIntraDay(interval, outsize, symbols, apikey)



TimeSeriesUrl <- c(
  "TIME_SERIES_DAILY",
  "TIME_SERIES_DAILY_ADJUSTED",
  "TIME_SERIES_WEEKLY",
  "TIME_SERIES_WEEKLY_ADJUSTED",
  "TIME_SERIES_MONTHLY",
  "TIME_SERIES_MONTHLY_ADJUSTED"
)

