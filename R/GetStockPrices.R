#' Stock Price Functions
#' 
#' Intraday, daily, then wk/month functions to pull historical or intraday price data
#'
#' @param tickers a vector of one or more tickers to pull data for
#' @param adj a boolean applicable for weekly, monthly, and daily time horizons. Set TRUE for prices adjusted for splits etc.
#' @param interval applicable to IntraDay Prices only. One of: "1min", "5min", "15min", "30min", "60min"
#' @param period applicable when needing to specify one of "wk" or "month"
#' @param apikey API key from vendor in order to authorize api calls
#' 
#' @import data.table
#' @importFrom httr GET content parse_url build_url
#' @importFrom stringr str_replace_all str_replace
#' @importFrom jsonlite fromJSON
#' 
#' @name stock_prices
NULL

#' @describeIn stock_prices intraday function. Unique parameter of "interval". See above.
#' @export
GetIntraDay <- function(tickers = "MSFT", 
                        interval = c("1min", "5min", "15min", "30min", "60min"),
                        apikey = "demo"){
  url <- "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=&interval=&apikey="
  ll <- httr::parse_url(url)
  ll$query$interval <- interval
  ll$query$apikey <- apikey
  
  resList <- lapply(tickers, function(i, ll){
    ll$query$symbol <- i
    resp <- httr::GET(httr::build_url(ll))
    jsonlite::fromJSON(httr::content(resp, "text"))
  }, ll)
  names(resList) <- tickers
  return(resList)
}


#' @describeIn stock_prices intraday function. Similar to intraday, has option for compact or full. 
#' Additionally, set \code{adj = TRUE} when prices should be adjusted (see description)
#' @export
GetTimeSeriesDaily <- function(tickers = "MSFT", 
                               adj = FALSE, 
                               apikey = "demo"){
  url <- "https://www.alphavantage.co/query?function=&symbol=&apikey="
  ll <- httr::parse_url(url)
  
  if(adj){
    ll$query[["function"]] <- "TIME_SERIES_DAILY_ADJUSTED"
  }else{
    ll$query[["function"]] <- "TIME_SERIES_DAILY"
  }
  ll$query$apikey <- apikey
  
  
  resList <- lapply(tickers, function(i, ll){
    ll$query$symbol <- i
    tryCatch({
      resp <- httr::GET(httr::build_url(ll))
      parsed <- jsonlite::fromJSON(httr::content(resp, "text"))
      
      cnams1 <- stringr::str_replace_all(stringr::str_replace(names(parsed[[1]]), "[1-9]+\\. ", ""), " ", "_")
      cnams2 <- stringr::str_replace_all(stringr::str_replace(names(parsed[[2]][[1]]), "[1-9]+\\. ", ""), " ", "_")
      
      DT1 <- setDT(parsed[[1]])[]
      setDT(DT1)
      setnames(DT1, cnams1)
      
      DT2 <- rbindlist(lapply(parsed[[2]], as.data.table))
      setDT(DT2)
      setnames(DT2, cnams2)
      
      resultDT <- cbind(
        DT1,
        date = names(parsed[[2]]),
        DT2
      )
      return( resultDT )
      
    }, error = function(c){
      return( NULL )
    })
    
  }, ll )
  
  names(resList) <- tickers
  return( resList )
}
  

#' @describeIn stock_prices function for weekly or monthly data specified by 
#' parameter \code{period} which can be one of: "wk" or "month"
#' @export
GetTimeSeries <- function(tickers = "MSFT",  
                          adj = FALSE, 
                          period = c("day", "wk", "month"), 
                          apikey = "demo"){
  
  if(period == "day")
    return(GetTimeSeriesDaily(tickers, adj, apikey))
  
  
  if(period == "wk")
    fn <- "TIME_SERIES_WEEKLY"
  
  if(period == "month")
    fn <- "TIME_SERIES_MONTHLY"
  
  if(adj)
    fn <- paste0(fn, "_ADJUSTED")
  
  url <- "https://www.alphavantage.co/query?function=&symbol=&apikey="
  ll <- httr::parse_url(url)
  
  ll$query[["function"]] <- fn
  ll$query$apikey <- apikey
  
  resList <- lapply(tickers, function(i, ll){
    ll$query$symbol <- i
    resp <- httr::GET(httr::build_url(ll))
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"))
    
    cnams1 <- stringr::str_replace_all(stringr::str_replace(names(parsed[[1]]), "[1-9]+\\. ", ""), " ", "_")
    cnams2 <- stringr::str_replace_all(stringr::str_replace(names(parsed[[2]][[1]]), "[1-9]+\\. ", ""), " ", "_")
    
    DT1 <- setDT(parsed[[1]])[]
    setDT(DT1)
    setnames(DT1, cnams1)
    
    DT2 <- rbindlist(lapply(parsed[[2]], as.data.table))
    setDT(DT2)
    setnames(DT2, cnams2)
    
    resultDT <- cbind(
      DT1,
      date = names(parsed[[2]]),
      DT2
    )
    return(resultDT)
  }, ll)
  
  names(resList) <- tickers
  return(resList)
}


