#' Index of all Tickers
#' 
#' Get an Index of All Tickers listed on the three primary exchanges: NYSE, NASDAQ, and AMEX
#' 
#' @import httr
#' @import data.table
#' 
#' @export
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