#' Index of all Tickers
#' 
#' Get an Index of All Tickers listed on the three primary exchanges: NYSE, NASDAQ, and AMEX
#' 
#' @param FULL boolean defaults to false. If set true, does not filter out based on data issues
#' 
#' @import httr
#' @import data.table
#' 
#' @export
GetStockIndex <- function(full = FALSE){
  ll <- parse_url("https://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=&render=download")
  
  DT <- rbindlist(lapply(c("nasdaq", "nyse", "amex"), function(i, ll){
    ll$query$exchange <- i
    resp <- GET(build_url(ll))
    parsed <- content(resp, "text")
    dt <- fread(parsed, header = TRUE)[, !"V9"]
    dt[, "Exchange" := i ]
    dt[]
  }, ll))
  
  if(full)
    return(DT)
  
  for(cnam in names(DT)){
    set(x = DT, 
        i = NULL, 
        j = cnam, value = stringr::str_replace_all(DT[, get(cnam)], 
                                                   pattern = "n\\/a", 
                                                   replacement = NA_character_))
  }
  set(x = DT, 
      i = NULL, 
      j = "MarketCap", 
      value = stringr::str_replace_all(DT$MarketCap, 
                                       pattern = "\\$|[A-Z]", 
                                       replacement = ""))
  
  DT[, LastSale := as.numeric(LastSale) ]
  DT[, MarketCap := as.numeric(MarketCap) ]
  DT[, IPOyear := as.integer(IPOyear) ]
  
  return( DT[ !is.na(MarketCap) & !is.na(IPOyear) ] )
}