test_that("Get stock index", {
  sdt <- GetStockIndex()
  expect_length(sdt, 9)
})


test_that("get intra day quote", {
  
  tk <- "MSFT"
  ky <- "demo"
  intrall <- GetIntraDay(tk, interval = "5min", ky)
  
  expect_true(names(intrall) == "MSFT")
  expect_length(names(intrall$MSFT$`Time Series (5min)`[[1]]), 5)
})


test_that("get daily quote", {
  
  tk <- "MSFT"
  ky <- "demo"
  tsDayll <- GetTimeSeriesDaily(tk, adj = TRUE, ky)
  expect_equal(ncol(tsDayll$MSFT), 14)
})


test_that("vary time series", {
  
  tk <- "MSFT"
  ky <- "demo"
  
  tsDayll   <- GetTimeSeries(tk, adj = FALSE, period = "day", ky)
  tsWeekll  <- GetTimeSeries(tk, adj = FALSE, period = "wk", ky)
  tsMonthll <- GetTimeSeries(tk, adj = FALSE, period = "month", ky)
  
  expect_equal(ncol(tsDayll$MSFT), 11)
  expect_equal(ncol(tsWeekll$MSFT), 10)
  expect_equal(ncol(tsMonthll$MSFT), 10)
  
  
  tsDayll   <- GetTimeSeries(tk, adj = TRUE, period = "day", ky)
  tsWeekll  <- GetTimeSeries(tk, adj = TRUE, period = "wk", ky)
  tsMonthll <- GetTimeSeries(tk, adj = TRUE, period = "month", ky)
  
  expect_equal(ncol(tsDayll$MSFT), 14)
  expect_equal(ncol(tsWeekll$MSFT), 12)
  expect_equal(ncol(tsMonthll$MSFT), 12)
})










