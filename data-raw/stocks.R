
library(lubridate)
library(xts)

stocks <- xts(coredata(EuStockMarkets),
              order.by = as.Date(lubridate::date_decimal(index(EuStockMarkets))))

usethis::use_data(stocks)
