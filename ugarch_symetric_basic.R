
# 1. check the stationarity(Ho = series is not stationary)
# 2. check volatility clustering
# 3. check ARCH effect (Ho = series is not ARCH effect)
# 4. GARCH model
# 5. garch specificarion output
# 6. plot the graph
# 7. Forecast the volatility
# 8. various graphs

library(readxl)
library(tseries)
library(rugarch)#garch model
library(FinTS)
library(e1071)
library(zoo) #about NA

GARCH_2022 <- read_excel("F:/DATA_GARCH/GARCH_2022.xlsx",
                         col_types = c("date", "numeric", "numeric",
                                       "numeric", "numeric"), na = "#N/A")

# 1. check the stationarity(Ho = series is not stationary)
adf.test(GARCH_2022$kospi)

kospi.r = diff(log(GARCH_2022$kospi))

adf.test(kospi.r)

# 2. check volatility clustering
plot.ts(kospi.r)

# 3. check ARCH effect (Ho = series is not ARCH effect)
ArchTest(kospi.r)

# 4. GARCH model
garch(kospi.r, grad="numerical", trace=FALSE)
# a: number of ARCH order / b: number of garch order

# 5. garch specificarion output
kospi_garch = ugarchspec(variance.model = list(garchOrder=c(1,1)),
                                              mean.model = list(armaOrder=c(1,1)))
kospi_garch_fit = ugarchfit(kospi_garch, data = kospi.r)
kospi_garch_fit

# 6. plot the graph
news_garch=newsimpact(kospi_garch_fit)
plot(news_garch$zx, news_garch$zy, ylab=news_garch$yexpr, xlab=news_garch$xexpr, main="News Impact Curve")

# 7. Forecast the volatility
kospi_forecast = ugarchforecast(kospi_garch_fit, n.head=10)
kospi_forecast = ugarchforecast(kospi_garch_fit, n.ahead=20)
kospi_forecast

# 8. various graphs
plot(kospi_garch_fit)