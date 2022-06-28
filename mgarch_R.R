
# 1. estimate the series of univariate GARCH
# 2. estimate the correlation


library(readxl)
library(tseries)
library(rugarch)#garch model
library(rmgarch)#mgarch model
library(FinTS)
library(e1071)

GARCH_2022 <- read_excel("F:/DATA_GARCH/GARCH_2022.xlsx",
                         col_types = c("date", "numeric", "numeric",
                                       "numeric", "numeric"), na = "#N/A")

# 1. check the stationarity(Ho = series is not stationary)
kospi.r = diff(log(GARCH_2022$kospi))
nasdaq.r = diff(log(GARCH_2022$nasdaq))
bitcoin.r = diff(log(GARCH_2022$bitcoin))

model1 = ugarchspec(mean.model = list(armaOrder=c(0,0)), variance.model = list(garchOrder=c(1,1), model="sGARCH"), distribution.model = "norm")

modelspec = dccspec(uspec = multispec(replicate(2,model1)), dccOrder = c(1,1), distribution = "mvnorm")

# modelfit = dccfit(modelspec, data = data.frame(nasdaq.r, kospi.r, bitcoin.r))
modelfit = dccfit(modelspec, data = data.frame(kospi.r, nasdaq.r))
modelfit

dccforecast(modelfit, n.ahead=7)

# [correlation] 


modelfitmodel1 = ugarchspec(mean.model = list(armaOrder=c(0,0)), variance.model = list(garchOrder=c(1,1), model="sGARCH"), distribution.model = "norm")

modelspec = dccspec(uspec = multispec(replicate(3,model1)), dccOrder = c(1,1), distribution = "mvnorm")

modelfit = dccfit(modelspec, data = data.frame(kospi.r, nasdaq.r, bitcoin.r))

correlation = rcor(modelfit)
dim(correlation)

# last correlation
correlation[,,dim(correlation)[3]]


corr_kospi_nasdaq = correlation[2,1,]
corr_kospi_bitcoin = correlation[1,3,]
corr_nasdaq_bitcoin = correlation[2,3,]

plot.ts(corr_kospi_nasdaq)
plot.ts(corr_kospi_bitcoin)
plot.ts(corr_nasdaq_bitcoin)

# [compare with 3 graph]
par(mfrow = c(3,1))
plot.ts()

# covariance
covariance = rcov(modelfit)
covariance[,,dim(covariance)[3]]

cov_kospi_nasdaq = covariance[2,1,]
cov_kospi_bitcoin = covariance[1,3,]
cov_nasdaq_bitcoin = covariance[2,3,]

plot.ts(cov_kospi_nasdaq)
plot.ts(cov_kospi_bitcoin)
plot.ts(cov_nasdaq_bitcoin)





