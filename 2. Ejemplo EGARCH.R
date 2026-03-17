######################################################
############### Tópicos en Econometría ###############
############# Metodologia Box - Jenkins ##############
################## Modelos EGARCH ####################
######################################################

############
# Paquetes #
############
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("e1071")
install.packages("FinTS")
install.packages("tseries")
install.packages("rugarch")
install.packages("forecast")
install.packages("lmtest")
install.packages("lubridate")
library(quantmod)
library(PerformanceAnalytics)
library(e1071)
library(FinTS)
library(tseries)
library(rugarch)
library(forecast)
library(lmtest)
library(lubridate)


# Datos
data(sp500ret)

#sp500ret <- ts(sp500ret, 
#               freq=251, 
#               start=decimal_date(ymd("1987-03-10")))

#head(sp500ret,5)
#tail(sp500ret, 5)

par(mfrow=c(1,1))
ts.plot(sp500ret,main="Gráfica de la Serie sp500ret", col = "red")


######################
# Estimación - ARIMA #
######################
autoArima <- auto.arima(sp500ret, d = 0)
autoArima
coeftest(autoArima)

#######################
# Diagnostico - ARIMA #
#######################
tsdiag(autoArima)
Box.test(autoArima$residuals,lag=1)

#autocorr.res(mod1, rezago.max=36)

# ARCH-LM Test
arima.ArchTest <- ArchTest(autoArima$residuals, lags=8, demean=TRUE)
arima.ArchTest

res2_arima <- autoArima$residuals^2
par(mfrow=c(1,2))
facs <- acf(res2_arima,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(res2_arima,main="FACP", lag.max = 36,ylim=c(-1,1))
facp


#############################
# Estimación - Modelo GARCH #
#############################

garch_mod <- ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(3,3)))
garch_mod

garch_mod_fit <- ugarchfit(garch_mod, data = sp500ret)
garch_mod_fit

# Autocorrelación
e_garch_sd <- egarch_mod_fit@fit$residuals/egarch_mod_fit@fit$sigma
par(mfrow=c(1,2))
facs <- acf(e_garch_sd,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(e_garch_sd,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

# Efecto ARCH
e_garch_sd2 <- e_garch_sd^2
par(mfrow=c(1,2))
facs <- acf(e_garch_sd2,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(e_garch_sd2,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

##############################
# Estimación - Modelo EGARCH #
##############################

egarch_mod <- ugarchspec(mean.model=list(armaOrder=c(3,3), include.mean=TRUE),variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                         distribution.model = "ged")

egarch_mod_fit <- ugarchfit(egarch_mod, data = sp500ret)
egarch_mod_fit

# Autocorrelación
e_garch_sd <- garch_mod_fit@fit$residuals/garch_mod_fit@fit$sigma
par(mfrow=c(1,2))
facs <- acf(e_garch_sd,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(e_garch_sd,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

# Efecto ARCH
e_garch_sd2 <- e_garch_sd^2
par(mfrow=c(1,2))
facs <- acf(e_garch_sd2,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(e_garch_sd2,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

# Volatilidad Condicional
par(mfrow=c(1,2))
plot.ts(egarch_mod_fit@fit$sigma, col = "darkgreen", main = "Volatilidad Condicional (SD)")
# R
# ...existing code...
# Fit an EGARCH(1,1) mean ARMA(3,3) if egarch_mod_fit does not exist
if (!exists("egarch_mod_fit")) {
  library(rugarch)
  egarch_spec <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(3, 3), include.mean = TRUE),
    distribution.model = "norm"
  )
  egarch_mod_fit <- ugarchfit(spec = egarch_spec, data = sp500ret)
}

# obtain standardized residuals (returns a numeric vector)
e_garch_sd <- residuals(egarch_mod_fit, standardize = TRUE)

# ...existing code...
e_garch_sdplot(sigma(egarch_mod_fit), ylab="sigma(t)", col="blue", main = "Volatilidad Condicional (SD)")

sigma2 <- (sigma(egarch_mod_fit))^2
sigma2 <- ts(sigma2, start = c(1), frequency = 1)
par(mfrow=c(1,1))
plot(sigma2, ylab="sigma^2(t)", col="blue", main = "Volatilidad Condicional (Variance)")

plot(egarch_mod_fit, which = "all")


plot(egarch_mod_fi, which = 9)
plot(egarch_mod_fi, which = 1)

##############
# Pronóstico # 
##############

par(mfrow=c(1,1))

# Media
forecast1<-ugarchforecast(garch11_fit,n.ahead = 4)
plot(forecast1, which=1)

# Varianza
forecast2<-ugarchforecast(garch11_fit,n.ahead = 4)
plot(forecast1, which=3)


par(mfrow=c(1,2))
plot(forecast1, which=1)
plot(forecast1, which=3)

