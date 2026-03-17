######################################################
############### Tópicos en Econometría ###############
############# Metodologia Box - Jenkins ##############
################ Modelos ARCH-GARCH ##################
######################################################
# Datos "oil.xls"

############
# Paquetes #
############
install.packages("tseries")
install.packages("forecast")
install.packages("lmtest")
install.packages("rugarch")
install.packages("FinTS")
install.packages("aTSA")
install.packages("lubridate")
install.packages("AnalyzeTS")


library(tseries)
library(forecast)
library(lmtest)
library(rugarch)
library(FinTS)
library(aTSA)
library(lubridate)
library(AnalyzeTS)
library(readxl)

################
# Cargar Datos #
# Then load and read the file
library(rlang)
library(readxl)
y <- read_excel("oil (1).xls")
spot <- ts(y[,2])

spot <- ts(spot, 
           freq=365.25/7, 
           start=decimal_date(ymd("1987-05-15")))

head(spot,33)
tail(spot,53)

# Transformaciones
dspot <- 100*diff(spot, differences = 1)
lspot <- log(spot)
p <- 100*diff(log(spot), differences = 1)

par(mfrow=c(2,2))
ts.plot(spot,main="Gráfica de la Serie (spot)", col = "blue")
ts.plot(dspot,main="Gráfica de la Serie (dspot)", col = "darkgreen")
ts.plot(lspot,main="Gráfica de la Serie (lspot)", col = "red")
ts.plot(p,main="Gráfica de la Serie (p)", col = "purple")

class(p)
summary(p)
start(p)    
end(p)
frequency(p)
par(mfrow=c(1,1))
hist(p, main="Histograma de p", breaks=20, freq=FALSE, col="grey")

##################
# Identificación #
##################
par(mfrow=c(1,2))
facs <- acf(p,main="FACS", lag.max = 10, ylim=c(-1,1))
facs
facp <- pacf(p,main="FACP", lag.max = 10,ylim=c(-1,1))
facp

##############
# Estimación #
##############
######################
# Estimación - ARIMA #
######################

mod1 <- arima(p, order=c(0,0,3),
              fixed=c(NA,rep(0,1),NA,
                      NA),
              method="ML")
mod1
summary(mod1)
coeftest(mod1)

#######################
# Diagnostico - ARIMA #
#######################
tsdiag(mod1)
autoplot(mod1)

# ARCH-LM Test
arima.ArchTest <- ArchTest(mod1$residuals, lags=8, demean=TRUE)
arima.ArchTest

# Errores al Cuadrado
res2_arima <- mod1$residuals^2
par(mfrow=c(1,2))
facs <- acf(res2_arima,main="FACS", lag.max = 36, ylim=c(-1,1))
facp <- pacf(res2_arima,main="FACP", lag.max = 36,ylim=c(-1,1))

##################################
# Estimación - Modelo ARCH-GARCH #
##################################

#ARCH(5)
arch5 <- ugarchspec(mean.model=list(armaOrder=c(0,1), include.mean=TRUE), variance.model=list(garchOrder=c(5,0)),
                    distribution.model = "norm")
arch5_fit <- ugarchfit(arch5, data = p)
arch5_fit
coef(arch5_fit)

#GARCH(1,1)
garch11 <- ugarchspec(mean.model=list(armaOrder=c(0,1), include.mean=TRUE), variance.model=list(garchOrder=c(1,1)),
                      distribution.model = "ged")
garch11_fit <- ugarchfit(garch11, data = p)
garch11_fit
coef(garch11_fit)

#Diagnóstico ARCH
e_garch_sd <- arch5_fit@fit$residuals/arch5_fit@fit$sigma
par(mfrow=c(1,2))
facs <- acf(e_garch_sd,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(e_garch_sd,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

e_garch_sd2 <- e_garch_sd^2
par(mfrow=c(1,2))
facs <- acf(e_garch_sd2,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(e_garch_sd2,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

#Diagnóstico GARCH
e_garch_sd <- garch11_fit@fit$residuals/garch11_fit@fit$sigma
par(mfrow=c(1,2))
facs <- acf(e_garch_sd,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(e_garch_sd,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

e_garch_sd2 <- e_garch_sd^2
par(mfrow=c(1,2))
facs <- acf(e_garch_sd2,main="FACS", lag.max = 36, ylim=c(-1,1))
facs
facp <- pacf(e_garch_sd2,main="FACP", lag.max = 36,ylim=c(-1,1))
facp

# Volatilidad Condicional
par(mfrow=c(1,2))
plot.ts(garch11_fit@fit$sigma, col = "darkgreen", main = "Volatilidad Condicional (SD)")
plot(sigma(garch11_fit), ylab="sigma(t)", col="blue", main = "Volatilidad Condicional (SD)")

sigma2 <- (sigma(garch11_fit))^2
sigma2 <- ts(sigma2,    freq=365.25/7, 
             start=decimal_date(ymd("1987-05-15")))
par(mfrow=c(1,1))
plot.ts(sigma2, ylab="sigma^2(t)", col="blue", main = "Volatilidad Condicional (Variance)")

plot(garch11_fit, which = "all")
plot(garch11_fit, which = 9)
plot(garch11_fit, which = 1)

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
