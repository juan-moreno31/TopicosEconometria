#################################################################
#################### Tópicos en Econometría #####################
######## Aplicaciones a la Macreoconomía y las Finanzas #########
#################### Analisis de Portafolios ####################
#################################################################
 
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("tidyverse")
install.packages("highcharter")
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(highcharter)

rm(list=ls())
# Para evitar n?meros en notaci?n cient?fica
options(scipen=999)

# Descargamos autom?ticamente los datos de las 5 acciones, 
# fusionamos los datos y calculamos los rendimientos
symbols <- c("TSLA","MSFT","NEE","WMT","PFE")

prices <- getSymbols(symbols,src = 'yahoo',
                     periodicity = "monthly",
                     from = "2018-01-01",
                     auto.assign = TRUE,
                     warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)


# Retornos
returns <- Return.calculate(prices) %>%
  na.omit() 

returns <- Return.calculate(prices, method = "log") %>%
  na.omit() 

# Otra forma
returns2 <- na.omit(diff(log(prices)))
# Los objetos return y return2 tendr?n exactamente los mismos retornos

table.Stats(returns) 

# Visualizaci?n del riesgo y el retorno de los activos
chart.Boxplot(returns)

charts.PerformanceSummary(returns, 
                          main = "Performance of $1.00 over time",
                          wealth.index = TRUE)

# Sin TESLA
charts.PerformanceSummary(returns[,2:5], 
                          main = "Performance of $1.00 over time",
                          wealth.index = TRUE)

#################################
# Conformaci?n de un Portafolio #
#################################

# Equitativo
w_ew <- rep(0.2,5)

# Agresivo
w_aggressive <- c(0.4,0.3,0.2,0,0.1)

# Retornos
portfolio_returns_ew <- Return.portfolio(returns, weights = w_ew) %>%
  `colnames<-`("returns")

portfolio_returns_ag <- Return.portfolio(returns, weights = w_aggressive) %>%
  `colnames<-`("returns")

charts.PerformanceSummary(portfolio_returns_ew, 
                          main = "Equally-weighted Portfolio")

charts.PerformanceSummary(portfolio_returns_ag, 
                          main = "Aggressive Portfolio Performance")

# Gr?ficos din?micos del rendimiento mensual de cada portafolio

highchart(type = "stock") %>% 
  hc_title(text = "Equally-weighted Portfolio") %>% 
  hc_add_series(portfolio_returns_ew$returns, 
                name = "Monthly Returns", 
                color = "cornflowerblue") %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_exporting(enabled = TRUE)

highchart(type = "stock") %>% 
  hc_title(text = "Aggressive Portfolio") %>% 
  hc_add_series(portfolio_returns_ag$returns, 
                name = "Monthly Returns", 
                color = "cornflowerblue") %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_exporting(enabled = TRUE)


