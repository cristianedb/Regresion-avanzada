################################################################################
#                                                                              #
#                   TRABAJO PRACTICO - REGRESION AVANZADA                      #
#                                                                              #
################################################################################

    options(device = "windows")


#limpio la memoria
rm( list=ls() )
gc()
options(scipen = 999) ### turn off scientific notation

library(readxl)
library(tidyverse)
library(gplots)
library(carData)
library(car)
library(QuantPsyc)
library(sqldf)
library(psych)
library(PerformanceAnalytics)
library(olsrr)
library(MASS)
library(lmtest) #dwtest
library(stargazer)

setwd("C:/Users/u500246/OneDrive - Telecom Argentina SA/_maestria/10- Regresion avanzada/Practica/TP")
cars <- read_csv("CarPrice.csv")

#selecciono solo las variables numéricas
idx <- which(sapply(cars, class) %in% c("numeric","integer"))

#creo un subconjunto solo con las variables numericas
n_cars <- subset(cars, select = idx )

################################################################################
# 1 -¿Cuál podría ser a su criterio la variable dependiente para pensar en un 
# modelo de regresión lineal múltiple?
#
# La variable dependiente es el precio, justamente porque el resto de las variables 
# son caracteristicas de los autos (descriptoras) y el precio depende de todas ellas
# (variable dependiente)

################################################################################
# 2 -Generen información exploratoria de la variable dependiente elegida, y 
# algunas relaciones con al menos tres columnas.

summary(cars$price)

library(ggplot2)
library(dplyr)

# Histograma de precios
cars %>%ggplot( aes(x=price)) + geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

# Muestra la relacion de todas las variables para elegir las variables con las
# que mas se relacionan
chart.Correlation(n_cars, histogram = TRUE, lm = TRUE, method = "spearman")

# Las variables que mas se relacionan con el precio son: citympg,horsepower,
# curbweight,carwidth,carlength
chart.Correlation(n_cars[,c('citympg','horsepower','curbweight','carwidth','carlength','price')], histogram = TRUE, lm = TRUE, method = "spearman")


################################################################################
# 3 - Prueben y generen al menos 3 modelos de regresión lineal múltiple con la 
# variable dependiente elegida. (con variables cuantitativas y cualitativas).

modelo_m0 <- lm(price ~ ., data = n_cars )
summary(modelo_m0)
# Residual standard error: 3043 on 189 degrees of freedom
# Multiple R-squared:  0.8656,	Adjusted R-squared:  0.8549 
# F-statistic: 81.15 on 15 and 189 DF,  p-value: < 0.00000000000000022
n_cars1<-n_cars
n_cars1$car_ID<-NULL


modelo_m1 <- lm(price ~ ., data = n_cars )
summary(modelo_m1)


modelo_m2 <- lm(price ~ citympg + horsepower+curbweight+carwidth+carlength+price , data = cars )

modelo_m1 <- lm(price ~ citympg + horsepower , data = cars )
summary(modelo_m1)
 
