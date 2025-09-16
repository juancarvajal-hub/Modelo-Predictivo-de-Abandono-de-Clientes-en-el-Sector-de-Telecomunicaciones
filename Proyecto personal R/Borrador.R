#--------------------------------- Paquetes a usar-----------------------------#
require(magrittr)
require(tidyverse)
require(ggplot2)
require(psych)

datos <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", header = TRUE, sep = ",")
 

cat("la base de datos cuenta con",dim(datos),"filas y columnas respectivamente")

