#--------------------------------- Paquetes a usar ----------------------------#
require(magrittr)
require(tidyverse)
require(ggplot2)
require(psych)

rm(list = ls())
gc()

### ---------------------- Lectura de la base de datos ----------------------###

datos <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", header = TRUE, sep = ",")
cat("la base de datos cuenta con",dim(datos),"filas y columnas respectivamente")

# Contar valores faltantes por columnas del dataframe 

resumen1 <- data.frame(variables = names(datos),
completos = apply(!is.na(datos),2,sum)) %>% 
  mutate(porcentaje_completo = round(completos/nrow(datos)*100,2))
rownames(resumen1) <- NULL
resumen1 <- resumen1[c("variables","porcentaje_completo")]
resumen1


# 🔹 Gráfica de % de completitud por variable
ggplot(resumen1, aes(x = reorder(variables, porcentaje_completo), 
                    y = porcentaje_completo)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Completitud de la base de datos",
       x = "Variable",
       y = "% de valores completos") +
  theme_minimal()

cat("Base de datos bastante completa se evidencia poca presencia de valores faltantes" )

## ---------------- Análisis exploratorio de la información ------------------##





