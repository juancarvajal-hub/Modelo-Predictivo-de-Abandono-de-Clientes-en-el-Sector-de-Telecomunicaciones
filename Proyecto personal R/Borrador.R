#--------------------------------- Paquetes a usar ----------------------------#
require(magrittr)
require(tidyverse)
require(ggplot2)
require(psych)
require(xtable)

rm(list = ls())
gc()

################################################################################
### ---------------------- Lectura de la base de datos ----------------------###
################################################################################

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


################################################################################
## ---------------- Análisis exploratorio de la información ------------------##
################################################################################

# Sabemos que tenemos 21 variables en nuestra base de datos , lo que sigue es analizar si
# hay variables que presenten problemas como registros mal tomados o revisemos como el R esta 
# interpretando que tipo de variable es cada una de las columnas de la base de datos

datos %>% str()
datos <- datos %>% select(-customerID)

# Revisemos los valores únicos de las variables que son tipo texto o factor

for (i in names(datos)) { 
  if (class(datos[[i]]) %in% c("character", "factor")) {
    cat("\n", i, "tipo:",class(datos[[i]]),"→ Valores únicos:\n")
    print(unique(datos[[i]]))
  }
}

# Resumen de las variables numéricas

datos %>%
  select(where(is.numeric)) %>%
  str()

# Hasta este punto notamos que hay variables que requieren reemplazar valores y ajustar 
# el tipo de datos al que pertenecen eso es lo siguiente que vamos a modificar sabiendo que 
# la mayoría de variables deben ser de tipo factor.

# convertir cada variable de tipo character a tipo factor 
# Gender
datos$SeniorCitizen <- as.character(datos$SeniorCitizen)
datos$gender <- recode(datos$gender,"Female"="1",
                       "Male"="0")
# MultipleLines
datos$MultipleLines <- recode(datos$MultipleLines,"No phone service"="No") 
# OnlineSecurity
datos$OnlineSecurity <- recode(datos$OnlineSecurity,"No internet service"="No")
# OnlineBackup
datos$OnlineBackup <- recode(datos$OnlineBackup,"No internet service"="No")
# DeviceProtection
datos$DeviceProtection <- recode(datos$DeviceProtection,"No internet service"="No")
# TechSupport
datos$TechSupport <- recode(datos$TechSupport,"No internet service"="No")
# StreamingTV
datos$StreamingTV <- recode(datos$StreamingTV,"No internet service"="No")
# StreamingMovies
datos$StreamingMovies <- recode(datos$StreamingMovies,"No internet service"="No")


# Convertir en factor todas las variables de tipo character 
datos %<>% mutate(across(where(is.character),as.factor))
datos %<>% mutate(across(where(~is.factor(.) && all(levels(.) %in% c("Yes","No"))),
                        ~ ifelse(.=="Yes","1","0")))
datos %<>% mutate(across(where(is.character),as.factor))

datos %>% str()
# Mostrar el resultado en una tabla 

data.frame(variables = c(names(datos)),
           tipo = sapply(datos,class))


################################-------------###################################
###---------------------- Primeras gráficas descriptivas --------------------### 
################################-------------###################################





                 