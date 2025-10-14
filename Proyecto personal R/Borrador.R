#--------------------------------- Paquetes a usar ----------------------------#
require(magrittr)
require(tidyverse)
require(ggplot2)
require(psych)
require(xtable)
library(caret)
library(pROC)

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

for (i in names(select(datos,-c("Churn","TotalCharges","MonthlyCharges","tenure")))){
  print(
  datos %>%  ggplot(aes(x=.data[[i]],fill = Churn))+ 
    geom_bar(position = "dodge")+
    labs(title = paste("Distribición de",i,"según Churn"),
         x=i,y="Numero de clientes")+
    scale_fill_brewer(palette = "Set2") +
    geom_text(stat = "count", aes(label = ..count..),
              position = position_dodge(width = 0.9), 
              vjust = -0.3, size = 3) +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  # centrado
    )
  )
}

c("PaymentMethod","PaperlessBilling","Contract","StreamingMovies","StreamingTV","TechSupport",
  "DeviceProtection")
datos %>% select(where(is.factor)) %>% names()
datos %>% names()


# Mas graficas descriptivas esta vez tomando las variables numericas 

for (i in c("tenure","MonthlyCharges","TotalCharges")){
  if (i == "TotalCharges"){
    print(
      datos %>%
        mutate(intervalo = cut(.data[[i]]/10, breaks = 10)) %>%        # Divide tenure en 10 intervalos
        group_by(intervalo, Churn) %>%
        summarise(Frecuencia = n()) %>%
        ggplot(aes(x = intervalo, y = Frecuencia, fill = Churn)) +
        geom_col(position = "dodge", color = "white") +          # Barras separadas
        geom_text(aes(label = Frecuencia),
                  position = position_dodge(width = 0.9),
                  vjust = -0.3, size = 3.5) +                    # Texto encima de barras
        scale_fill_brewer(palette = "Set2") +
        labs(title = paste("Distribución de la Variable",i,"por Churn"),
             x = paste(i,"(en intervalos)"),
             y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
    )
  }else{
    print(
      datos %>%
        mutate(intervalo = cut(.data[[i]], breaks = 10)) %>%        # Divide tenure en 10 intervalos
        group_by(intervalo, Churn) %>%
        summarise(Frecuencia = n()) %>%
        ggplot(aes(x = intervalo, y = Frecuencia, fill = Churn)) +
        geom_col(position = "dodge", color = "white") +          # Barras separadas
        geom_text(aes(label = Frecuencia),
                  position = position_dodge(width = 0.9),
                  vjust = -0.3, size = 3.5) +                    # Texto encima de barras
        scale_fill_brewer(palette = "Set2") +
        labs(title = paste("Distribución de la Variable",i,"por Churn"),
             x = paste(i,"en intervalos"),
             y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
      
    )
  }  
}

#    gráficos de densidad 

datos %>%
  ggplot(aes(x = MonthlyCharges, fill = Churn)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribución de Tenure por Estado de Churn",
       x = paste(""),
       y = "Densidad") +
  xlim(0,120)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))


# ---------------Modelos de clasificación para la predicción ------------------#
################################################################################

# -------------------------- Regresión logística ------------------------------#

datos %<>% na.omit()
# Dividir en conjunto de entrenamiento y prueba
set.seed(212001)
library(caret)

trainIndex <- createDataPartition(datos$Churn, p = 0.8, list = FALSE)
train <- datos[trainIndex, ]
test <- datos[-trainIndex, ]
train %>% dim()
test %>% dim()
                 

## Ajustar el  Modelo de regresión logística 

modelo_log <- glm(Churn ~.,
                  data = train,
                  family = binomial)
summary(modelo_log)

# Reducción de variables 

modelo_step <- step(modelo_log, direction = "both", trace = FALSE)
saveRDS(modelo_step,"modelo_step.rds")

summary(modelo_step)$

# El modelo mejora considerablemente el numero de variables significativas del modelo 

# Predicciones en probabilidad
pred_probs <- predict(modelo_step, newdata = test, type = "response")

# Convertir a clases
pred_class <- ifelse(pred_probs > 0.5, "1", "0")
pred_class <- as.factor(pred_class)


# Evaluación del modelo.

# Matriz de confusión
conf_matrix <- confusionMatrix(pred_class, test$Churn, positive = "1")
conf_matrix

roc_obj <- roc(test$Churn, pred_probs)
plot(roc_obj, col = "blue", main = "Curva ROC - Regresión Logística")
auc(roc_obj)
