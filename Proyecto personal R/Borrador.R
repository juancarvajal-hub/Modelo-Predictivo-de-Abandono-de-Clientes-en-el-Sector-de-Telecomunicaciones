#--------------------------------- Paquetes a usar ----------------------------#
require(magrittr)
require(tidyverse)
require(ggplot2)
require(psych)
require(xtable)
library(caret)
library(pROC)
library(randomForest)
library(xgboost)

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


# Curva roc versión mejorada 

# Extraer los datos de la curva ROC
roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)

# Valor del AUC
auc_value <- round(auc(roc_obj), 3)

# Crear una columna para el valor de la línea diagonal
roc_df$random_line <- roc_df$FPR

# Gráfico
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  # sombreado entre la curva y la línea diagonal
  geom_ribbon(aes(ymin = random_line, ymax = TPR),
              fill = "#0072B2", alpha = 0.25) +
  # línea ROC
  geom_line(color = "#0072B2", linewidth = 1.2) +
  # línea diagonal de referencia
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray60") +
  labs(
    title = "Curva ROC - Modelo de Regresión Logística",
    subtitle = paste("Área bajo la curva (AUC):", auc_value),
    x = "1 - Especificidad (False Positive Rate)",
    y = "Sensibilidad (True Positive Rate)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray30")
  )


#---------------------------- Modelo de Random forest -------------------------#
################################################################################

set.seed(0101)
modelo_rf <- randomForest(
  Churn ~ ., 
  data = train,
  ntree = 1500,
  mtry = 4,
  importance = TRUE ,    # permite calcular importancia de variables
)

saveRDS(modelo_rf,"modelo_rf.rds")


# Predicciones de clase (Sí / No)
pred_rf <- predict(modelo_rf, newdata = test, type = "response")

# Predicciones de probabilidad (para ROC y AUC)
pred_probs_rf <- predict(modelo_rf, newdata = test, type = "prob")[, 2]

conf_matrix_rf <- confusionMatrix(pred_rf, test$Churn, positive = "1")
saveRDS(conf_matrix_rf,"conf_matrix_rf.rds")
conf_matrix_rf


# Importancia de las variables 

varImpPlot(modelo_rf,
           main = "Importancia de las Variables - Random Forest",
           col = "#1B9E77")

# Modelo tuneado 
set.seed(212121)

tune_grid <- expand.grid(
  mtry = c(2, 3, 4, 5, 6)
)

control <- trainControl(method = "cv", number = 5)

modelo_tuneado <- train(
  Churn ~ .,
  data = train,
  method = "rf",
  trControl = control,
  tuneGrid = tune_grid,
  ntree = 500
)

modelo_tuneado

# modelo reducido

modelo_rf_reducido <- randomForest(
  Churn ~ TotalCharges + MonthlyCharges + tenure +
    Contract + InternetService + PaymentMethod +
    PaperlessBilling + TechSupport + OnlineSecurity+
    PaperlessBilling+PaymentMethod+OnlineBackup+StreamingTV,
  data = train,
  ntree = 1000,
  mtry = modelo_tuneado$bestTune$mtry,
  importance = TRUE
)


pred_rf_reducido <- predict(modelo_rf_reducido, newdata = test)

conf_matrix_rf_reducido <- confusionMatrix(pred_rf_reducido, test$Churn, positive = "1")
conf_matrix_rf_reducido$table

saveRDS(modelo_rf_reducido,"modelo_rf_reducido.rds")
saveRDS(conf_matrix_rf_reducido,"conf_matrix_rf_reducido.rds")

cat("La precision con la que el modelo acierta es de",conf_matrix_rf$overall["Accuracy"])
cat("La precision con la que el modelo acierta es de",conf_matrix_rf_reducido$overall["Accuracy"])


# Probabilidades del modelo (ajusta el nombre del objeto a tu modelo)
pred_probs <- predict(modelo_rf_reducido, newdata = test, type = "prob")[,2]

# Crear la curva ROC
roc_obj <- roc(test$Churn, pred_probs)

# Convertir la curva ROC a data frame
roc_df <- data.frame(
  fpr = 1 - roc_obj$specificities,
  tpr = roc_obj$sensitivities
)

# Calcular AUC
auc_value <- auc(roc_obj)

# Crear un data frame con la línea diagonal
diagonal <- data.frame(fpr = seq(0, 1, length.out = 100))
diagonal$tpr <- diagonal$fpr

# Crear gráfico profesional con el área entre la curva y la línea diagonal
g <- ggplot() +
  # Rellenar el área entre la curva ROC y la diagonal
  geom_ribbon(
    data = roc_df,
    aes(x = fpr, ymin = fpr, ymax = tpr),
    fill = "#56B4E9",
    alpha = 0.3
  ) +
  # Dibujar la curva ROC
  geom_line(
    data = roc_df,
    aes(x = fpr, y = tpr),
    color = "#0072B2",
    size = 1.2
  ) +
  # Dibujar la línea diagonal (modelo aleatorio)
  geom_abline(linetype = "dashed", color = "gray50", linewidth = 0.8) +
  # Títulos y estilo
  labs(
    title = "Curva ROC - Random Forest",
    subtitle = paste("Área bajo la curva (AUC):", round(auc_value, 4)),
    x = "Tasa de Falsos Positivos (1 - Especificidad)",
    y = "Tasa de Verdaderos Positivos (Sensibilidad)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12),
    panel.grid.minor = element_blank()
  )

saveRDS(g,"g.rds")


# Modelo xgboost

# Crear matrices de entrenamiento y prueba
train_matrix <- model.matrix(Churn ~ . - 1, data = train)  # sin intercepto
test_matrix  <- model.matrix(Churn ~ . - 1, data = test)

# Variables objetivo (convertidas a numéricas: 1 = Sí, 0 = No)
train_label <- as.numeric(train$Churn) - 1
test_label  <- as.numeric(test$Churn) - 1

set.seed(123)

control <- trainControl(
  method = "cv",                # validación cruzada
  number = 5,                   # 5 particiones
  verboseIter = FALSE,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,  # para usar AUC
  savePredictions = TRUE
)


grid <- expand.grid(
  nrounds = c(100, 200),     # número de árboles
  max_depth = c(3, 5),         # profundidad
  eta = c(0.01, 0.05),       # learning rate
  gamma = c(0, 0.1),         # penalización de división
  colsample_bytree = c(0.8), # columnas por árbol
  min_child_weight = c(1, 3),  # tamaño mínimo del nodo
  subsample = c(0.8)           # proporción de datos usados por árbol
)

set.seed(181818)

modelo_xgb_tuned <- train(
  x = train_matrix,
  y = factor(ifelse(train_label == 1, "Yes", "No")),  # etiquetas tipo factor
  method = "xgbTree",
  trControl = control,
  tuneGrid = grid,
  metric = "ROC"  # optimizamos según el AUC
)

saveRDS(modelo_xgb_tuned,"modelo_xgb_tuned.rds")
modelo_xgb_tuned$bestTune

plot(modelo_xgb_tuned)

# Predicciones con el mejor modelo
pred_probs_xg <- predict(modelo_xgb_tuned, newdata = test_matrix, type = "prob")[, "Yes"]
pred_class_xg <- predict(modelo_xgb_tuned, newdata = test_matrix)

# Matriz de confusión
conf_matrix_xg <- confusionMatrix(pred_class_xg, factor(ifelse(test_label == 1, "Yes", "No")), positive = "Yes")
conf_matrix_xg

saveRDS(conf_matrix_xg,"conf_matrix_xg.rds")

conf_matrix_xg$overall["Accuracy"]



# Obtener probabilidades del modelo XGBoost
pred_probs <- predict(modelo_xgb_tuned, newdata = test_matrix, type = "prob")[, 2]

# Crear objeto ROC
roc_obj <- roc(test$Churn, pred_probs)

# Convertir los datos de la curva ROC a un data.frame
roc_data <- data.frame(
  TPR = rev(roc_obj$sensitivities),     # Sensibilidad
  FPR = rev(1 - roc_obj$specificities)  # 1 - Especificidad
)

# Calcular AUC
auc_value <- auc(roc_obj)

# Crear data.frame para el área entre la curva y la diagonal
area_data <- roc_data %>%
  mutate(base = FPR) %>%
  filter(TPR > base)  # Sombreamos solo el área sobre la línea base

# Graficar
g1 <- ggplot(roc_data, aes(x = FPR, y = TPR)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +  # Línea base
  geom_ribbon(data = area_data, aes(ymin = base, ymax = TPR), fill = "#1F77B4", alpha = 0.3) + # Área entre curva y base
  geom_line(color = "#1F77B4", size = 1.2) +  # Curva ROC
  labs(
    title = paste("Curva ROC - Modelo XGBoost (AUC =", round(auc_value, 3), ")"),
    x = "Tasa de Falsos Positivos (1 - Especificidad)",
    y = "Tasa de Verdaderos Positivos (Sensibilidad)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )

saveRDS(g1,"g1.rds")
