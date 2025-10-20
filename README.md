# 🧠 Modelo Predictivo para Abandono de Clientes (Churn)

## 📌 Descripción del Proyecto
Este es un **proyecto personal** desarrollado con **R**, cuyo objetivo es construir y evaluar distintos modelos predictivos para estimar la probabilidad de que un cliente abandone un servicio (*churn*).  

El análisis se realizó utilizando un conjunto de datos del sector de **telecomunicaciones**, que incluye información sobre el perfil del cliente, los servicios contratados y los costos asociados.  

---

## 📋 Flujo del Proyecto

1. **Carga de datos y librerías**
   - Importación del dataset en formato `.csv`.
   - Carga de las librerías necesarias: `tidyverse`, `caret`, `pROC`, `randomForest`, `xgboost`, entre otras.

2. **Exploración y limpieza de datos**
   - Análisis de valores faltantes y tipos de variables.  
   - Codificación de variables categóricas.  
   - Transformación de columnas numéricas y verificación de outliers.

3. **Análisis descriptivo**
   - Gráficos de barras, histogramas y densidades.
   - Distribución de variables como `tenure`, `MonthlyCharges` y `TotalCharges`.
   - Visualización de la variable objetivo `Churn`.

4. **Preparación de datos**
   - División en conjuntos **train (70%)** y **test (30%)**.  
   - Creación de un *pipeline (recipe)* para el preprocesamiento.  

5. **Modelado**
   - Modelos implementados:
     - **Regresión Logística**
     - **Random Forest**
     - **XGBoost**
   - Cada modelo fue entrenado con el conjunto de entrenamiento y evaluado con el conjunto de prueba.

6. **Evaluación**
   - Métricas utilizadas:
     - AUC (Área bajo la curva ROC)
     - Precisión
     - Recall (Sensibilidad)
     - Matriz de confusión
   - Se generaron gráficos ROC personalizados con `ggplot2` para cada modelo.

7. **Selección del modelo**
   - El modelo **XGBoost** obtuvo el mejor desempeño general, aunque la diferencia con los demás fue mínima.
   - Todos los modelos demostraron un poder predictivo similar.

---

## 💾 Almacenamiento de Modelos

Cada modelo entrenado fue **guardado en formato `.rds`** mediante la función `saveRDS()`.  
Esto permite cargarlos fácilmente en otros scripts o documentos reproducibles, como el archivo `.qmd` (Quarto), utilizando `readRDS()`.  

📂 Ejemplo:
```r
# Guardar el modelo
saveRDS(modelo_xgb_tuned, "modelo_xgb_tuned.rds")

# Cargar el modelo en un archivo QMD
modelo_xgb_tuned <- readRDS("modelo_xgb_tuned.rds")

