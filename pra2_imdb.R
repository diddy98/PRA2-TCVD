# TIPOLOGÍA Y CICLO DE VIDA DE LOS DATOS: PRÁCTICA 2

# PREPROCESADO Y LIMPIEZA

# Carga de librerías

librerias <- c('tidyverse', 'ggplot2', 'dplyr', 'corrplot', 'cowplot', 'gridExtra')

lapply(librerias, function(package) {if (!require(package, character.only = TRUE)) {install.packages(package, dependencies = TRUE); library(package, character.only = TRUE)}})

# Carga del conjunto de datos

data <- read.csv("imdb_top_1000.csv")

# Estructura del dataset

str(data)

# Primeras 10 líneas del dataset

head(data)

# Adición de una columna con un identificador único para cada película

data <- data %>% mutate(ID=row_number())

# Eliminación de las columnas innecesarias

data <- data %>% select(-c('Poster_Link', 'Overview'))

# Renombrar variable "Series_Title" a "Movies_Title"

data <- data %>% rename(Movies_Title = Series_Title)

# Eliminar "min" de la columna "Runtime"

data$Runtime <- gsub("min", "", data$Runtime)

# Reducción de la columna "Genre" a un género de película (el primer valor antes de la coma (",")) 

data$Genre <- sapply(strsplit(data$Genre, ","), function(x) trimws(x[1]))

# Transformación de las variables Released_year, Runtime, Meta_score, IMDB Rating y Gross a numéricas

data <- data %>% mutate(across(c(Released_Year, Runtime, Meta_score, IMDB_Rating), as.numeric))

# Para la variable Gross, al delimitar los miles con coma, se deben sustituir y posteriormente hacer la transformación

data <- data %>% mutate(Gross = as.numeric(gsub(",", "", Gross)))

# Discretización de variables IMDB_Rating y Metascore

puntos_corte <- c(7.5, 8, 8.5, 10)

etiquetas <- c("7.5-8", "8-8.5", "8.5-10")

data$IMDB_Rating_mod <- cut(data$IMDB_Rating, breaks = puntos_corte, labels = etiquetas, include.lowest = TRUE)

data$Meta_score_mod <- cut(data$Meta_score, breaks = 5, labels = c("F", "C", "B", "B+", "A"))

# Visualizar datos NA

colSums(is.na(data))

# Visualizar valor nulo en Released_Year

data[is.na(data$Released_Year),]

# Imputación del valor 1995 en la película Apollo 13

data[data$ID==967, 'Released_Year'] <- 1995

# Valores nulos para Meta\_score y Gross

# Inicialmente se observa si muestran correlación con el resto de variables numéricas dentro del modelo

corr_matrix  <-  cor(data[, unlist(lapply(data, is.numeric))], use = 'complete.obs')

corrplot(corr_matrix, method='number')

# Si se observa el número de valores únicos dentro de cada columna del dataset, en cuanto a variables categóricas como director, el número es elevado

sapply(data, function(x) n_distinct(x))

# Ajuste de la mediana y de la media

hist(data$Meta_score, probability = FALSE)
abline(v = mean(data$Meta_score, na.rm=TRUE), col='lightblue', lwd = 3)
abline(v = median(data$Meta_score, na.rm=TRUE), col='blue', lwd = 3)

hist(data$Gross, probability = FALSE)
abline(v = mean(data$Gross, na.rm=TRUE), col='lightblue', lwd = 3)
abline(v = median(data$Gross, na.rm=TRUE), col='blue', lwd = 3)

# Imputación en las variables 'Gross' y 'Meta_score' (y, por consiguiente, en Meta_score_mod)

data[is.na(data$Meta_score), 'Meta_score'] <- median(data$Meta_score, na.rm=TRUE)
data[is.na(data$Gross), 'Gross'] <- median(data$Gross, na.rm=TRUE)

# Al ser el valor mediano de Meta_score 79, esto les otorgaría una puntuación de 'B+'

data[is.na(data$Meta_score_mod), 'Meta_score_mod'] <- 'B+'

# Si se vuelve a comprobar la presencia de valores nulos, no se encuentra ninguno

colSums(is.na(data))

# Presencia de valores en blanco en Certificate

data["Certificate"][data["Certificate"] == '']

# Sustitución de valores en blanco por NA

data["Certificate"][data["Certificate"] == ''] <- NA 

# Imputación del valor 'Not rated' para aquellas filas con valor nulo en Certificate

data[is.na(data$Certificate), 'Certificate'] <- 'Not rated'

# ANÁLISIS DESCRIPTIVO GENERAL

# Variables numéricas

num_vars <- data %>% select_if(is.numeric) %>% select(-ID)

summary(num_vars)

apply(num_vars,2,sd)

df_num <- tidyr::gather(num_vars, key = "variable", value = "value")

boxplot_lista <- lapply(unique(df_num$variable), function(var) {
  ggplot(df_num %>% filter(variable == var), aes(x = variable, y = value)) +
    geom_boxplot() +
    labs(title = "", x="")
})

boxplot_combinado <- cowplot::plot_grid(plotlist = boxplot_lista, nrow = 1)

print(boxplot_combinado)

# Variables categóricas

cat_vars <- data %>% select(Certificate, Genre, IMDB_Rating_mod, Meta_score_mod)

categorical_vars <- names(cat_vars)

barplot_lista <- list()

for (var in categorical_vars) {
  plot <- ggplot(cat_vars, aes(x = .data[[var]])) +
    geom_bar() +
    labs(title = paste("Bar Plot for", var), x = var, y = "Count")
  
  barplot_lista[[var]] <- plot
}

grid <- do.call(grid.arrange, c(barplot_lista, ncol = 2))

print(grid)

# Variables categóricas 2

cat_vars1 <- data %>% select(Director, Star1, Star2, Star3, Star4)

categorical_vars1 <- names(cat_vars1)

barplot_lista1 <- list()

for (var in categorical_vars1) {
  plot <- ggplot(cat_vars1, aes(x = .data[[var]])) +
    geom_bar() +
    labs(title = paste("Bar Plot for", var), x = var, y = "Count")
  
  barplot_lista1[[var]] <- plot
}

grid1 <- do.call(grid.arrange, c(barplot_lista1, ncol = 2))

print(grid1)

# Relaciones básicas

ggplot(data, aes(x = Meta_score, y = Gross)) +
  geom_point() +
  labs(title = "Relación entre Meta_score y Recaudación", x = "Meta_score", y = "Recaudación")

ggplot(data, aes(x = IMDB_Rating, y = Gross)) +
  geom_point() +
  labs(title = "Relación entre IMDB_Rating y Recaudación", x = "IMDB", y = "Recaudación")

ggplot(data, aes(x = IMDB_Rating, y = Runtime)) +
  geom_point() +
  labs(title = "Relación entre IMDB_Rating y Recaudación", x = "IMDB", y = "Recaudación")

ggplot(data, aes(x = Meta_score, y = Runtime)) +
  geom_point() +
  labs(title = "Relación entre IMDB_Rating y Recaudación", x = "IMDB", y = "Recaudación")

# ANÁLISIS

# Crea un loop para generar un diagrama de dispersión por cada variable numérica
for (variable in names(data)[variables_numericas]) {
  # Crea el diagrama de dispersión
  plot(data[[variable]], data$IMDB_Rating, 
       main = paste("Diagrama de dispersión:", variable),
       xlab = variable, ylab = "Calificación IMDB")
}

# Crea un loop para realizar las pruebas para cada variable
for (variable in names(data)) {
  # Verifica si la variable es numérica
  if (is.numeric(data[[variable]])) {
    # Realiza la prueba de normalidad (Shapiro-Wilk) para la variable actual
    normality_test <- shapiro.test(data[[variable]])
    
    # Realiza la prueba de homogeneidad de varianzas (Levene) entre la variable actual y IMDB_Rating_Categoria
    homogeneity_test <- leveneTest(data[[variable]], data$IMDB_Rating_Categoria)
    
    # Imprime los resultados de las pruebas para la variable actual
    cat("Variable:", variable, "\n")
    cat("Prueba de normalidad (Shapiro-Wilk):\n")
    cat("Estadístico:", normality_test$statistic, "\n")
    cat("Valor p:", normality_test$p.value, "\n\n")
    cat("Prueba de homogeneidad de varianzas (Levene):\n")
    cat("Estadístico:", homogeneity_test$statistic, "\n")
    cat("Valor p:", homogeneity_test$p.value, "\n\n")
  }
}

# Correlación

cor.test(data$Runtime, data$IMDB_Rating, method="spearman")
cor.test(data$No_of_Votes, data$IMDB_Rating, method="spearman")
cor.test(data$Gross, data$IMDB_Rating, method="spearman")
cor.test(data$Released_Year, data$IMDB_Rating, method="spearman")

# Seleccionar todas las variables excepto "Series_Title"
selected_vars <- data %>% select(-Series_Title,-IMDB_Rating, -IMDB_Rating_Categoria )

# Realizar la prueba de Kruskal-Wallis para cada variable
kruskal_results <- lapply(selected_vars, function(var) {
  kruskal.test(var ~ IMDB_Rating_Categoria, data = data)
})

# Imprimir los resultados
for (i in seq_along(kruskal_results)) {
  variable <- names(selected_vars)[i]
  result <- kruskal_results[[i]]
  
  cat("Variable:", variable, "\n")
  print(result)
  cat("-----------------------------------\n")
}


# Realizar una regresión lineal
model <- lm(IMDB_Rating ~ Runtime + Gross + Released_Year + No_of_Votes + as.factor(Genre), data = data)

# Obtener los resultados de la regresión
summary(model)

