# Se deben de tener ya precalculadas las similitudes entre items
library(jsonlite)
library(dplyr)

##############################################
## Determinar el dataset con el que se va a trabajar
##############################################

# Si el script se ejecuta en forma interactiva (desde algún IDE), entonces toma el dataset de movielens. Si se ejecuta automáticamente desde otro script o desde la terminal, debe llevar como argumento cuál dataset se quiere analizar

if(!interactive()){ 
  args <- commandArgs(TRUE)
  dataset <- args[1]
  time <- args[2]
  if(length(args) == 0) {
    cat("No especificaste dataset ni carpeta de salida\n\n")
    quit(save = "no", status = 0, runLast = FALSE)
  } 
} else {
  dataset <- "MovieLens"
  time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
}

#folder <- paste0("../out/", dataset, "/", time)
folder <- "../out/MovieLens/2016-08-04_13_05_41_similitudes_modelo_vecindario"
folder_plots <- paste0(folder, "/plots")
folder_models <- paste0(folder, "/models")
folder_tables <- paste0(folder, "/tables")

system(paste("mkdir", folder))
system(paste("mkdir", folder_plots))
system(paste("mkdir", folder_models))
system(paste("mkdir", folder_tables))

file_name_train <- paste0("../out/", dataset, "/train.rds")
file_name_test <- paste0("../out/", dataset, "/test.rds")

cat("Leyendo archivos de calificaciones\n")

train_data <- readRDS(file_name_train)
test_data <- readRDS(file_name_test)

##############################################
## Similitudes
##############################################

# archivo_similitudes <- paste0(folder_tables, "/similitudes_items_centradas.csv")
folder_similitudes_json <- paste0(folder_tables, "/json_similitudes")

archivos_similitudes <- list.files(folder_similitudes_json, full.names = T)
archivos_similitudes <- archivos_similitudes[1:3]

similitudes <- list()
i = 0
while(i < length(archivos_similitudes)){
  i = i + 1
  sims <- fromJSON(readLines(archivos_similitudes[i]))
  similitudes <- append(similitudes, sims)
}

##############################################
## Algunas recomendaciones
##############################################

# Para el usuario 2, predicción de la calificación del item 3:

# Items calificados por el usuario 2:
user_3 <- train_data %>% filter(userId == 2)

# Items similares al item 3:
similitudes[[3]] %>% head

# Filtrar los items parecidos a 3 que el usuario 2 haya visto y unir los datos del usuario:
df_3 <- similitudes[[3]] %>% 
  mutate(itemId = as.integer(gsub("[^0-9]+", "", itemId))) %>% 
  filter(itemId %in% user_3$itemId) %>% 
  left_join(user_3) 

# Predicción
df_3 %>% 
  summarise(pred_rat = sum(sim_cos * rating_cent)/sum(sim_cos))
# La calificación predicha es 0.3131239, mientras que la calificación real centrada es 0.4774406



