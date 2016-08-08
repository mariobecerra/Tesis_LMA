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
nombres_items <- read.csv(paste0("../out/", dataset, "/items_new_ids.csv")) %>% 
  mutate(itemId = paste0("item_", itemId))

##############################################
## Similitudes
##############################################

# archivo_similitudes <- paste0(folder_tables, "/similitudes_items_centradas.csv")
folder_similitudes_json <- paste0(folder_tables, "/json_similitudes")

archivos_similitudes <- list.files(folder_similitudes_json, full.names = T)
#archivos_similitudes <- archivos_similitudes[1:17]

similitudes <- list()
i = 0
while(i < length(archivos_similitudes)){
  i = i + 1
  archivo <- archivos_similitudes[i]
  cat("Leyendo archivo", archivo, "\n")
  file_content <- readLines(con = archivo)
  cat("Archivo leído. Convirtiendo de JSON a lista.\n")
  sims <- fromJSON(file_content)
  cat("Convertido. Agregando a lista final.\n")
  similitudes <- append(similitudes, sims)
  cat("Agregado.\n\n")
}


##############################################
## Calcular RMSE de conjunto de prueba
##############################################

# # Todas las predicciones del usuario 2:
# 
# # Items calificados por el usuario 2:
# user_2_test <- test_data %>% 
#   filter(userId == 2) %>% 
#   mutate(itemId = paste0("item_", itemId))
# 
# user_2_train <- train_data %>% 
#   filter(userId == 2) %>% 
#   mutate(item_train = paste0("item_", itemId)) %>% 
#   select(item_train,
#          rating_train = rating_cent)
# 
# 
# # Primero encuentra los n artículos más similares a los que se van a evaluar
# items_to_predict_sims <- lapply(user_2_test$itemId, function(i){
#   items = head(similitudes[[i]], 900)
#   items %>% 
#     mutate(item_to_predict = names(similitudes[i])) %>% 
#     select(item_to_predict, 
#            items_similares = itemId,
#            sim_cos) %>% 
#     return(.)
# }) %>% 
#   rbind_all()
# 
# predicciones <- items_to_predict_sims %>% 
#   left_join(user_2_train,
#             by = c("items_similares" = "item_train")) %>% 
#   filter(!is.na(rating_train)) %>% 
#   group_by(item_to_predict) %>% 
#   summarise(pred_rating = sum(sim_cos * rating_train)/sum(sim_cos)) %>% 
#   left_join(select(user_2_test, 
#                    item_to_predict = itemId,
#                    rating_cent)) %>% 
#   mutate(error = rating_cent - pred_rating)
# 
# 



predict_rating <- function(test_data, train_data, similitudes, userId_in, n = 500) {
  user_test <- test_data %>% 
    filter(userId == userId_in) %>% 
    #mutate(itemId2 = itemId) %>% 
    mutate(itemId = paste0("item_", itemId))
  
  user_train <- train_data %>% 
    filter(userId == userId_in) %>% 
    mutate(item_train = paste0("item_", itemId)) %>% 
    select(item_train,
           rating_train = rating_cent)
  
  # Primero encuentra los n artículos más similares a los que se van a evaluar
  items_to_predict_sims <- lapply(user_test$itemId, function(i){
    items = head(similitudes[[i]], n)
    if(length(items) == 0) return(NULL)
    else {items %>% 
        mutate(item_to_predict = names(similitudes[i])) %>% 
        select(item_to_predict, 
               items_similares = itemId,
               sim_cos) %>% 
        return(.)}
  }) %>% 
    rbind_all()
  
  predicciones <- items_to_predict_sims %>% 
    left_join(user_train,
              by = c("items_similares" = "item_train")) %>% 
    filter(!is.na(rating_train)) %>% 
    group_by(item_to_predict) %>% 
    summarise(pred_rating = sum(sim_cos * rating_train)/sum(sim_cos)) %>% 
    full_join(select(user_test, 
                     item_to_predict = itemId,
                     rating_cent)) %>% 
    # Si no hubo items para calcular similitud y por ende una predicción, entonces la predicción es 0, o sea, el promedio del usuario
    mutate(pred_rating = ifelse(is.na(pred_rating), 0, pred_rating)) %>% 
    mutate(error = rating_cent - pred_rating)
  
  return(predicciones)
}

i <- 1
cant_usuarios <- length(unique(test_data$userId))
predicciones_test <- lapply(unique(test_data$userId), function(usuario){
  cat("Usuario:", usuario, "\n")
  cat("Iteración", i, "de", cant_usuarios, "\n\n")
  i <<- i + 1
  preds <- predict_rating(test_data, train_data, similitudes, usuario, 600)
  preds$userId = usuario
  return(preds)
}) %>% 
  rbind_all()

mean(predicciones_test$error^2) %>% sqrt
# 0.8304887333578501174

write.table(predicciones_test,
            file = paste0(folder_tables, "/predicciones_modelo_vecindario.csv"),
            sep = ",",
            row.names = F,
            col.names = T)

##############################################
## Algunas recomendaciones
##############################################

# Predicciones del conjunto de prueba del usuario 2
predict_rating(test_data, train_data, similitudes, 2, 1000)
