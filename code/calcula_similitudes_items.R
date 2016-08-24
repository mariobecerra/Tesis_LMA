library(dplyr)
library(Matrix)
library(jsonlite)

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
  dataset <- "BookCrossing"
  time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
}

folder <- paste0("../out/", dataset, "/", time)
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

##############################################
## Calcula similitudes
##############################################

cosine_sim_items <- function(df_ratings, archivo_salida){
  
  # Calcula la norma de las columnas de la matriz de ratings
  # Es más rápido con dplyr por la ralidad
  cat("Calculando normas\n")
  normas_items <- df_ratings %>% 
    group_by(itemId) %>% 
    summarise(normas = sqrt(sum(rating * rating))) %>% 
    right_join(data.frame(itemId = 1:max(df_ratings$itemId))) %>% 
    mutate(normas = ifelse(is.na(normas), 0, normas)) %>% 
    .$normas
  
  # Crear matriz de calificaciones rala
  R <- sparseMatrix(
    i = df_ratings$userId,
    j = df_ratings$itemId,
    x = df_ratings$rating
  ) 
  
  num_items <- ncol(R)
  cat("Normas calculadas\n\n")
  # Calcula la similitud coseno entre cada par de columnas de la matriz R
  for(i in 1:num_items){
    cat("Item", i, "de", num_items, "\n")
    x = R[,i] # i-ésima columna de R
    norm_x <- sqrt(t(x) %*% x)
    # La siguiente división puede resultar en NaNs cuando en la columna hay puros ceros. 
    # Esto sucede en items que no tienen ninguna calificación.
    # Al final tiene sentido remmplazar los NaNs por ceros, así no van a ser tomados en cuenta para el vecindario
    x = x/norm_x
    res <- as.numeric(x %*% R) / normas_items
    cat(res, file = archivo_salida, sep = ",", append = T)
    cat("\n", file = archivo_salida, sep = "", append = T)
  }
}

# Sin centrar
# archivo_salida <- paste0(folder_tables, "/similitudes_items.csv")
# cosine_sim_items(select(train_data, userId, itemId, rating), archivo_salida)

# Centrando por usuario
archivo_salida_center <- paste0(folder_tables, "/similitudes_items_centradas.csv")
cosine_sim_items(select(train_data, userId, itemId, rating = rating_cent), archivo_salida_center)

##############################################
## Lista de k elementos más cercanos
##############################################

rm(train_data)
gc()

archivo_similitudes <- paste0(folder_tables, "/similitudes_items_centradas.csv")
# archivo_similitudes <- paste0(folder_tables, "/prueba_similitudes.csv")

wc_l <- system(paste0("wc -l ", archivo_similitudes))
num_lineas <- wc_l %>% stringi::stri_extract_all(str = ., regex = "[0-9]+") %>% unlist() %>% .[1] %>% as.numeric()
# 26744

items_names <- paste0("item_", 1:num_lineas)

# Num de items más similares
k = 1000

folder_salida_json <- paste0(folder_tables, "/json_similitudes")

system(paste0("mkdir ", folder_salida_json))

num_leidas <- 0
while(num_leidas < num_lineas){
  n = 1000 # Número de items más cercanos
  hora_actual <- substr(Sys.time(), 1, 19)
  cat("Leyendo items", num_leidas + 1, "a", num_leidas + k, "\n")
  cat("\tHora:", hora_actual, "\n")
  
  similitudes <- read.csv(archivo_similitudes,
                          skip = num_leidas,
                          nrows = k,
                          header = F,
                          col.names = items_names,
                          colClasses = rep("numeric", num_lineas))
    
  cat("\tSimilitudes leídas\n")
  
  nombres_items <- paste0("item_", (num_leidas + 1):(num_leidas + nrow(similitudes)))
  
  cat("\tOrdenando items\n")
  lista_int <- lapply(1:nrow(similitudes), function(i) {
    item <- paste0("item_", num_leidas + i)
    cat("\t\t", item, "\n")
    df <- data.frame(itemId = names(similitudes),
                     sim_cos = as.numeric(similitudes[i,])) %>% 
      filter(!is.nan(sim_cos),
             itemId != item) %>% 
      arrange(desc(sim_cos)) %>% 
      head(n)
    return(df)
  })
  names(lista_int) <- nombres_items
  cat("\tItems ordenados\n")
  
  num_items_sprintf <- sprintf("%08d", (num_leidas + 1):(num_leidas + nrow(similitudes)))
  
  lista_int_json <- jsonlite::toJSON(lista_int)
  archivo_salida_json <- paste0(folder_salida_json, 
                                "/", 
                                num_items_sprintf[1], 
                                "_a_", 
                                num_items_sprintf[length(num_items_sprintf)],
                                ".json")
  write(lista_int_json, file = archivo_salida_json)
  cat("\tArchivo escrito exitosamente\n\n")
  
  num_leidas <- num_leidas + k
}








