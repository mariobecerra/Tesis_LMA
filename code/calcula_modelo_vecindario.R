# Se deben de tener ya precalculadas las similitudes entre items
library(jsonlite)
library(dplyr)
library(parallel)

cores <- detectCores()

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

#folder <- paste0("../out/", dataset, "/", time)
folder <- "../out/BookCrossing/2016-08-26_17_59_50/"
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
nombres_items <- readr::read_csv(paste0("../out/", dataset, "/items_new_ids.csv")) %>% 
  mutate(itemId = paste0("item_", itemId))

##############################################
## Similitudes
##############################################

# archivo_similitudes <- paste0(folder_tables, "/similitudes_items_centradas.csv")
folder_similitudes_json <- paste0(folder_tables, "/json_similitudes")

archivos_similitudes <- list.files(folder_similitudes_json, full.names = T)
#archivos_similitudes <- archivos_similitudes[1:17]

# similitudes <- list()
# i = 0
# while(i < length(archivos_similitudes)){
#   i = i + 1
#   archivo <- archivos_similitudes[i]
#   cat("Leyendo archivo", archivo, "\n")
#   file_content <- readLines(con = archivo)
#   cat("Archivo leído. Convirtiendo de JSON a lista.\n")
#   sims <- fromJSON(file_content, simplifyVector = F)
#   cat("Convertido. Agregando a lista final.\n")
#   similitudes <- append(similitudes, sims)
#   cat("Agregado.\n\n")
# }

lee_similitudes_JSON <- function(archivos_similitudes, num_mas_cercanos = 100, verbose = 1) {
  # archivos_similitudes: Vector de caracteres en donde cada entrada es la ruta a un archivo con formato json donde están las similitudes para cada item
  # num_mas_cercanos: Cuántas similitudes va a tomar por item
  # verbose: nivel de verbosidad. 
  #          Si es 0, no imprime nada;
  #          si es 1, imprime para cada archivo
  #          si es 2, imprime más información de cada archivo
  # Al final regresa un dataframe del estilo de:
  # itemId items_mas_cercanos sim_cos
  # (chr)              (chr)   (chr)
  # 1    item_1        item_317702  0.7894
  # 2    item_1        item_316908  0.6467
  # 3    item_1        item_315996  0.6139
  # 4 item_2998                 NA      NA
  # 5 item_2999                 NA      NA
  # 6 item_3000                 NA      NA
  
  #  num_mas_cercanos <- 100
  len_arch <- length(archivos_similitudes)
  similitudes_df <- lapply(1:len_arch, function(i) {
    if(verbose > 0) cat(i, "de", len_arch, "\n")
    archivo <- archivos_similitudes[i]
    if(verbose > 0) cat("Leyendo archivo", archivo, "...")
    file_content <- readLines(con = archivo)
    if(verbose > 0) cat("Archivo leído.\nConvirtiendo de JSON a lista...")
    sims <- fromJSON(file_content, simplifyVector = F)
    if(verbose > 0) cat("Convertido.\nConvirtiendo a dataframe...\n")
    
    # Crea un data frame donde las columnas son itemId, items_mas_cercanos_sim_cos.
    # Para cada item del archivo i, hay un dataframe de estos.
    similitides_loop <- lapply(sims, function(x) {
      if(verbose == 2) cat("\tObteniendo los más cercanos...")
      sims_lista <- x[[2]] %>% 
        head(num_mas_cercanos) %>%  
        unlist()
      if(verbose == 2) cat("Obtenidos.\n\tCreando dataframe...")
      if(length(sims_lista) > 0){
        sims_df <- data.frame(
          itemId = unlist(x[[1]]),
          items_mas_cercanos = sims_lista[names(sims_lista) == "itemId"], 
          sim_cos = sims_lista[names(sims_lista) == "sim_cos"],
          stringsAsFactors = F,
          row.names = NULL)
      } else {
        sims_df <- data.frame(
          itemId = unlist(x[[1]]),
          items_mas_cercanos = NA, 
          sim_cos = NA,
          stringsAsFactors = F,
          row.names = NULL)
      }
      if(verbose == 2) cat("Creado.\n")
      return(sims_df)
    }) %>% rbind_all()
    
    if(verbose > 0) cat("...Convertido.\n\n")
    return(similitides_loop)
  }) %>% 
    rbind_all()
  if(verbose > 0) cat("Listo.\n\n")
  return(similitudes_df)
}


#### Paralelo
lee_similitudes_JSON_par <- function(archivos_similitudes, num_mas_cercanos = 100, verbose = 1, cores = detectCores()) {
  # archivos_similitudes: Vector de caracteres en donde cada entrada es la ruta a un archivo con formato json donde están las similitudes para cada item
  # num_mas_cercanos: Cuántas similitudes va a tomar por item
  # verbose: nivel de verbosidad. 
  #          Si es 0, no imprime nada;
  #          si es 1, imprime para cada archivo
  #          si es 2, imprime más información de cada archivo
  # Al final regresa un dataframe del estilo de:
  # itemId items_mas_cercanos sim_cos
  # (chr)              (chr)   (chr)
  # 1    item_1        item_317702  0.7894
  # 2    item_1        item_316908  0.6467
  # 3    item_1        item_315996  0.6139
  # 4 item_2998                 NA      NA
  # 5 item_2999                 NA      NA
  # 6 item_3000                 NA      NA
  
  #  num_mas_cercanos <- 100
  len_arch <- length(archivos_similitudes)
  similitudes_df <- mclapply(1:len_arch, function(i) {
    if(verbose > 0) cat(i, "de", len_arch, "\n")
    archivo <- archivos_similitudes[i]
    if(verbose > 0) cat("Leyendo archivo", archivo, "...")
    file_content <- readLines(con = archivo)
    if(verbose > 0) cat("Archivo leído.\nConvirtiendo de JSON a lista...")
    sims <- fromJSON(file_content, simplifyVector = F)
    if(verbose > 0) cat("Convertido.\nConvirtiendo a dataframe...\n")
    
    # Crea un data frame donde las columnas son itemId, items_mas_cercanos_sim_cos.
    # Para cada item del archivo i, hay un dataframe de estos.
    similitides_loop <- lapply(sims, function(x) {
      if(verbose == 2) cat("\tObteniendo los más cercanos...")
      sims_lista <- x[[2]] %>% 
        head(num_mas_cercanos) %>%  
        unlist()
      if(verbose == 2) cat("Obtenidos.\n\tCreando dataframe...")
      if(length(sims_lista) > 0){
        sims_df <- data.frame(
          itemId = unlist(x[[1]]),
          items_mas_cercanos = sims_lista[names(sims_lista) == "itemId"], 
          sim_cos = sims_lista[names(sims_lista) == "sim_cos"],
          stringsAsFactors = F,
          row.names = NULL)
      } else {
        sims_df <- data.frame(
          itemId = unlist(x[[1]]),
          items_mas_cercanos = NA, 
          sim_cos = NA,
          stringsAsFactors = F,
          row.names = NULL)
      }
      if(verbose == 2) cat("Creado.\n")
      return(sims_df)
    }) %>% rbind_all()
    
    if(verbose > 0) cat("...Convertido.\n\n")
    return(similitides_loop)
  }, mc.cores = cores) %>% 
    rbind_all()
  if(verbose > 0) cat("Listo.\n\n")
  return(similitudes_df)
}



similitudes <- lee_similitudes_JSON_par(archivos_similitudes)

similitudes$sim_cos <- as.numeric(similitudes$sim_cos)

# similitudes_seq <- lee_similitudes_JSON(head(archivos_similitudes, 10))
# similitudes_par <- lee_similitudes_JSON_par(head(archivos_similitudes, 10), cores = 3)
# 
# tiempos_seq <- microbenchmark::microbenchmark(similitudes_seq <- lee_similitudes_JSON(head(archivos_similitudes, 13), verbose = 0), times = 5)
# 
# tiempos_par <- microbenchmark::microbenchmark(similitudes_par <- lee_similitudes_JSON_par(head(archivos_similitudes, 13), verbose = 0, cores = 3), times = 5)

##############################################
## Calcular RMSE de conjunto de prueba
##############################################

predict_rating <- function(test_data, train_data, similitudes, usuario) {
  
  user_test <- test_data %>% 
    filter(userId == usuario) %>% 
    mutate(itemId = paste0("item_", itemId))
  
  user_train <- train_data %>% 
    filter(userId == usuario) %>% 
    mutate(item_train = paste0("item_", itemId)) %>% 
    select(item_train,
           rating_train = rating_cent)
  
  items_to_predict_sims <- similitudes %>% 
    filter(itemId %in% user_test$itemId)
  
  # Primero encuentra los n artículos más similares a los que se van a evaluar
  # items_to_predict_sims <- lapply(user_test$itemId, function(i){
  #   items = head(similitudes[[i]]$similitudes, n)
  #   if(length(items) == 0) return(NULL)
  #   else {items %>% 
  #       mutate(item_to_predict = unlist(similitudes[[i]]$item)) %>% 
  #       select(item_to_predict, 
  #              items_mas_cercanos = itemId,
  #              sim_cos) %>% 
  #       return(.)}
  # }) %>% 
  #   rbind_all()
  
  predicciones <- items_to_predict_sims %>% 
    left_join(user_train,
              by = c("items_mas_cercanos" = "item_train")) %>% 
    filter(!is.na(rating_train)) %>% 
    group_by(itemId) %>% 
    summarise(pred_rating = sum(sim_cos * rating_train)/sum(sim_cos)) %>% 
    full_join(select(user_test, 
                     itemId = itemId,
                     rating_cent)) %>% 
    # Si no hubo items para calcular similitud y por ende una predicción, entonces la predicción es 0, o sea, el promedio del usuario
    mutate(pred_rating = ifelse(is.na(pred_rating), 0, pred_rating)) %>% 
    mutate(error = rating_cent - pred_rating)
  
  return(predicciones)
}

i <- 1
cant_usuarios <- length(unique(test_data$userId))
#### Aquí podría ser mclapply
predicciones_test <- lapply(unique(test_data$userId), function(usuario){
  cat("Usuario:", usuario, "\n")
  cat("Iteración", i, "de", cant_usuarios, "\n\n")
  i <<- i + 1
  preds <- predict_rating(test_data, train_data, similitudes, usuario)
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


# BookCrossing 
# Harry Potter
# similitudes %>% filter(itemId == "item_144117") %>% left_join(nombres_items, by = c("items_mas_cercanos" = "itemId")) %>% View

# Tolkien
# similitudes %>% filter(itemId == "item_54361") %>% left_join(nombres_items, by = c("items_mas_cercanos" = "itemId")) %>% View

# Goedel, Escher y Bach
# Checar esto, porque hay unos items que no tienen ID original, y eso está mal, no todos deberían de tener
# Update: Parece ser que es error de los datos. Originalmente no venáin esos IDS. Por ejemplo, el nuevo ID 23817 tiene como original el 0140268693 (lo vi con  train_data %>% filter(itemId == 23817)), y el ID 0140268693 no viene en el archivo de 'items' que descargué de BX.
#similitudes %>% filter(itemId == "item_120066") %>% left_join(nombres_items, by = c("items_mas_cercanos" = "itemId")) %>% View


