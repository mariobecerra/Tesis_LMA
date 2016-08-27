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
folder_salida_json <- paste0(folder_tables, "/json_similitudes")

system(paste("mkdir", folder))
system(paste("mkdir", folder_plots))
system(paste("mkdir", folder_models))
system(paste("mkdir", folder_tables))
system(paste0("mkdir ", folder_salida_json))

file_name_train <- paste0("../out/", dataset, "/train.rds")
file_name_test <- paste0("../out/", dataset, "/test.rds")

cat("Leyendo archivos de calificaciones\n")

train_data <- readRDS(file_name_train)

########################################################
####### Función que calcula similitudes
########################################################

cosine_sim_items <- function(df_ratings, folder_salida_json, k = 1000, n = 1000){
  # n: Número de items más cercanos
  # k: Número de filas que va a leer cada iteración
  
  # Calcula la norma de las columnas de la matriz de ratings
  # Es más rápido con dplyr por la ralidad
  cat("Calculando normas...")
  normas_items <- df_ratings %>% 
    group_by(itemId) %>% 
    summarise(normas = sqrt(sum(rating * rating))) %>% 
    right_join(data.frame(itemId = 1:max(df_ratings$itemId))) %>% 
    mutate(normas = ifelse(is.na(normas), 0, normas)) %>% 
    .$normas
  
  cat("Normas calculadas\n")
  
  cat("Crear matriz de calificaciones rala...")
  
  # Crear matriz de calificaciones rala
  R <- sparseMatrix(
    i = df_ratings$userId,
    j = df_ratings$itemId,
    x = df_ratings$rating
  ) 
  
  cat("Listo\n\n")
  
  num_items <- ncol(R)
  items_names <- paste0("item_", 1:num_items)

  num_calculadas <- 0
  
  while(num_calculadas < num_items){
  #while(num_calculadas < 31){
    
    if(num_calculadas + k > num_items){
      idx_fin <- num_items
    } else {
      idx_fin <- num_calculadas + k
    }
    
    lista_fin <- lapply((num_calculadas + 1):(idx_fin), function(i){
      
      item <- paste0("item_", i)
      cat("Item", i, "de", num_items, "..... ")
      
      # Calcula la similitud coseno entre cada par de columnas de la matriz R
      x = R[,i] # i-ésima columna de R
      norm_x <- sqrt(t(x) %*% x)
      # La siguiente división puede resultar en NaNs cuando en la columna hay puros ceros. 
      # Esto sucede en items que no tienen ninguna calificación.
      # Al final tiene sentido remmplazar los NaNs por ceros, así no van a ser tomados en cuenta para el vecindario
      x = x/norm_x
      res <- as.numeric(x %*% R) / normas_items
      
      # Data frame con dos columnas: la primera trae los items más cercanos al item correspondiente, y la segunda trae la similitud
      df <- data.frame(itemId = items_names,
                       sim_cos = res,
                       stringsAsFactors = F) %>% 
        filter(!is.nan(sim_cos),
               itemId != item) %>% 
        arrange(desc(sim_cos)) %>% 
        head(n)
      
      cat("similitudes calculadas\n")
      return(list(item = item, similitudes = df))
      
    })
    
    num_items_sprintf <- sprintf(
      "%08d", 
      (num_calculadas + 1):idx_fin
    )
    
    lista_fin_json <- jsonlite::toJSON(lista_fin)
    
    archivo_salida_json <- paste0(folder_salida_json, 
                                  "/", 
                                  num_items_sprintf[1], 
                                  "_a_", 
                                  num_items_sprintf[length(num_items_sprintf)],
                                  ".json")
    
    cat("\n\tEscribiendo archivo...")
    write(lista_fin_json, file = archivo_salida_json)
    num_calculadas <- idx_fin
    cat("Archivo escrito exitosamente\n\n")
  }
}



#cosine_sim_items(head(train_data, 100), folder_salida_json, k = 5, n = 1000)
cosine_sim_items(train_data, folder_salida_json, k = 1000, n = 1000)





