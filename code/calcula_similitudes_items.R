library(dplyr)
library(Matrix)

normas_columnas_matriz_ratings <- function(df){
  df %>% 
    group_by(itemId) %>% 
    summarise(normas = sqrt(sum(rating * rating)))
}

cosine_sim_col <- function(A, archivo_salida){
  B = A
  A = t(A)
  #normas_columnas_B <- sapply(B, 2, norm)
  normas_columnas_B <- list()
  num_cols <- ncol(B)
  for(i in 1:num_cols){
    cat("Calculando norma de columna", i, "de", num_cols, "\n")
    x <- B[,i]
    norma <- sqrt(t(x) %*% x)
    normas_columnas_B <- append(normas_columnas_B, norma)
  }
  normas_columnas_B <- unlist(normas_columnas_B)
  num_filas <- nrow(A)
  cat("Normas calculadas\n\n")
  for(i in 1:num_filas){
    cat("Fila", i, "de", num_filas, "\n")
    x = A[i,]
    x = x/sqrt(t(x) %*% x)
    res <- as.numeric(x %*% B) / normas_columnas_B
    cat(res, file = archivo_salida, sep = ",", append = T)
    cat("\n", file = archivo_salida, sep = "", append = T)
  }
}


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


folder <- paste0("../out/", dataset, "/", time)
folder_plots <- paste0(folder, "/plots")
folder_models <- paste0(folder, "/models")
folder_tables <- paste0(folder, "/tables")

system(paste("mkdir", folder))
system(paste("mkdir", folder_plots))
system(paste("mkdir", folder_models))
system(paste("mkdir", folder_tables))

archivo_calis <- paste0("../data/", dataset, "/ratings.csv")

cat("Leyendo archivo de calificaciones\n")

calis <- readr::read_csv(archivo_calis) %>%   
  select(userId,
         itemId_orig = itemId,
         rating) %>% 
  mutate(itemId = as.integer(factor(itemId_orig)))

cant_usuarios <- length(unique(calis$userId))
cant_pelis <- length(unique(calis$itemId))

##############################################
## Conjuntos de prueba y validación
##############################################

cat("Conjuntos de prueba y validación\n")

set.seed(2805)

valida_usuarios <- sample(unique(calis$userId), cant_usuarios*.3 )
valida_items <- sample(unique(calis$itemId), cant_pelis*.3 )

dat_2 <- calis %>%
  mutate(valida_usu = userId %in% valida_usuarios) %>%
  mutate(valida_item = itemId %in% valida_items)

dat_train <- dat_2 %>% 
  filter(!valida_usu | !valida_item) %>% 
  select(-valida_usu, -valida_item)

# cant_usuarios_train <- length(unique(dat_train$userId))
# cant_items_train <- length(unique(dat_train$itemId))
#
# r_train <- sparseMatrix(
#   i = dat_train$userId,
#   j = dat_train$itemId,
#   x = dat_train$rating
# ) 
# 
# archivo_salida <- paste0(folder_tables, "/similitudes_items.csv")
# 
# 
# 
# 
# 
# 
# 
# normas_columnas_matriz_ratings <- function(df){
#   df %>% 
#     group_by(itemId) %>% 
#     summarise(normas = sqrt(sum(rating * rating)))
# }

cosine_sim_items <- function(df_ratings, archivo_salida){
  
  normas_items <- df_ratings %>% 
    group_by(itemId) %>% 
    summarise(normas = sqrt(sum(rating * rating))) %>% 
    right_join(data.frame(itemId = 1:max(df_ratings$itemId))) %>% 
    mutate(normas = ifelse(is.na(normas), 0, normas)) %>% 
    .$normas
  
  R <- sparseMatrix(
    i = df_ratings$userId,
    j = df_ratings$itemId,
    x = df_ratings$rating
  ) 
  
  num_items <- ncol(R)
  cat("Normas calculadas\n\n")
  for(i in 1:num_items){
    cat("Item", i, "de", num_items, "\n")
    x = R[,i]
    x = x/sqrt(t(x) %*% x)
    res <- as.numeric(x %*% R) / normas_items
    cat(res, file = archivo_salida, sep = ",", append = T)
    cat("\n", file = archivo_salida, sep = "", append = T)
  }
}

archivo_salida <- paste0(folder_tables, "/similitudes_items.csv")

cosine_sim_items(dat_train, archivo_salida)

# 
# cosine <- function(df_ratings) {
#   R <- sparseMatrix(
#     i = df_ratings$userId,
#     j = df_ratings$itemId,
#     x = df_ratings$rating
#   ) 
#   
#   y <- t(R) %*% R
#   res <- y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
#   return(res)
# }
# 
# #cosine_sim_items(dat_train, archivo_salida)
# 
# 
# #### Probar
# 
# 
# df_ratings <- dat_train[100:600,] %>% 
#   mutate(itemId = as.integer(factor(itemId)))
# 
# similitudes1 <- cosine(df_ratings)
# 
# 
# cosine_sim_items(df_ratings, archivo_salida)
