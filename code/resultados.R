### resultados.R

################################
### Paquetes
################################

library(Rcpp)
library(Matrix)
library(tidyverse)

source("utils.R")

theme_set(theme_bw(base_size = 20))

##############################################
## Determinar el dataset con el que se va a trabajar
##############################################

# Si el script se ejecuta en forma interactiva (desde algún IDE), entonces toma el dataset de BookCrossing. Si se ejecuta automáticamente desde otro script o desde la terminal, debe llevar como argumento cuál dataset se quiere analizar

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
  # dataset <- "MovieLens"
  time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
}

folder <- paste0("../out/", dataset)

################################
### Leer datos
################################

# Modelo factorización
errores_ml <- read.table(paste0(folder, "/modelos_factorizacion/tables/errores_modelo_factorizacion.psv"), 
                         sep = "|", 
                         header = T)


if(dataset == "BookCrossing"){
  items <- read.table(paste0(folder, "/items_new_ids.psv"), 
                      stringsAsFactors = F,
                      header = T,
                      quote = "",
                      sep = "|",
                      comment.char = "") %>% 
    select(itemId, 
           itemId_orig,
           title = Book.Title,
           author = Book.Author,
           year = Year.Of.Publication)
} else{
  items <- read.table(paste0(folder, "/items_new_ids.psv"), 
                      stringsAsFactors = F,
                      header = T,
                      quote = "",
                      sep = "|",
                      comment.char = "")
}


lista_fact <- readRDS(paste0(folder, "/modelos_factorizacion/models/dimlat_200_learning_rate_0.001_lambda_0.01.rds"))


train <- read_rds(paste0(folder, "/train.rds"))
test <- read_rds(paste0(folder, "/test.rds")) %>% 
  filter(itemId <= max(train$itemId)) # Para que no haya problema de suscript out of bounds en caso de que los últimos artículos de test no esté en train

#test_top_n_df <- read_rds(paste0(folder, "/test_top_n.rds"))

sourceCpp("calc_error.cpp")

################################
### Análisis
################################


## Gráfica de error de validación y de entrenamiento en cada iteración del algoritmo de optimización
(lista_fact$err %>% 
   filter(iter > 2) %>% 
   gather(tipo_error, error, erroresent:erroresval) %>% 
   mutate(tipo_error = ifelse(tipo_error == "erroresent",
                              "Entrenamiento",
                              "Validación")) %>% 
   ggplot(aes(iter, error)) +
   geom_point(aes(shape = tipo_error), size = 2.5) +
   geom_line(aes(group = tipo_error)) +
   scale_shape_discrete(name = "Tipo de error") +
   xlab("Número de iteración")
) %>% 
  ggsave(., 
         file = paste0(folder, "/plots/error_por_iteracion_modelo_fact.pdf"),
         device = "pdf")


# Gráfica de errores de entrenamiento y validación de cada modelo
(errores_ml %>% 
    top_n(-7, error_val) %>% 
    gather(tipo_error, error, error_ent:error_val) %>% 
    mutate(modelo = paste0("DL: ", dim_lat, "\n",
                           "LR: ", learning_rate, "\n",
                           "L: ", lambda),
           error = as.numeric(error),
           tipo_error = ifelse(tipo_error == "error_ent",
                               "\nEntrena-\nmiento\n",
                               "\nValida-\nción\n")) %>% 
    ggplot() + 
    geom_point(aes(modelo, error, shape = tipo_error), size = 2.3) +
    scale_shape_discrete(name = "Tipo de error") +
    theme(axis.text=element_text(size = 11))
  
) %>% 
  ggsave(., 
         file = paste0("../out/", 
                       dataset, 
                       "/plots/errores_ent_validacion_factorizacion.pdf"),
         device = 'pdf')


P_df <- data.frame(itemId = 1:dim(lista_fact$P)[1], lista_fact$P) %>% 
  left_join(items)

################################
### RMSE del conjunto de prueba
################################

test_rmse <- calc_error(test$u_id, 
                        test$itemId, 
                        test$rating_cent, 
                        lista_fact$U, 
                        lista_fact$P, 
                        lista_fact$a, 
                        lista_fact$b) %>% 
  sqrt()

cat(test_rmse, file = paste0(folder, "/modelos_factorizacion/tables/test_rmse.txt"))

################################
### Representantes de factores latentes
################################

system(paste0("mkdir ", folder, "/modelos_factorizacion/factores_latentes_representantes"))

# Factor latente 1:
arrange(P_df, desc(X1)) %>% 
  head(50) %>% 
  rename(FL = X1) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/modelos_factorizacion/factores_latentes_representantes/FL_1_head.psv"))

arrange(P_df, desc(X1)) %>% 
  tail(50) %>% 
  rename(FL = X1) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/modelos_factorizacion/factores_latentes_representantes/FL_1_tail.psv"))

# Factor latente 2:
arrange(P_df, desc(X2)) %>% 
  head(50) %>% 
  rename(FL = X2) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/modelos_factorizacion/factores_latentes_representantes/FL_2_head.psv"))

arrange(P_df, desc(X2)) %>% 
  tail(50) %>% 
  rename(FL = X2) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/modelos_factorizacion/factores_latentes_representantes/FL_2_tail.psv"))

# Factor latente 3:
arrange(P_df, desc(X3)) %>% 
  head(50) %>%
  rename(FL = X3) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/modelos_factorizacion/factores_latentes_representantes/FL_3_head.psv"))

arrange(P_df, desc(X3)) %>% 
  tail(50) %>% 
  rename(FL = X3) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/modelos_factorizacion/factores_latentes_representantes/FL_3_tail.psv"))


################################
### Artículos similares con matriz de items
################################

folder_vecinos <- paste0(folder, "/modelos_factorizacion/vecinos_peliculas/")
system(paste0("mkdir ", folder_vecinos))

encontrar_vecinos <- function(id, k, P_df, out_folder = NULL){
  # Recibe el id de un artículo y regresa los k más parecidos de 
  # acuerdo a la matriz de artículos P_df
  n <- ncol(P_df)
  aa <- RANN::nn2(query = P_df %>% 
                    filter(itemId == id) %>% 
                    select(starts_with("X")) %>% 
                    as.matrix(),
                  data = P_df %>% 
                    select(starts_with("X")) %>% 
                    as.matrix(),
                  k = k)
  out <- P_df %>% 
    select(everything(), -starts_with("X")) %>% 
    .[aa[[1]],] %>% 
    mutate(distancia = as.vector(aa[[2]]))
  
  if(!is.null(out_folder)){
    item_title <- out$title[[1]] %>% 
      make_names() %>% 
      stringi::stri_replace_all(., replacement = '_', regex = '__+') %>% 
      stringi::stri_replace_last(., replacement = '', regex = '_+$')
    
    file_name <- paste0(out_folder, item_title) %>% 
      stringi::stri_replace_all(., replacement = '/', regex = '//+')
    
    write_psv(out, file_name)  
  } else{
    return(out)
  }
}


if(dataset == "MovieLens"){
  # Parecidas a Toy Story
  encontrar_vecinos(1, 40, P_df, folder_vecinos)
  
  # Parecidas a Aladdin
  encontrar_vecinos(583, 40, P_df, folder_vecinos)
  
  # Parecidas a Pulp Fiction
  encontrar_vecinos(294, 40, P_df, folder_vecinos)
  
  # Parecidas a Interstellar
  encontrar_vecinos(22802, 40, P_df, folder_vecinos)
  
  # Parecidas a Lion King
  encontrar_vecinos(361, 40, P_df, folder_vecinos)
  
  # Parecidas a Hannibal
  encontrar_vecinos(4055, 40, P_df, folder_vecinos)
  
  # Parecidos a Harry Potter and the Sorcerer's Stone
  encontrar_vecinos(4801, 40, P_df, folder_vecinos)
  
  # Parecidas a Sin City
  encontrar_vecinos(9933, 40, P_df, folder_vecinos)
  
  # Parecidas a Sin City 2
  encontrar_vecinos(23754, 40, P_df, folder_vecinos)
  
  
} else {
  # Parecidos a Harry Potter and the Sorcerer's Stone
  encontrar_vecinos(77945, 40, P_df) 
  
  # Parecidos a Harry Potter and the Chamber of Secrets
  encontrar_vecinos(52606, 40, P_df) 
}



################################
### Ejemplos de usuarios
################################

# set.seed(124362)
# usuarios <- sample(unique(test$u_id), 5)
# 
# usuarios_ejemplos_items_gustan <- train %>% 
#   filter(u_id %in% usuarios) %>% 
#   filter(rating_cent > 0) %>% 
#   group_by(u_id) %>% 
#   top_n(10, rating_cent) %>% 
#   # arrange(desc(rating_cent))
#   left_join(items) %>% 
#   select(u_id, rating, title, genres)



################################
### Top-N recommendations
################################

# Basado en:
# Performance of Recommender Algorithms on Top-N Recommendation Tasks 
# Paolo Cremonesi, Yehuda Koren, Roberto Turrin

# Dataframe con todos los usuarios que calificaron con la máxima calificación algún artículo, los artículos que cada uno de esos usuarios calificó, el predicted  rating y el lugar que ocupa entre mil artículos aleatorios (p). También incluye el número de items que el usuario calificó.
# Uno para el modelo de factorización y uno para el modelo base.
top_n_fact <- readRDS(paste0(folder, "/test_top_n.rds"))
top_n_mb <- readRDS(paste0(folder, "/test_top_n_modelo_base.rds"))

calcular_precision_recall <- function(df, N){
  num_filas <- nrow(df)
  df <- df %>% 
    mutate(hit = p <= N)
  recall <- sum(df$hit)/num_filas
  precision <- recall/N
  return(c(precision, recall))
}

precision_recall <- lapply(1:25, function(N){
  vec_mb <- calcular_precision_recall(top_n_mb, N)
  vec_fact <- calcular_precision_recall(top_n_fact, N)
  return(tibble(
    modelo = c("Fact", "Base"),
    N = c(N, N), 
    precision = c(vec_fact[1], vec_mb[1]), 
    recall = c(vec_fact[2], vec_mb[2])
  ))
}) %>% 
  bind_rows() %>% 
  arrange(modelo)

(precision_recall %>% 
    ggplot(aes(N, recall, group = modelo)) + 
    geom_point(aes(shape = modelo), size = 2) + 
    geom_line(aes(linetype = modelo)) +
    ylab("Recall")
) %>% 
  ggsave(., 
         file = paste0(folder, "/plots/recall_base_fact_", dataset, ".pdf"),
         device = "pdf")


(precision_recall %>% 
    ggplot(aes(N, precision, group = modelo)) + 
    geom_point(aes(shape = modelo), size = 2) + 
    geom_line(aes(linetype = modelo)) +
    ylab("Precisión")
) %>% 
  ggsave(., 
         file = paste0(folder, "/plots/precision_base_fact_", dataset, ".pdf"),
         device = "pdf")

system(paste0("mkdir ", folder, "/tables"))

write_psv(precision_recall, 
          paste0(folder, "/tables/precision_recall_top_N_recommendations.psv"))







