### resultados.R

################################
### Paquetes
################################

library(Rcpp)
library(Matrix)
library(tidyverse)

source("utils.R")


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
  time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
}

folder <- paste0("../out/", dataset)

################################
### Leer datos
################################

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


#lista_fact <- readRDS(paste0(folder, "/modelos_factorizacion/models/dimlat_1000_learning_rate_0.001_lambda_0.01.rds"))
lista_fact <- readRDS(paste0(folder, "/modelos_factorizacion/models/dimlat_200_learning_rate_0.001_lambda_0.01.rds"))

(lista_fact$err %>% 
  filter(iter > 2) %>% 
  gather(tipo_error, error, erroresent:erroresval) %>% 
  mutate(tipo_error = ifelse(tipo_error == "erroresent",
                             "Entrena-\nmiento",
                             "Valida-\nción")) %>% 
  ggplot() +
  geom_point(aes(iter, error, shape = tipo_error)) +
  scale_shape_discrete(name = "Tipo de error") +
  theme_bw() +
  xlab("Número de iteración")) %>% 
  ggsave(., 
         file = paste0(folder, "/plots/error_por_iteracion_modelo_fact.png"),
         dpi = 500)
  
  

train <- read_rds(paste0(folder, "/train.rds"))
test <- read_rds(paste0(folder, "/test.rds")) %>% 
    filter(itemId <= max(train$itemId)) # Para que no haya problema de suscript out of bounds en caso de que los últimos artículos de test no esté en train

test_top_n_df <- read_rds(paste0(folder, "/test_top_n.rds"))

sourceCpp("calc_error.cpp")

################################
### Análisis
################################

errores_ml %>% 
  mutate(params = paste(dim_lat, learning_rate, lambda)) %>% 
  gather(tipo_error, error, error_ent, error_val) %>%  
  ggplot() + 
  geom_point(aes(params, error, color = tipo_error)) + 
  axis_labels_vert()

# errores_bx <- read.table("../out/BookCrossing/modelos_factorizacion/tables/errores_modelo_factorizacion.psv", sep = "|", header = T)
# 
# errores_bx %>% mutate(params = paste(dim_lat, learning_rate, lambda)) %>% gather(tipo_error, error, error_ent, error_val) %>%  ggplot() + geom_point(aes(params, error, color = tipo_error)) + axis_labels_vert()



# lista_fact <- readRDS("../out/MovieLens/modelos_factorizacion_mal/models/dimlat_5_learning_rate_0.01_lambda_0.001.rds")


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
### Top-N recommendations
################################




################################
### Representantes de factores latentes
################################

system(paste0("mkdir ", folder, "/factores_latentes_representantes"))

# Factor latente 1:
arrange(P_df, desc(X1)) %>% 
  head(50) %>% 
  rename(FL = X1) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/factores_latentes_representantes/FL_1_head.psv"))

arrange(P_df, desc(X1)) %>% 
  tail(50) %>% 
  rename(FL = X1) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/factores_latentes_representantes/FL_1_tail.psv"))

# Factor latente 2:
arrange(P_df, desc(X2)) %>% 
  head(50) %>% 
  rename(FL = X2) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/factores_latentes_representantes/FL_2_head.psv"))

arrange(P_df, desc(X2)) %>% 
  tail(50) %>% 
  rename(FL = X2) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/factores_latentes_representantes/FL_2_tail.psv"))

# Factor latente 3:
arrange(P_df, desc(X3)) %>% 
  head(50) %>%
  rename(FL = X3) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/factores_latentes_representantes/FL_3_head.psv"))

arrange(P_df, desc(X3)) %>% 
  tail(50) %>% 
  rename(FL = X3) %>% 
  select(everything(), -starts_with("X")) %>% 
  write_psv(., paste0(folder, "/factores_latentes_representantes/FL_3_tail.psv"))


################################
### Artículos similares con matriz de items
################################

encontrar_vecinos <- function(id, k, P_df){
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
  
  return(out)
}

if(dataset == "MovieLens"){
  # Parecidas a Toy Story
  encontrar_vecinos(1, 40, P_df)
  
  # Parecidas a Aladdin
  encontrar_vecinos(583, 40, P_df)
  
  # Parecidas a Pulp Fiction
  encontrar_vecinos(294, 40, P_df)
  
  # Parecidas a Interstellar
  encontrar_vecinos(22802, 40, P_df)
  
  # Parecidas a Lion King
  encontrar_vecinos(361, 40, P_df)
  
  # Parecidas a Hannibal
  encontrar_vecinos(4055, 40, P_df)
  
  # Parecidas a Sin City
  encontrar_vecinos(9933, 40, P_df)
  
  # Parecidas a Sin City 2
  encontrar_vecinos(23754, 40, P_df)
} else {
  # Parecidos a Harry Potter and the Sorcerer's Stone
  encontrar_vecinos(77945, 40, P_df) 
  
  # Parecidos a Harry Potter and the Chamber of Secrets
  encontrar_vecinos(52606, 40, P_df) 
  
  
}



################################
### Ejemplos de usuarios
################################

set.seed(124362)
usuarios <- sample(unique(test$u_id), 5)

usuarios_ejemplos_items_gustan <- train %>% 
  filter(u_id %in% usuarios) %>% 
  filter(rating_cent > 0) %>% 
  group_by(u_id) %>% 
  top_n(10, rating_cent) %>% 
  # arrange(desc(rating_cent))
  left_join(items) %>% 
  select(u_id, rating, title, genres)







