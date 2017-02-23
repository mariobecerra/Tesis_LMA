# Basado en:
# Performance of Recommender Algorithms on Top-N Recommendation Tasks 
# Paolo Cremonesi
# Yehuda Koren 
# Roberto Turrin
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

items <- read.table(paste0(folder, "/items_new_ids.psv"), 
                    stringsAsFactors = F,
                    header = T,
                    quote = "",
                    sep = "|",
                    comment.char = "")

#lista_fact <- readRDS(paste0(folder, "/modelos_factorizacion/models/dimlat_1000_learning_rate_0.001_lambda_0.01.rds"))
lista_fact <- readRDS(paste0(folder, "/modelos_factorizacion/models/dimlat_200_learning_rate_0.001_lambda_0.01.rds"))

#train <- read_rds(paste0(folder, "/train.rds"))
test <- read_rds(paste0(folder, "/test.rds"))

sourceCpp("calc_error.cpp")

################################
### Top-N recommendations
################################

# Máximo rating posible en el conjunto de prueba
max_rating <- max(test$rating)

# Filtra los artículos calificados con la máxima calificación
test_max_rating <- test %>% 
  filter(rating == max_rating)

# N de Top-N recommendations
N <- 20

# Número de artículos aleatorios con los que se compara
k <- 1000

# Usuarios que calificarion con la calificación máxima
user_unique_test_max <- unique(test_max_rating$u_id)
i <- 0
non_rated_items <- lapply(user_unique_test_max, function(u_id_loop){
  i <<- i + 1
  tot <- length(user_unique_test_max)
  cat("i =", i, "\n",
      "user:", u_id_loop, "\n",
      round(100*i/tot, 1), "%\n\n")
  rated_by_user <- test %>% 
    filter(u_id == u_id_loop) %>% 
    .$itemId
  items_sample <- sample(items$itemId, k + length(rated_by_user))
  items_sample_not_rated <- items_sample[!(items_sample %in% rated_by_user)]
  items_sample_not_rated_2 <- sample(items_sample_not_rated, k)
  df_out <- tibble(u_id = u_id_loop,
                   itemId = items_sample_not_rated_2)
}) %>% 
  bind_rows()
# Dataframe que contiene, para cada usuario que calificó con la calificación máxima algún artículo, mil artículos aleatorios que no calificó antes.
saveRDS(non_rated_items, paste0(folder, "/non_rated_items.rds"))


test_top_n <- rep(0, nrow(test_max_rating))
pred_ratings_out <- rep(0, nrow(test_max_rating))
n_items_user <- rep(0, nrow(test_max_rating))
i <- 0
j <- 0
for(u_id_loop in user_unique_test_max){
  #for(u_id_loop in user_unique_test_max[1:100]){
  j <- j + 1
  cat("user:", u_id_loop, "\n",
      "iter:", j, "\n",
      100*(round(j/length(user_unique_test_max), 4)), "%\n\n")
  items_loop <- test_max_rating %>% 
    filter(u_id == u_id_loop) %>% 
    .$itemId
  # Este paso se puede acelerar usando los índices del dataframe (van de 1000 en 1000)
  items_sample_not_rated <- non_rated_items %>%
    filter(u_id == u_id_loop) %>%
    .$itemId
  # items_sample_not_rated <- non_rated_items$itemId[(j*1000 + 1):((j+1)*1000)]
  items_of_interest <- c(items_loop, items_sample_not_rated)
  pred_ratings <- lista_fact$a[u_id_loop] + lista_fact$b[items_of_interest] + 
    (lista_fact$U[u_id_loop,] %*% 
       t(lista_fact$P[items_of_interest,])) %>% 
    as.vector()
  pred_ratings_df <- tibble(itemId = items_of_interest,
                            pred_rat = pred_ratings)
  n_loop <- length(items_loop)
  #test_top_n <- rep(0, length(items_loop))
  #i <- 0
  for(itemId_loop in items_loop){
    i <- i + 1
    #cat("i", i, "\n")
    idx <- c(itemId_loop, items_sample_not_rated)
    arranged_ratings <- pred_ratings_df %>% 
      filter(itemId %in% idx) %>% 
      arrange(desc(pred_rat))
    p <- which(arranged_ratings$itemId == itemId_loop)
    pred_ratings_out[i] <- arranged_ratings$pred_rat[p]
    test_top_n[i] <- p
    n_items_user[i] <- n_loop
  }
}

Sys.time()

# test_top_n y pred_ratings_out deben de estar ordenados
test_top_n_df <- test_max_rating %>% 
  select(u_id, itemId, rating_cent) %>% 
  mutate(p = test_top_n,
         pred_rating = pred_ratings_out,
         number_of_items = n_items_user)

# Dataframe con cada artículo calificado con la máxima calificación, el predicted 
#rating y el lugar que ocupa entre mil artículos aleatorios
saveRDS(test_top_n_df, paste0(folder, "/test_top_n.rds"))
