### resultados.R

library(Rcpp)
library(Matrix)
library(tidyverse)

source("utils.R")

errores_ml <- read.table("../out/MovieLens/2016-10-02_22_50_37/tables/errores_modelo_factorizacion.psv", sep = "|", header = T)

errores_ml %>% mutate(params = paste(dim_lat, learning_rate, lambda)) %>% gather(tipo_error, error, error_ent, error_val) %>%  ggplot() + geom_point(aes(params, error, color = tipo_error)) + axis_labels_vert()

# errores_bx <- read.table("../out/BookCrossing/Final_models/tables/errores_modelo_factorizacion.psv", sep = "|", header = T)
# 
# errores_bx %>% mutate(params = paste(dim_lat, learning_rate, lambda)) %>% gather(tipo_error, error, error_ent, error_val) %>%  ggplot() + geom_point(aes(params, error, color = tipo_error)) + axis_labels_vert()


lista_fact <- readRDS("../out/MovieLens/2016-10-02_22_50_37/models/dimlat_1000_learning_rate_0.001_lambda_0.01.rds")
lista_fact <- readRDS("../out/MovieLens/2016-10-02_22_50_37/models/dimlat_200_learning_rate_0.001_lambda_0.01.rds")

# lista_fact <- readRDS("../out/MovieLens/Final_models_mal/models/dimlat_5_learning_rate_0.01_lambda_0.001.rds")

items <- read_csv("../out/MovieLens/items_new_ids.csv")

P_df <- data.frame(itemId = 1:dim(lista_fact$P)[1], lista_fact$P) %>% 
  left_join(items)


# Factor latente 1:
arrange(P_df, desc(X1)) %>% head(20) %>% select(X1, title, genres)
arrange(P_df, desc(X1)) %>% tail(20) %>% select(X1, title, genres)

# Factor latente 2:
arrange(P_df, desc(X2)) %>% head(20) %>% select(X2, title, genres)
arrange(P_df, desc(X2)) %>% tail(20) %>% select(X2, title, genres)

# Factor latente 3:
arrange(P_df, desc(X3)) %>% head(20) %>% select(X3, title, genres)
arrange(P_df, desc(X3)) %>% tail(20) %>% select(X3, title, genres)

test <- read_rds("../out/MovieLens/test.rds")

sourceCpp("calc_error.cpp")


encontrar_vecinos <- function(id, k, P_df){
  n <- ncol(P_df)
  aa <- RANN::nn2(query = P_df %>% 
                               filter(itemId == id) %>% 
                               .[,2:(n-3)],
                             data = P_df[,2:(n-3)],
                             k = k)
  
  out <- P_df %>% 
    select(title, genres) %>% 
    .[aa[[1]],] %>% 
    mutate(distancia = as.vector(aa[[2]]))
  
  return(out)
}


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


X.v <- sparseMatrix(i = test$u_id, j = test$itemId, x = test$rating_cent)

calc_error(test$u_id, test$itemId, test$rating_cent, lista_fact$U, lista_fact$P, rep(0, dim(X.v)[1]), rep(0, dim(X.v)[2]))









