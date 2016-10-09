### resultados.R

library(dplyr)
library(tidyr)
library(ggplot2)

source("utils.R")

errores_ml <- read.table("../out/MovieLens/2016-10-02_22_50_37/tables/errores_modelo_factorizacion.psv", sep = "|", header = T)

errores_ml %>% mutate(params = paste(dim_lat, learning_rate, lambda)) %>% gather(tipo_error, error, error_ent, error_val) %>%  ggplot() + geom_point(aes(params, error, color = tipo_error)) + axis_labels_vert()


errores_bx <- read.table("../out/BookCrossing/Final_models/tables/errores_modelo_factorizacion.psv", sep = "|", header = T)

errores_bx %>% mutate(params = paste(dim_lat, learning_rate, lambda)) %>% gather(tipo_error, error, error_ent, error_val) %>%  ggplot() + geom_point(aes(params, error, color = tipo_error)) + axis_labels_vert()


lista_fact <- readRDS("../out/MovieLens/2016-10-02_22_50_37/models/dimlat_1000_learning_rate_0.001_lambda_0.01.rds")

lista_fact <- readRDS("../out/MovieLens/Final_models_mal/models/dimlat_5_learning_rate_0.01_lambda_0.001.rds")

items <- read.csv("../out/MovieLens/items_new_ids.csv")

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





