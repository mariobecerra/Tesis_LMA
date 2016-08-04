library(dplyr)
#library(recommenderlab)

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

# calis <- readr::read_csv(archivo_calis) %>%   
#   select(user = userId,
#          item = itemId,
#          rating) %>% 
#   as.data.frame() %>% 
#   as(., "realRatingMatrix")
  
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

dat_train %>% 
  group_by(userId) %>% 
  mutate(media_usuario = mean(rating)) %>% 
  mutate(rating = rating - media_usuario) %>% 
  select(userId, itemId, rating) %>% 
  head(1000) %>% 
  write.table(., file = "~/Desktop/ejem.csv",
              sep = ",", row.names = F)

dat_test <- dat_2 %>% 
  filter(valida_usu & valida_item) %>% 
  select(-valida_usu, -valida_item)


# r_train <- dat_train %>%
#   select(user = userId,
#          item = itemId_orig,
#          rating) %>%
#   as.data.frame() %>%
#   as(., "realRatingMatrix")

r_train <- sparseMatrix(
  i = dat_train$userId,
  j = dat_train$itemId,
  x = dat_train$rating
) 


r_test <- sparseMatrix(
  i = dat_test$userId,
  j = dat_test$itemId,
  x = dat_test$rating,
  dims = dim(r_train)
) %>% 
  as(., "realRatingMatrix")

# r_test <- dat_test %>%
#   select(user = userId,
#          item = itemId_orig,
#          rating) %>%
#   as.data.frame() %>%
#   as(., "realRatingMatrix")

rm(calis)
rm(dat_train)
rm(dat_test)
rm(dat_2)

# Recomendaciones

r1 <- Recommender(r_train[1:1000,1:1000], "IBCF")
p1 <- predict(r1, r_train[101:105,], type="ratings")


r1 <- Recommender(r_train[1:100,], "UBCF")
p1 <- predict(r1, r_test[101:105,], type="ratings")



r1 <- Recommender(r_train, "UBCF")

p1 <- predict(r1, r_test, type="ratings")








