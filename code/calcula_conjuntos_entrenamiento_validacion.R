library(dplyr)

##############################################
## Determinar el dataset que va a leer
##############################################

# Si el script se ejecuta en forma interactiva (desde algún IDE), entonces toma el dataset de movielens. Si se ejecuta automáticamente desde otro script o desde la terminal, debe llevar como argumento cuál dataset se quiere analizar
if(!interactive()){ 
  args <- commandArgs(TRUE)
  dataset <- args[1]
  if(length(args) == 0) {
    cat("No especificaste dataset ni carpeta de salida\n\n")
    quit(save = "no", status = 0, runLast = FALSE)
  } 
} else {
  dataset <- "BookCrossing"
}

archivo_calis <- paste0("../data/", dataset, "/ratings.csv")

# Lee archivos
calis <- readr::read_csv(archivo_calis) %>%   
  select(userId,
         itemId_orig = itemId,
         rating) %>% 
  mutate(itemId = as.integer(factor(itemId_orig)))
head(calis)

cant_usuarios <- length(unique(calis$userId))
cant_pelis <- length(unique(calis$itemId))

archivo_items <- paste0("../data/", dataset, "/items.csv")
nombres_items <- readr::read_csv(archivo_items) %>% 
  rename(itemId_orig = itemId) %>% 
  left_join(unique(select(calis, itemId, itemId_orig)))
write.table(nombres_items, file = paste0("../out/", dataset, "/items_new_ids.csv"),
            sep = ",", row.names = F, col.names = T)

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

dat_test <- dat_2 %>% 
  filter(valida_usu & valida_item) %>% 
  select(-valida_usu, -valida_item)


media_gral_train <- mean(dat_train$rating)

dat_train_2 <- dat_train %>% 
  mutate(u_id = as.integer(factor(userId)),
         rating_cent = rating - media_gral_train,
         media_gral_train = media_gral_train)

dat_test_2 <- dat_test %>% 
  left_join(unique(dat_train_2[,c('userId', 'u_id')])) %>% 
  mutate(rating_cent = rating - media_gral_train,
         media_gral_train = media_gral_train) %>% 
  filter(!is.na(u_id) & !is.na(itemId))

dir_out <-  paste0("../out/", dataset)

cat("Guardando...")

saveRDS(dat_train_2, paste0(dir_out, "/train.rds"))
saveRDS(dat_test_2, paste0(dir_out, "/test.rds"))


cat("Listo\n\n")
