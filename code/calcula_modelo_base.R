library(Matrix)
library(Rcpp)
library(tidyverse)

theme_set(theme_bw(base_size = 20))

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
  #dataset <- "MovieLens"
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

file_name_train <- paste0("../out/", dataset, "/train.rds")
file_name_test <- paste0("../out/", dataset, "/test.rds")

cat("Leyendo archivos de calificaciones\n")

# train_data <- readRDS(file_name_train)
# test_data <- readRDS(file_name_test)
train_data <- readRDS(file_name_train)
test_data_0 <- readRDS(file_name_test)

media_gral_train <- mean(train_data$rating)

cant_usuarios <- length(unique(test_data_0$userId))
cant_pelis <- length(unique(test_data_0$itemId))

valida_usuarios <- sample(unique(test_data_0$userId), cant_usuarios*.5 )
valida_items <- sample(unique(test_data_0$itemId), cant_pelis*.5 )

dat_2 <- test_data_0 %>%
  mutate(valida_usu = userId %in% valida_usuarios) %>%
  mutate(valida_item = itemId %in% valida_items)

validation_data <- dat_2 %>% 
  filter(!valida_usu | !valida_item) %>% 
  select(-valida_usu, -valida_item)

test_data <- dat_2 %>% 
  filter(valida_usu & valida_item) %>% 
  select(-valida_usu, -valida_item)

rm(test_data_0)
rm(dat_2)
rm(valida_usuarios)
rm(valida_items)


##############################################
## Modelo base
##############################################

cat("Calcular modelo base\n")
promedios_usuarios <- train_data %>% 
  group_by(u_id) %>% 
  summarise(media_usuario = mean(rating),
            num_calis_usuario = n())

promedios_items <- train_data %>% 
  group_by(itemId) %>% 
  summarise(media_item = mean(rating),
            num_calis_item = n())

df_validation <- validation_data %>% 
  left_join(promedios_usuarios) %>% 
  left_join(promedios_items)

errores_gamma <- sapply(0:30, function(gamma){
  cat("gamma:", gamma, '\n')
  dat_validation <- df_validation %>% 
    mutate(rating_mb = media_gral_train + 
             (num_calis_usuario * (media_usuario - media_gral_train))/(gamma + num_calis_usuario) +
             (num_calis_item * (media_item - media_gral_train))/(gamma + num_calis_item)
    ) %>% 
    mutate(rating_mb = ifelse(is.na(rating_mb), media_gral_train, rating_mb))
  
  error <- sqrt(mean((dat_validation$rating - dat_validation$rating_mb)^2))
  return(c(gamma, error))
}) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(gamma = V1, error = V2) %>% 
  mutate(gamma = as.integer(gamma))

#min <- min(errores_gamma$error)
gamma_min <- errores_gamma %>% 
  filter(error == min(error))
cat("gamma mínima:", gamma_min$gamma, '\n')
cat("error de validación:", gamma_min$error, '\n')

(errores_gamma %>% 
  ggplot(aes(x = gamma, y = error)) + 
  geom_line(aes(group = 1)) + 
  geom_point() +
  geom_abline(slope = 0, intercept = gamma_min$error, linetype = 'dotted') +
  geom_vline(xintercept = gamma_min$gamma, linetype = 'dotted')) %>% 
  ggsave(., 
         file = paste0(folder_plots, "/modelo_base.pdf"),
         device = 'pdf')

write.table(errores_gamma, 
            file = paste0(folder_tables, "/errores_modelo_base.psv"),
            col.names = T,
            sep = "|",
            row.names = F,
            quote = TRUE)



df_test <- test_data %>% 
  left_join(promedios_usuarios) %>% 
  left_join(promedios_items)

dat_test <- df_test %>% 
  mutate(rating_mb = media_gral_train + 
           (num_calis_usuario * (media_usuario - media_gral_train))/(gamma_min$gamma + num_calis_usuario) +
           (num_calis_item * (media_item - media_gral_train))/(gamma_min$gamma + num_calis_item)
  ) %>% 
  mutate(rating_mb = ifelse(is.na(rating_mb), media_gral_train, rating_mb))

error_test <- sqrt(mean((dat_test$rating - dat_test$rating_mb)^2))
cat("error de prueba:", error_test, '\n')

cat("gamma mínima:", gamma_min$gamma, '\n', 
    file = paste0(folder_tables, "/error_prueba_validacion.txt"))
cat("error de validación:", gamma_min$error, '\n', 
    file = paste0(folder_tables, "/error_prueba_validacion.txt"), 
    append = T)
cat("error de prueba:", error_test, '\n',
    file = paste0(folder_tables, "/error_prueba_validacion.txt"), 
    append = T)

cat("¡¡Listo!! :D \n")

