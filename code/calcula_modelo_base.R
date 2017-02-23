library(Matrix)
library(Rcpp)
library(tidyverse)

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

system(paste("mkdir", folder))
system(paste("mkdir", folder_plots))
system(paste("mkdir", folder_models))
system(paste("mkdir", folder_tables))

file_name_train <- paste0("../out/", dataset, "/train.rds")
file_name_test <- paste0("../out/", dataset, "/test.rds")

cat("Leyendo archivos de calificaciones\n")

train_data <- readRDS(file_name_train)
test_data <- readRDS(file_name_test)

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

df_prueba <- test_data %>% 
  left_join(promedios_usuarios) %>% 
  left_join(promedios_items)

errores_gamma <- sapply(0:30, function(gamma){
  cat("gamma:", gamma, '\n')
  dat_test_3 <- df_prueba %>% 
    mutate(rating_mb = media_gral_train + 
             (num_calis_usuario * (media_usuario - media_gral_train))/(gamma + num_calis_usuario) +
             (num_calis_item * (media_item - media_gral_train))/(gamma + num_calis_item)
    ) %>% 
    mutate(rating_mb = ifelse(is.na(rating_mb), media_gral_train, rating_mb))
  
  error <- sqrt(mean((dat_test_3$rating - dat_test_3$rating_mb)^2))
  return(c(gamma, error))
}) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(gamma = V1, error = V2) %>% 
  mutate(gamma = as.integer(gamma))

#min <- min(errores_gamma$error)
gamma_min <- errores_gamma %>% 
  filter(error == min(error))

(errores_gamma %>% 
  ggplot(aes(x = gamma, y = error)) + 
  geom_line(aes(group = 1)) + 
  geom_point() +
  geom_abline(slope = 0, intercept = gamma_min$error, linetype = 'dotted') +
  geom_vline(xintercept = gamma_min$gamma, linetype = 'dotted') +
  theme_bw()) %>% 
  ggsave(., 
         file = paste0(folder_plots, "/modelo_base.png"),
         dpi = 500)

write.table(errores_gamma, 
            file = paste0(folder_tables, "/errores_modelo_base.psv"),
            col.names = T,
            sep = "|",
            row.names = F,
            quote = TRUE)

cat("¡¡Listo!! :D \n")

