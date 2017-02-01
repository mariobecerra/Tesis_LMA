library(tidyverse)

source("utils.R")

dataset <- "MovieLens"

folder_plots <- paste0(folder, "/plots")
folder_models <- paste0(folder, "/models")
folder_tables <- paste0(folder, "/tables")



file_name_train <- paste0("../out/", dataset, "/train.rds")
file_name_test <- paste0("../out/", dataset, "/test.rds")

cat("Leyendo archivos de calificaciones\n")

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






print_datos <- function(df){
  print(df %>% .$itemId %>% unique() %>% length)
  print(df %>% .$userId %>% unique() %>% length   )
  print(nrow(df))
}

print_datos(train_data)
print_datos(validation_data)
print_datos(test_data)




errores_fact <- read_psv(paste0("../out/", dataset, "/factor_models_overfit/tables/errores_modelo_factorizacion.psv"))

(errores_fact %>% 
  top_n(-7, error_val) %>% 
  gather(tipo_error, error, error_ent:error_val) %>% 
  mutate(modelo = paste0("DL: ", dim_lat, "\n",
                         "LR: ", learning_rate, "\n",
                         "λ: ", lambda),
         error = as.numeric(error),
         tipo_error = ifelse(tipo_error == "error_ent",
                             "Entrena-\nmiento",
                             "Valida-\nción")) %>% 
  ggplot() + 
  geom_point(aes(modelo, error, shape = tipo_error), size = 2.3) +
  scale_shape_discrete(name = "Tipo de error") +
  theme_bw() 
) %>% 
  ggsave(., file = paste0("../out/", 
                          dataset, 
                          "/plots/errores_ent_validacion_factorizacion.png"),
         dpi = 400)

errores_base <- read_psv(paste0("../out/", dataset, "/factor_models_overfit/tables/errores_modelo_base.psv"))

