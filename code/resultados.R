### resultados.R

errores_ml <- read.table("../out/MovieLens/Final_models/tables/errores_modelo_factorizacion.psv", sep = "|", header = T)

errores_ml %>% mutate(params = paste(dim_lat, learning_rate, lambda)) %>% gather(tipo_error, error, error_ent, error_val) %>%  ggplot() + geom_point(aes(params, error, color = tipo_error)) + axis_labels_vert()


errores_bx <- read.table("../out/BookCrossing/Final_models/tables/errores_modelo_factorizacion.psv", sep = "|", header = T)

errores_bx %>% mutate(params = paste(dim_lat, learning_rate, lambda)) %>% gather(tipo_error, error, error_ent, error_val) %>%  ggplot() + geom_point(aes(params, error, color = tipo_error)) + axis_labels_vert()
