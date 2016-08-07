library(dplyr)

time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)

system(paste('Rscript calcula_conjuntos_entrenamiento_validacion.R MovieLens', time))
system(paste('Rscript calcula_modelo_base.R MovieLens', time))
system(paste('Rscript calcula_modelo_vecindario.R MovieLens', time))
system(paste('Rscript calcula_modelo_factorizacion.R MovieLens', time))
