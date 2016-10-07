library(dplyr)

time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)

# system(paste('Rscript calcula_conjuntos_entrenamiento_validacion.R BookCrossing'))
# system(paste('Rscript calcula_modelo_base.R BookCrossing', time))
# system(paste('Rscript calcula_similitudes_items.R BookCrossing', time))
system(paste('Rscript calcula_modelo_vecindario.R BookCrossing', time))
# system(paste('Rscript calcula_modelo_factorizacion.R BookCrossing', time))
#system(paste('Rscript calcula_modelo_factorizacion_2.R BookCrossing', time))
