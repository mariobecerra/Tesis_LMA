library(dplyr)
## MovieLens
time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)

system(paste('Rscript calcula_modelo_base.R MovieLens', time))
system(paste('Rscript calcula_modelo_factorizacion.R MovieLens', time))
