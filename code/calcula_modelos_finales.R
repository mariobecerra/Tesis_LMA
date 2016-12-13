library(dplyr)

time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)

system(paste('Rscript calcula_modelo_fact_final.R MovieLens', time))

system(paste('Rscript calcula_modelo_fact_final.R BookCrossing', time))
