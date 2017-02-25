library(dplyr)
library(stringi)

source("utils.R")

##############################################
## Determinar el dataset con el que se va a trabajar
##############################################

# Si el script se ejecuta en forma interactiva (desde algún IDE), entonces toma el dataset de BookCrossing. Si se ejecuta automáticamente desde otro script o desde la terminal, debe llevar como argumento cuál dataset se quiere analizar

if(!interactive()){ 
  args <- commandArgs(TRUE)
  dataset <- args[1]
  time <- args[2]
  if(length(args) == 0) {
    cat("No especificaste dataset ni carpeta de salida\n\n")
    quit(save = "no", status = 0, runLast = FALSE)
  } 
} else {
  # dataset <- "BookCrossing"
  dataset <- "MovieLens"
  time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
}

folder <- paste0("../out/", dataset)
file_out <- paste0(folder, "/modelos_factorizacion/vecinos_peliculas/tabla.tex")

##############################################


lista_tablas <- list.files(paste0(folder, "/modelos_factorizacion/vecinos_peliculas"),
                           full.names = T) %>% 
  grep(".*tex", ., value = T, perl = T, invert = T)

cat("\\begin{tabular}{ |l|l|l| }\n\\hline\n\\multicolumn{3}{ |c| }{Vecinos} \\\\\n\\hline\nPelícula & Película más cercana & Distancia \\\\ \\hline\n", file = file_out)
for(x in lista_tablas){
  tabla <- read_psv(x) %>% 
    head(11) %>% 
    select(title, distancia) 
  
  cat("\\multirow{10}{*}{",
      tabla[[1,1]], "}", file = file_out, append = T)
  for(j in 2:11){
    nombre_peli <- stri_replace_all(str = tabla[[j, 1]], fixed = "&", replacement = "\\&")
    nombre_peli <- iconv(nombre_peli, from = "utf-8", to = "ASCII//TRANSLIT")
    cat(" & ", nombre_peli, " & ", round(tabla[[j, 2]], 2), " \\\\\n", file = file_out, append = T)  
  }
  cat("\\hline\n", file = file_out, append = T)
}
cat("\\end{tabular}\n", file = file_out, append = T)


