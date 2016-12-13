library(dplyr)
library(tidyr)
library(Matrix)
library(Rcpp)
library(ggplot2)

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
## Compilar funciones en Rcpp
##############################################

cat("Compilar funciones en Rcpp\n")

Rcpp::sourceCpp('gradiente.cpp')
Rcpp::sourceCpp('calc_error.cpp')

##############################################
## Función que encuentra las dimensiones latentes
##############################################

encontrar_dim_latentes <- function(i, j, x, i.v, j.v, x.v, gamma, lambda, k, deltalim, maxiter = 250){
  X <- sparseMatrix(i = i, j = j, x = x)
  X.v <- sparseMatrix(i = i.v, j = j.v, x = x.v)
  set.seed(2805)
  U <- matrix(rnorm(k*dim(X)[1],0,0.01), ncol=k, nrow=dim(X)[1])
  P <- matrix(rnorm(k*dim(X)[2],0,0.01), ncol=k, nrow=dim(X)[2])
  a <- rep(0, dim(X)[1])
  b <- rep(0, dim(X)[2]) 
  num_iter <- 1
  delta <- deltalim + 1
  erroresent <- 0
  erroresval <- 0
  horas_iter <- Sys.time()
  while(delta > deltalim & num_iter < maxiter){
    ee1 <- sqrt(calc_error(i, j, x, U, P, a, b))
    ev1 <- sqrt(calc_error(i.v, j.v, x.v, U, P, a, b))
    erroresent <- append(erroresent, ee1)
    erroresval <- append(erroresval, ev1)
    hora <- Sys.time()
    horas_iter <- append(horas_iter, hora)
    cat("\tNúmero de iteración:", num_iter, '\n')
    cat("\tHora:", as.character(hora), '\n')
    cat("\tlambda =", lambda, ", gamma =", gamma, ", k =", k, "\n")
    out <- gradiente(i, j, x, U, P, a, b, gamma, lambda)
    U <- out[[1]]
    P <- out[[2]]
    a <- out[[3]]
    b <- out[[4]]
    ee2 <- sqrt(calc_error(i, j, x, U, P, a, b))
    ev2 <- sqrt(calc_error(i.v, j.v, x.v, U, P, a, b))
    num_iter <- num_iter + 1
    delta <- (ev1 - ev2)/ev1
    cat('\terror entrenamiento =', ee2, "\n")
    cat('\terror validación =', ev2, '\n')
    cat('\tdelta =', delta, '\n\n')
  }
  
  df_it <- data.frame(iter=2:num_iter)
  df_it$erroresent <- erroresent[2:num_iter]
  df_it$erroresval <- erroresval[2:num_iter]
  df_it$horas <- horas_iter[2:num_iter]
  l <- list(P, U, a, b, df_it)
  names(l) <- c('P', 'U', 'a', 'b', 'err')
  return(l)
}

##############################################
## Aplicar la función
##############################################

cat("Aplicar la función\n\n")

#time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
file_tabla_errores <- paste0(folder_tables,
                             "/errores_modelo_factorizacion.psv")

cat("dim_lat|learning_rate|lambda|iter|error_ent|error_val\n",
    file = file_tabla_errores)

# k <- 1000
# gamma <- 0.001
# lambda <- 0.01

dimensiones_lat <- lapply(c(200, 1000), function(k) #Num dimensiones latentes
  lapply(0.001, function(gamma) # Gamma: learning rate
    lapply(0.01, function(lambda) { # lambda: parámetro de regularización
      cat('dimensiones =', k, '\n')
      cat('gamma (learning rate) =', gamma, '\n')
      cat('lambda (regularización) =', lambda, '\n\n')
      temp <- encontrar_dim_latentes(i = train_data$u_id, 
                                     j = train_data$itemId, 
                                     x = train_data$rating_cent,
                                     i.v = test_data$u_id,
                                     j.v = test_data$itemId,
                                     x.v = test_data$rating_cent,
                                     gamma, 
                                     lambda, 
                                     k, 
                                     deltalim = 0.000001,
                                     maxiter = 400)
      saveRDS(temp, paste0(folder_models,
                           '/dimlat_',
                           k, 
                           "_learning_rate_", 
                           gamma, 
                           "_lambda_",
                           lambda, 
                           ".rds"))
      
      title <- paste0("k = ", k,
                      ", gamma = ", gamma,
                      ", lambda = ", lambda)
      title_file_plot <- paste0(folder_plots,
                                '/modelo_factorizacion_dimlat_',
                                k, 
                                "_learning_rate_", 
                                gamma, 
                                "_lambda_",
                                lambda, 
                                ".png")
      
      temp$err %>%  
        rename(Entrenamiento = erroresent, Validación = erroresval) %>% 
        gather(tipo_error, error, Entrenamiento, Validación) %>% 
        mutate(iter = as.integer(iter)) %>% 
        ggplot(aes(x = iter, y = error, group = tipo_error, color = tipo_error)) +
        geom_line() + 
        geom_point() + 
        scale_x_continuous(breaks = seq(0, max(temp$err$iter), 5)) + 
        ggtitle(title) +
        labs(colour = "Tipo de error") + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggsave(file = title_file_plot)
      
      #e0 <- temp$err[nrow(temp$err),]
      e0 <- temp$err[temp$err$erroresval == min(temp$err$erroresval),]
      e <- paste(k, gamma, lambda, e0$iter, e0$erroresent, e0$erroresval, sep = "|")
      cat(e,
          "\n",
          append = T,
          file = file_tabla_errores)
    } )))

cat("\n\n¡¡Listo!! :D\n\n")

