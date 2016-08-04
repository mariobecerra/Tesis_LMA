library(Matrix)

Rcpp::sourceCpp("cosine_sim.cpp")

#A = rsparsematrix(138493, 26744, 0.005)

n_items <- 300

#A = rsparsematrix(5*n_items, n_items, 0.005)

A = r_train[1:(5*n_items), 1:n_items]
distintos_cero <- apply(A, 1, function(x) sum(x != 0)) == 0
A[distintos_cero,] <- rsparsematrix(nrow = 1, ncol = ncol(A), density = 0.1)

B = t(A)

normas_columnas <- apply(B, 2, function(x){sqrt(sum(x^2))})
B/normas_columnas

mult1 <- function(A, B, archivo_salida){
  for(i in 1:nrow(A)){
    for(j in 1:ncol(B)){
      res <- A[i,] %*% B[,j]
      if(j == ncol(B)) {
        cat(res, file = archivo_salida, sep = "", append = T)
      } else{
        cat(res, ",", file = archivo_salida, sep = "", append = T)
      }
    }
    cat("", file = archivo_salida, sep = "\n", append = T)
  }
}


mult2 <- function(A, B, archivo_salida){
  for(i in 1:nrow(A)){
    for(j in 1:ncol(B)){
      res <- A[i,] %*% B[,j]
      cat(res, ",", file = archivo_salida, sep = "", append = T)
    }
    cat("", file = archivo_salida, sep = "\n", append = T)
  }
}


mult3 <- function(A, B, archivo_salida){
  for(i in 1:nrow(A)){
    fila <- ""
    for(j in 1:ncol(B)){
      res <- A[i,] %*% B[,j]
      fila <- paste0(fila, res, ",")
    }
    cat(fila, "\n", file = archivo_salida, sep = "", append = T)
  }
}

mult4 <- function(A, B, archivo_salida){
  for(i in 1:nrow(A)){
    res <- as.numeric(A[i,] %*% B)
    cat(res, file = archivo_salida, sep = ",", append = T)
    cat("\n", file = archivo_salida, sep = "", append = T)
  }
}



cosine_sim <- function(A, B, archivo_salida){
  normas_columnas <- apply(B, 2, function(x){sqrt(sum(x^2))})
  B = B/normas_columnas
  for(i in 1:nrow(A)){
    res <- as.numeric(A[i,] %*% B)
    cat(res, file = archivo_salida, sep = ",", append = T)
    cat("\n", file = archivo_salida, sep = "", append = T)
  }
}




time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
archivo_salida <- paste("~/Desktop/mult_mat", time, sep = "_")

time1 <- system.time(
  mult1(A, t(A), archivo_salida)
)


time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
archivo_salida <- paste("~/Desktop/mult_mat", time, sep = "_")

time2 <- system.time(
  mult2(A, t(A), archivo_salida)
)


time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
archivo_salida <- paste("~/Desktop/mult_mat", time, sep = "_")

time3 <- system.time(
  mult3(A, t(A), archivo_salida)
)


time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
archivo_salida <- paste("~/Desktop/mult_mat", time, sep = "_")

time4 <- system.time(
  mult4(A, t(A), archivo_salida)
)



time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
archivo_salida <- paste("~/Desktop/mult_mat", time, sep = "_")

time_cos <- system.time(
  cosine_sim(A, t(A), archivo_salida)
)



