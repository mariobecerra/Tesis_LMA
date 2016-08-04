library(Matrix)

n_items <- 3000

#A = rsparsematrix(5*n_items, n_items, 0.005)

A = r_train[1:(5*n_items), 1:n_items]
distintos_cero <- apply(A, 2, function(x) sum(x != 0)) == 0
A[,distintos_cero] <- rsparsematrix(nrow = 1, ncol = nrow(A), density = 0.5)


cosine_sim_col <- function(A, archivo_salida){
  B = A
  A = t(A)
  normas_columnas_B <- apply(B, 2, function(x){sqrt(sum(x^2))})
  for(i in 1:nrow(A)){
    x = A[i,]
    x = x/sqrt(t(x) %*% x)
    res <- as.numeric(x %*% B) / normas_columnas_B
    cat(res, file = archivo_salida, sep = ",", append = T)
    cat("\n", file = archivo_salida, sep = "", append = T)
  }
}




time <- substr(Sys.time(), 1, 19) %>% gsub("[ :]", "_", .)
archivo_salida <- paste("~/Desktop/mult_mat", time, sep = "_")

time_cos <- system.time(
  cosine_sim_col(A, archivo_salida)
)

time_cos






cosine <- function(x) {
  y <- t(x) %*% x
  res <- y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  return(res)
}