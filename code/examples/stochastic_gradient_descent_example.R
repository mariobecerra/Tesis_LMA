library(tidyverse)

theme_set(theme_bw(base_size = 20))

system("mkdir ../../out/optim")

###################################
###################################
### Linear Regression
###################################
###################################

N <- 1000
beta_0 <- 2
beta_1 <- 1

set.seed(124362)
data <- tibble(x = rnorm(N),
               y = beta_0 + beta_1*x + rnorm(N))

modelo <- lm(y~x, data = data)



gradient_row <- function(data_row, betas){
  li <- data_row$y - betas[1] - betas[2]*data_row$x
  g1 <- -2*li
  g2 <- -2*li*data_row$x
  return(c(g1, g2))
}

epoch_update <- function(data, betas, alpha, n_epoch, verbose = 1, reordering = F){
  n <- nrow(data)
  if(reordering) data$ix <- sample(1:n)
  else data$ix <- 1:n
  epoch_values <- tibble(
    n_epoch = rep(n_epoch, n),
    obs = 1:n,
    beta_0 = rep(0, n),
    beta_1 = rep(0, n),
    gradient_norm = rep(0, n))
  
  # Iterate over rows in data
  for(i in 1:n){
    # Update coefficientes
    g <- gradient_row(data[data$ix == i,], betas) 
    betas <- betas - alpha*g
    # Print and write values in table to keep track and make plots
    g_norm <- sqrt(sum(g^2))
    g_unit <- g/g_norm
    epoch_values$beta_0[i] <- betas[1]
    epoch_values$beta_1[i] <- betas[2]
    epoch_values$gradient_norm[i] <- g_norm
    if(verbose == 2){
      cat(
        "\n\tEpoch: ", n_epoch,
        "\n\tObs: ", i, 
        "\n\tbeta_0: ", betas[1], 
        "\n\tbeta_1: ", betas[2],
        "\n\tgradient_norm: ", g_norm, 
        "\n\tDirection: ", g_unit[1], g_unit[2],
        "\n")
    }
  } # End for
  
  
  if(verbose == 1){
    cat(
      "\n\tEpoch: ", n_epoch,
      "\n\tbeta_0: ", epoch_values$beta_0[n],
      "\n\tbeta_1: ", epoch_values$beta_1[n],
      "\n\tgradient_norm: ", epoch_values$gradient_norm[n],
      "\n")
  }
  return(list(
    epoch_values = epoch_values,
    betas = betas
  ))
}


max_it <- 300
n <- nrow(data)
data_gradient_descent <- tibble(
  epoch = rep(0, max_it*n),
  obs = 1:(max_it*n),
  beta_0 = rep(0, max_it*n),
  beta_1 = rep(0, max_it*n),
  gradient_norm = rep(0, max_it*n))

i = 0
betas <- c(0, 0)
alpha = 0.001
while(i < max_it){
  i = i + 1
  epoch_betas <- epoch_update(data, betas, alpha, i, verbose = 1)
  betas_old <- betas
  betas <- epoch_betas$betas
  data_gradient_descent[((i-1)*n + 1):((i)*n),] <- epoch_betas$epoch_values
  g_norm <- epoch_betas$epoch_values$gradient_norm[n]
  dif_betas_norm <- sum((betas - betas_old)^2)
  if(g_norm < 0.000001 | dif_betas_norm < 1e-15) break
}


data_gradient_descent <- data_gradient_descent[1:(i*n),]
data_gradient_descent$it <- 1:nrow(data_gradient_descent)

# data_gradient_descent %>% 
#   filter(it %% n == 1) %>% 
#   ggplot(aes(it, gradient_norm)) +
#   geom_point(size = 0.7) +
#   geom_line(size = 0.4)


plot_gd_iter <- function(data_gradient_descent, modelo, beta_0, beta_1, denom = 0){
  # N <- nrow(data_gradient_descent)
  
  if(denom > 0) {
    data <- data_gradient_descent %>% 
      filter(it %% floor(n/denom) == 1)
  } else {
    data <- data_gradient_descent
  }
  
  gg <- data %>% 
    ggplot(aes(beta_0, beta_1)) +
    xlab("Beta 0") +
    ylab("Beta 1") +
    geom_segment(
      aes(
        xend = c(tail(beta_0, n = -1), NA),
        yend = c(tail(beta_1, n = -1), NA)
      ),
      size = 0.4,
      color = '#919191',
      arrow = arrow(length = unit(0.18, "cm"))
    ) +
    #geom_path(size = 0.6, color = 'red') +
    geom_point(size = 0.3, color = 'black') +
    geom_point(aes(x, y),
               data = tibble(x = beta_0,
                             y = beta_1)) +
    geom_point(aes(x, y),
               data = tibble(x = modelo$coefficients[1],
                             y = modelo$coefficients[2]),
               shape = 'x',
               size = 5) +
    theme(
      #panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  return(gg)
}



# Plots only at the beginning of each epoch
plot_gd_iter(data_gradient_descent, modelo, beta_0, beta_1, 1) %>% 
  ggsave(.,
         file = "../../out/optim/lin_reg_example_1_SGD_each_epoch.pdf",
         device = 'pdf',
         width = 8,
         height = 5)

# Plots all iterations in all epochs
(data_gradient_descent %>% 
    ggplot(aes(beta_0, beta_1)) +
    xlab("Beta 0") +
    ylab("Beta 1") +
    geom_path(size = 0.1, color = 'black') +
    geom_point(size = 0.01, color = 'black', alpha = 0.2) +
    geom_point(aes(x, y),
               data = tibble(x = beta_0,
                             y = beta_1)) +
    geom_point(aes(x, y),
               data = tibble(x = modelo$coefficients[1],
                             y = modelo$coefficients[2]),
               shape = 'x',
               size = 5) +
    theme(
      panel.grid.minor = element_blank()
    )
) %>% 
  ggsave(.,
         file = "../../out/optim/lin_reg_example_1_SGD_iter_all.pdf",
         device = 'pdf',
         width = 8,
         height = 5)

###################################
###################################
### Logistic Regression
###################################
###################################

rm(list = setdiff(ls(), list("epoch_update", "plot_gd_iter")))

N <- 1000
beta_0 <- 1
beta_1 <- 4

set.seed(124362)
data <- tibble(x = rnorm(N),
               z = beta_0 + beta_1*x,
               pr = 1/(1 + exp(-z)),
               y = rbinom(1000, 1, pr))

modelo <- glm(y ~ x, data = data, family = "binomial")


h <- function(x){
  return(1/(1 + exp(-x)))
}
 
# gradient <- function(data, betas){
#   n <- nrow(data)
#   const <- rep(1, nrow(data))
#   bx <- betas[1]*const + betas[2]*data$x
#   li <- data$y - h(bx)
#   g1 <- -2*sum(li)/n
#   g2 <- -2*sum(li*data$x)/n
#   return(c(g1, g2))
# }


gradient_row <- function(data_row, betas){
  bx <- betas[1] + betas[2]*data_row$x
  li <- li <- data_row$y - h(bx)
  g1 <- -2*li
  g2 <- -2*li*data_row$x
  return(c(g1, g2))
}


max_it <- 300
n <- nrow(data)
data_gradient_descent <- tibble(
  epoch = rep(0, max_it*n),
  obs = 1:(max_it*n),
  beta_0 = rep(0, max_it*n),
  beta_1 = rep(0, max_it*n),
  gradient_norm = rep(0, max_it*n))

i = 0
betas <- c(-10, 10)
alpha = 0.01
while(i < max_it){
  i = i + 1
  epoch_betas <- epoch_update(data, betas, alpha, i, verbose = 1)
  betas_old <- betas
  betas <- epoch_betas$betas
  data_gradient_descent[((i-1)*n + 1):((i)*n),] <- epoch_betas$epoch_values
  g_norm <- epoch_betas$epoch_values$gradient_norm[n]
  dif_betas_norm <- sum((betas - betas_old)^2)
  if(g_norm < 0.000001 | dif_betas_norm < 1e-15) break
}

data_gradient_descent <- data_gradient_descent[1:(i*n),]
data_gradient_descent$it <- 1:nrow(data_gradient_descent)

# Plots only at the beginning of each epoch
plot_gd_iter(data_gradient_descent, modelo, beta_0, beta_1, 1) %>% 
  ggsave(.,
         file = "../../out/optim/log_reg_example_1_SGD_each_epoch.pdf",
         device = 'pdf',
         width = 8,
         height = 5)

# Plots all iterations in all epochs
(data_gradient_descent %>% 
    ggplot(aes(beta_0, beta_1)) +
    xlab("Beta 0") +
    ylab("Beta 1") +
    geom_point(size = 0.01, color = 'black', alpha = 0.2) +
    geom_point(aes(x, y),
               data = tibble(x = beta_0,
                             y = beta_1)) +
    geom_point(aes(x, y),
               data = tibble(x = modelo$coefficients[1],
                             y = modelo$coefficients[2]),
               shape = 'x',
               size = 5) +
    theme(
      panel.grid.minor = element_blank()
    )
) %>% 
  ggsave(.,
         file = "../../out/optim/log_reg_example_1_SGD_iter_all.pdf",
         device = 'pdf',
         width = 8,
         height = 5)