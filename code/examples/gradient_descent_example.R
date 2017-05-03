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


(data %>% 
  ggplot(aes(x, y)) +
  geom_point(size = 0.7, alpha = 0.6) +
  geom_abline(slope = beta_1, intercept = beta_0)
) %>% 
  ggsave(.,
         file = "../../out/optim/lin_reg_example_1.pdf",
         device = 'pdf')

gradient <- function(data, betas){
  n <- nrow(data)
  const <- rep(1, nrow(data))
  li <- data$y - betas[1]*const - betas[2]*data$x
  g1 <- -2*sum(li)/n
  g2 <- -2*sum(li*data$x)/n
  return(c(g1, g2))
}


max_it <- 100
data_gradient_descent <- tibble(it = 1:max_it,
                                beta_0 = rep(0, max_it),
                                beta_1 = rep(0, max_it),
                                gradient_norm = rep(0, max_it))

i = 0
betas <- c(0, 0)
alpha = 0.1
while(i < max_it){
  i = i + 1
  g <- gradient(data, betas)
  g_norm <- sqrt(sum(g^2))
  g_unit <- g/g_norm
  cat("It: ", i, 
      "\n\tbeta_0: ", betas[1], 
      "\n\tbeta_1: ", betas[2],
      "\n\tgradient_norm: ", g_norm, 
      "\n\tDirection: ", g_unit[1], g_unit[2],
      "\n\n")
  data_gradient_descent$beta_0[i] <- betas[1]
  data_gradient_descent$beta_1[i] <- betas[2]
  data_gradient_descent$gradient_norm[i] <- g_norm
  if(g_norm < 0.000001) break
  betas <- betas - alpha*g
}


data_gradient_descent <- data_gradient_descent[1:i,]

data_gradient_descent %>% 
  ggplot(aes(it, gradient_norm)) +
  geom_point(size = 0.7) +
  geom_line(size = 0.4)



# data_gradient_descent %>% 
#   ggplot(aes(beta_0, beta_1)) +
#   geom_path(size = 0.3, color = 'red') +
#   geom_point(size = 0.7, alpha = 0.4)


(data_gradient_descent %>% 
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
  geom_point(size = 0.4, color = 'black') +
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
) %>% 
  ggsave(.,
         file = "../../out/optim/lin_reg_example_1_GD_iter.pdf",
         device = 'pdf',
         width = 8,
         height = 5)


###################################
###################################
### Logistic Regression
###################################
###################################

rm(list = ls())

N <- 1000
beta_0 <- 1
beta_1 <- 4

set.seed(124362)
data <- tibble(x = rnorm(N),
               z = beta_0 + beta_1*x,
               pr = 1/(1 + exp(-z)),
               y = rbinom(1000, 1, pr))

modelo <- glm(y ~ x, data = data, family = "binomial")


(data %>% 
  ggplot(aes(x, y)) +
  geom_point(size = 0.5, alpha = 0.4) +
  stat_smooth(method="glm", 
              method.args = list(family = "binomial"), 
              se = FALSE,
              size = 0.6,
              color = 'black')
) %>% 
  ggsave(.,
         file = "../../out/optim/log_reg_example_1.pdf",
         device = 'pdf')

h <- function(x){
  return(1/(1 + exp(-x)))
}

gradient <- function(data, betas){
  n <- nrow(data)
  const <- rep(1, nrow(data))
  bx <- betas[1]*const + betas[2]*data$x
  li <- data$y - h(bx)
  g1 <- -2*sum(li)/n
  g2 <- -2*sum(li*data$x)/n
  return(c(g1, g2))
}


deviance <- function(betas, data = data){
  n <- nrow(data)
  const <- rep(1, nrow(data))
  bx <- betas[1]*const + betas[2]*data$x
  hbx <- h(bx)
  aux <- (1 - data$y)*log(1 - hbx)
  aux2 <- ifelse(is.nan(aux), 0, aux)
  li <- data$y*log(hbx) + aux2
  d <- -2*sum(li)/n
  return(d)
}


backtrack <- function(betas, g, alpha, deviance, data, rho = 0.5, max_iter = 50, c1 = 10^-4){
  funct_value <- do.call(deviance, list(betas, data))
  norm_pk <- sum(g^2)
  i = 0
  while(i < max_iter){
    i = i + 1
    left <- do.call(deviance, list(betas - alpha*g, data))
    right <- funct_value + c1*alpha*norm_pk
    if(left <= right) break
    alpha = alpha*rho
  }
  return(list(alpha = alpha,
              iter = i))
}



max_it <- 1000
data_gradient_descent <- tibble(it = 1:max_it,
                                beta_0 = rep(0, max_it),
                                beta_1 = rep(0, max_it),
                                gradient_norm = rep(0, max_it),
                                deviance = rep(0, max_it),
                                alpha = rep(0, max_it))

i = 0
betas <- c(-10, 10) # reales 1 y 4
g <- gradient(data, betas)
g_norm <- sqrt(sum(g^2))
while(i < max_it){
  i = i + 1
  g_norm_0 <- g_norm # old gradient norm
  g <- gradient(data, betas)
  g_norm <- sqrt(sum(g^2))
  g_unit <- g/g_norm
  data_gradient_descent$beta_0[i] <- betas[1]
  data_gradient_descent$beta_1[i] <- betas[2]
  data_gradient_descent$gradient_norm[i] <- g_norm
  data_gradient_descent$deviance[i] <- deviance(betas, data)
  if(g_norm < 0.0001 & g_norm/g_norm_0 > 0.8) break
  backtrack_result <- backtrack(betas, g, 3, deviance, data)
  alpha <- backtrack_result[[1]]
  cat("It: ", i, 
      "\n\tbeta_0: ", betas[1], 
      "\n\tbeta_1: ", betas[2],
      "\n\tgradient_norm: ", g_norm, 
      "\n\tDirection: ", g_unit[1], g_unit[2],
      "\n\talpha: ", alpha,
      "\n\n")
  data_gradient_descent$alpha[i] <- alpha
  betas <- betas - alpha*g
}


data_gradient_descent <- data_gradient_descent[1:i,]

data_gradient_descent %>% 
  ggplot(aes(it, gradient_norm)) +
  geom_point(size = 0.7) +
  geom_line(size = 0.4)

data_gradient_descent %>% 
  ggplot(aes(it, deviance)) +
  geom_point(size = 0.7) +
  geom_line(size = 0.4)



(data_gradient_descent %>% 
  #filter(it < 35) %>% 
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
  geom_point(size = 0.4, color = 'black') +
  geom_point(aes(x, y),
             data = tibble(x = beta_0,
                           y = beta_1)) +
  geom_point(aes(x, y),
             data = tibble(x = modelo$coefficients[1],
                           y = modelo$coefficients[2]),
             shape = 'x',
             size = 5) +
  theme(panel.grid.minor = element_blank())
) %>% 
  ggsave(.,
         file = "../../out/optim/log_reg_example_1_GD_iter.pdf",
         device = 'pdf',
         width = 8,
         height = 5)



