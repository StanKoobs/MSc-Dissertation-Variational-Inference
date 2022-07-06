### Replicating Figure 10.2 Bishop (2006)

mu <- c(0, 0)
Sigma <- matrix(c(1, 0.95, 0.95, 1), ncol = 2, nrow = 2)

#install.packages("mvtnorm")
library(mvtnorm)

npoints <- length(seq(-3, 3, by = 0.05))
data <- matrix(c(rep(seq(-3, 3, by = 0.05), npoints), 
                 rep(seq(-3, 3, by = 0.05), each = npoints)), 
               ncol = 2)

# Numerical "brute force" KL minimisation

KL <- function(vec, type = "Forward") {
  mu1 <- vec[1]; mu2 <- vec[2]; sigma1 <- vec[3]; sigma2 <- vec[4]
  densityzcx <- dmvnorm(data, mu, Sigma)
  densityz <- dmvnorm(data, c(mu1, mu2), diag(c(sigma1, sigma2)))
  result <- KLD(densityz, densityzcx)
  if (type == "Forward") {
    result$sum.KLD.px.py
  } else if (type == "Reverse") {
    result$sum.KLD.py.px
  }
}

par <- optim(c(0,0,1,1), KL)$par
muvar <- par[1:2]
sigmavar <- diag(par[3:4])

q1 <- qmvnorm(0.6827, mean = mu, sigma = Sigma)
q2 <- qmvnorm(0.9545, mean = mu, sigma = Sigma)
q3 <- qmvnorm(0.9973, mean = mu, sigma = Sigma)


q1var <- qmvnorm(0.6827, mean = muvar, sigma = sigmavar)
q2var <- qmvnorm(0.9545, mean = muvar, sigma = sigmavar)
q3var <- qmvnorm(0.9973, mean = muvar, sigma = sigmavar)

zcondx <- function(x, y) {
  dmvnorm(cbind(x, y), mean = mu, sigma = Sigma)
}
variational <- function(x, y) {
  dmvnorm(cbind(x, y), mean = muvar, sigma = sigmavar)
}

data


#install.packages("tidyverse")
library(tidyverse)

data <- as_tibble(data)
names(data) <- c("x", "y")
dataz <- data %>% mutate(z = zcondx(x, y), c = as.factor("Conditional"))
datavar <- data %>% mutate(z = variational(x, y), c = as.factor("Variational"))


library(ggplot2)

ggplot(dataz, aes(x = x, y = y, z = z)) + stat_contour(breaks = c(0.007, 0.096, 0.424))

ggplot(datavar, aes(x = x, y = y, z = z)) + stat_contour(breaks = c(0.0002, 0.03, 0.677))


ggplot() +
  stat_contour(data = dataz, aes(x = x, y = y, z = z, colour = "blue"), breaks = c(0.007, 0.096, 0.424)) +
  stat_contour(data = datavar, aes(x = x, y = y, z = z), breaks = c(0.0002, 0.03, 0.677))

bind_rows(dataz, datavar) %>%
  ggplot(aes(x = x, y = y, z = z, colour = c)) + stat_contour(breaks = c(0.007, 0.096, 0.424))
  


p = ggplot(data, aes(x = x, y = y, z = qvar)) + geom_contour_filled()

