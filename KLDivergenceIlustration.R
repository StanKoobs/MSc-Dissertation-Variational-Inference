##########################################################################################
### Replicating Figure 10.2 of Bishop (2006)
##########################################################################################

source("Packages.R")
source("DissertationggTheme.R")

# Define mean and covariance matrix of p(y|D)
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.97, 0.97, 1), ncol = 2, nrow = 2)

# Find its value on grid of points
x <- seq(-3, 3, by = 0.025)
npoints <- length(x)
data <- matrix(c(rep(x, npoints), 
                 rep(x, each = npoints)), ncol = 2)

# Define objective function to be minimised 
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

# As this is an easy 2-dimensional problem, we do not need an efficient approach
# Therefore, we use numerical KL minimisation by using the Nelder-Mead method (default of
# the optim() function)
par <- optim(c(0, 0, 0.3, 0.3), KL)$par
muvar <- par[1:2]
sigmavar <- diag(par[3:4])

# Target density
zcondx <- function(x, y) {
  dmvnorm(cbind(x, y), mean = mu, sigma = Sigma)
}
# Variational density
variational <- function(x, y) {
  dmvnorm(cbind(x, y), mean = muvar, sigma = sigmavar)
}


findlevelCurves <- function(mu, sigma, func = zcondx) {
  q1 <- qmvnorm(0.6827, mean = mu, sigma = sigma)$quantile
  q2 <- qmvnorm(0.9545, mean = mu, sigma = sigma)$quantile
  q3 <- qmvnorm(0.9973, mean = mu, sigma = sigma)$quantile
  
  if (func == "zcondx") {
    c(zcondx(q1, q1), zcondx(q2, q2), zcondx(q3, q3))
  } else if (func == "variational") {
    c(variational(q1, q1), variational(q2, q2), variational(q3, q3))
  }
}

# Values of the corresponding 1, 2, and 3-sigma contours
breaksp <- findlevelCurves(mu, Sigma, "zcondx")
breaksq <- findlevelCurves(muvar, sigmavar, "variational")

# Include all data in one data frame
data <- as_tibble(data)
names(data) <- c("x", "y")
datap <- data %>% mutate(z = zcondx(x, y), c = as.factor("Conditional"))
dataq <- data %>% mutate(z = variational(x, y), 
                         c = as.factor("Variational"))



ForwardPlot <- ggplot() +
  stat_contour(data = datap, aes(x = x, y = y, z = z, colour = "$p(y|\\mathcal{D})$"), 
               breaks = breaksp, size = 1) +
  stat_contour(data = dataq, aes(x = x, y = y, z = z, colour = "$q(y)$"),
               breaks = breaksq, size = 1) +
  labs(color = "Distribution") + 
  DissertationggTheme() +
  ggtitle("Exclusive KL") +
  theme(legend.title = element_text(size = 13),
        legend.text = element_text(size = 13),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  xlim(-4.1, 4.1) +
  ylim(-4.1, 4.1)
  #scale_x_continuous(breaks = c(-2.5, 0, 2.5)) +
  #scale_y_continuous(breaks = c(-2.5, 0, 2.5)) 

ForwardPlot

options(tz = "Europe/Berlin")

tikz(file = "KLDivForward.tex", width = 5, height = 4)

ForwardPlot

dev.off()

# In the part below, we repeat the work done above for the reverse KL divergence

x <- seq(-4.25, 4.25, by = 0.025)
npoints <- length(x)
data <- matrix(c(rep(x, npoints), 
                 rep(x, each = npoints)), ncol = 2)

par <- optim(c(0, 0, 1, 1), KL, type = "Reverse")$par
muvar <- par[1:2]
sigmavar <- diag(par[3:4])

breaksp <- findlevelCurves(mu, Sigma, "zcondx")
breaksq <- findlevelCurves(muvar, sigmavar, "variational")

data <- as_tibble(data)
names(data) <- c("x", "y")
datap <- data %>% mutate(z = zcondx(x, y))
dataq <- data %>% mutate(z = variational(x, y))


ReversePlot <- ggplot() +
  stat_contour(data = datap, aes(x = x, y = y, z = z, colour = "$p(y|\\mathcal{D})$"), 
               breaks = breaksp, size = 1) +
  stat_contour(data = dataq, aes(x = x, y = y, z = z, colour = "$q(y)$"), 
               breaks = breaksq, size = 1) +
  labs(color = "Distribution") + 
  DissertationggTheme() +
  ggtitle("Inclusive KL") +
  theme(legend.title = element_text(size = 13),
        legend.text = element_text(size = 13),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ReversePlot


options(tz = "Europe/Berlin")

tikz(file = "KLDivForward.tex", width = 5, height = 4)

ReversePlot

dev.off()

# Combine both plots in one figure



plot_grid(ForwardPlot, ReversePlot)

