### Replicating Figure 1 Minka (2005)

source("Packages.R")
source("DissertationggTheme.R")

# Point grid
x <- seq(-1.8, 2, by = 0.01)

# Mixture of Gaussians we will be trying to approximate
p <- function(y) {
  0.45 * dnorm(y, -0.5, 0.2) + 0.55 * dnorm(y, 0.5, 0.4)
}

# Function for alpha divergence for alpha is not zero or one
falpha <- function(u, alpha) {
  1 / (alpha * (alpha - 1)) * (u^alpha - 1)
}

# Alpha-divergence function
alphadiv <- function(py, qy, alpha) {
  # qy and py are vectors of probability densities expected to be of the 
  # same length

  if (length(py) != length(qy)) {
    stop("py and qy not of equal length")
  }
  
  if (alpha == 0) {
    #KLD(py, qy)$sum.KLD.py.px
    sum(qy * (log(qy) - log(py)) + py - qy)
  } else if (alpha == 1) {
    #KLD(py, qy)$sum.KLD.px.py
    sum(py * (log(py) - log(qy))) + sum(qy - py)
  } else {
    #sum(falpha(qy / py, alpha = alpha) * py)
    1 / (alpha * (1 - alpha)) * 
      sum(alpha * py + (1 - alpha) * qy - py^alpha * qy^(1 - alpha))
  }
}

# Now we use q(y) = mass * N(y; mu, sigma) as our variational family
# Note that mass is a scaler that allows q(y) to be unnormalised
# This leads to the following objective function
objective <- function(vec, alpha) {
  # vec is expected to be 3-dim input vector with mu, sigma, and mass
  mu <- vec[1]; sigma <- vec[2]; mass <- vec[3]
  densityp <- p(x)
  densityq <- mass * dnorm(x, mu, sigma)
  alphadiv(densityq, densityp, alpha = alpha)
}

# As it is a simple 1D problem, we use a simple built-in optimisation 
# algorithm. That is, we use the  method "L-BFGS-B" of Byrd et. al. (1995) 
# which allows box constraints, that is, each variable can be given a 
# lower and/or uppe bound.
findParameters <- function(alpha) {
  optim(c(0.4, 0.4, 0.6), 
        objective, 
        alpha = alpha, 
        lower = c(-Inf, 0, 0),
        upper = c(10, 10, 10),
        method = "L-BFGS-B")$par
}

# Genrate sequence of py values
py <- p(x)

# Function to generate desired plots for particular alpha
generateplot <- function(alpha) {
  par <- findParameters(alpha)
  
  qy <- par[3] * dnorm(x, par[1], par[2]) 
  
  # Used for plotting "q" at the right spot in the figure
  xcoord <- x[which.max(qy)]
  ycoord <- max(qy)
  
  ggplot() +
    geom_line(aes(x = x, y = py, colour = "py"), size = 1.2) +
    geom_line(aes(x = x, y = qy, colour = "qy"), size = 1.2) +
    xlab(TeX(paste0("$\\alpha$ = ", as.character(alpha)))) +
    #ggtitle(paste("Alpha = ", alpha)) +
    #theme(plot.title = element_text(hjust = 0.5, size = 16, 
    #                                face = "bold")) +
    ThesisggTheme() +
    theme(legend.position = "none", 
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank()) +
    annotate(geom = "text", x = -0.75, y = 0.9, label = "p",
             color = "#F8766D", size = 10) +
    annotate(geom = "text", x = xcoord + 0.3, y = ycoord + 0.02, 
             label = "q", color = "#00BFC4", size = 10) +
    ylim(0, 1.02)
  
}

# Generate the five figures
pm10 <- generateplot(-10)
p0 <- generateplot(0)
p05 <- generateplot(0.5)
p1 <- generateplot(1)
p10 <- generateplot(10)

# Final figure
plot_grid(pm10, p0, p05, p1, p10, ncol = 5) 

