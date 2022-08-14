### Generating illustrative plot smoothness and strong convexity

source("Packages.R")

f <- function(x) {
  x^2 - 2 * x
}

smooth <- function(x) {
  3 * x^2 - 2 * x
}

strongconvex <- function(x) {
  0.4 * x^2 - 2 * x
}


x <- seq(-5, 5, by = 0.005)

fx <- f(x)
smoothx <- smooth(x)
strongconvexx <- strongconvex(x)


ggplot() +
  geom_line(aes(x = x, smoothx, colour = "Smoothness bound"), size = 0.8) +
  geom_line(aes(x = x, fx, colour = "f"), size = 0.8) +
  geom_line(aes(x = x, strongconvexx, colour = "Strong convexity bound"), 
            size = 0.8) +
  geom_point(aes(x = 0, y = 0), size = 2, colour = "black") +
  ylim(-4, 7) +
  xlim(-2, 4) +
  annotate("text", x = -0.22, y = -0.2, label = "f(x)", size = 8) +
  scale_color_manual(name = "Legend", 
                     values = c("Smoothness bound" = "#F8766D",
                                "f" = "#7CAE00",
                                "Strong convexity bound" = "#00BFC4")) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 18))



