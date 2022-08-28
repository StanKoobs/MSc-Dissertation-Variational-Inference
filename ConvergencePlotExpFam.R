### Convergence plot exponential family

nablaA <- function(theta) {
   3 * theta^3 + 100
}


library(TeX)

thetaseq <- seq(-2, 15, by = 0.05)

nablaseq <- nablaA(thetaseq)

alpha <- 0.3
gamma <- 0.3

thetan <- 3
theta <- 9
thetahat <- alpha * thetan + (1 - alpha) * theta

nablan <- nablaA(thetan)
nablareal <- nablaA(theta)
nablahat <- nablaA(thetahat)

nablanp1 <- gamma * nablahat + (1 - gamma) * nablan
thetap1 <- ((nablanp1 - 100) / 3)^(1 / 3)


ThesisggTheme <- function() {
  theme(panel.background = element_rect(fill = "grey96"),
        axis.line = element_blank(),
        panel.border = element_rect(colour = "gray82", fill = NA,
                                    size = 1.5),
        panel.grid.major = element_line(colour = "grey70", 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(colour = "grey70", 
                                        linetype = "dashed"),
        axis.text.x = element_text(size= 13),
        axis.text.y = element_text(size= 13),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.ticks = element_line(size = 1.5),
        axis.ticks.length = unit(1.5, "mm")
  )
}

ggplot() +
  geom_line(aes(x = thetaseq, y = nablaseq), size = 1.5, colour = "#F8766D") +
  geom_point(aes(x = thetan, y = nablan), size = 4) +
  geom_point(aes(x = theta, y = nablareal), size = 4) +
  geom_point(aes(x = thetahat, y = nablahat), size = 4) +
  geom_point(aes(x = thetap1, y = nablanp1), size = 4) +
  geom_segment(aes(x = thetan, y = 0, xend = theta, yend = 0), 
               size = 1, linetype = "dashed") +
  geom_segment(aes(x = thetan, y = nablan, xend = thetan, yend = 0), 
               size = 1, linetype = "dashed") +
  geom_segment(aes(x = theta, y = nablareal, xend = theta,, yend = 0), 
               size = 1, linetype = "dashed") +
  geom_segment(aes(x = thetahat, y = nablahat, xend = thetahat, yend = 0), 
               size = 1, linetype = "dashed") +
  geom_segment(aes(x = thetahat, y = nablahat, xend = thetahat, yend = 0), 
               size = 1, linetype = "dashed") +
  geom_segment(aes(x = thetan, y = nablahat, xend = thetan, yend = nablan), 
               size = 1, linetype = "dashed") +
  geom_segment(aes(x = thetahat, y = nablahat, xend = thetan, yend = nablahat), 
               size = 1, linetype = "dashed") +
  geom_segment(aes(x = thetahat, y = nablahat, xend = thetan, yend = nablahat), 
               size = 1, linetype = "dashed") +
  geom_segment(aes(x = thetahat, y = nablahat, xend = thetan, yend = nablahat), 
               size = 1, linetype = "dashed") + 
  geom_segment(aes(x = thetan, y = nablanp1, xend = thetap1, yend = nablanp1), 
               size = 1, linetype = "dashed") +
  geom_segment(aes(x = thetap1, y = nablanp1, xend = thetap1, yend = 0), 
               size = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, size = 1) +
  annotate("text", x = 3.05, y = -100, label = TeX("$\\theta_n$"), size = 8) +
  annotate("text", x = 9, y = -100, label = TeX("$\\theta$"), size = 8) +
  annotate("text", x = 7.2, y = -100, label = TeX("$\\hat{\\theta}_n$"), size = 8) +
  annotate("text", x = 5.08, y = -100, label = TeX("$\\theta_{n+1}$"), size = 8) +
  annotate("text", x = 2.5, y = 350, label = TeX("$1 - \\gamma$"), size = 9) +
  annotate("text", x = 2.7, y = 800, label = TeX("$\\gamma$"), size = 10) +
  annotate("text", x = 8, y = 80, label = TeX("$1 - \\alpha$"), size = 8) +
  annotate("text", x = 5.5, y = 80, label = TeX("$\\alpha$"), size = 8) +
  annotate("text", x = 7.8, y = 2000, label = TeX("$\\nabla A$"), size = 12,
           colour = "#F8766D") +
  ylim(-100, 2500) +
  xlim(0.5, 10) +
  theme_void() +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank() 
  )


