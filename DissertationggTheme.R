##### In this script we define our own ggplot theme #######################

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
