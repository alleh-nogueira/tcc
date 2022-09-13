
# data
df <- data.frame(
  p = c(1, 2, 3, 4), ## This provides an order to the data
  label = c("Age (65 and older versus <65)", "Male versus Female", "High income versus Low income", "High school or higher versus No High school"),
  or = c(1.00, 2.00, 3.00, 0.50),
  ll = c(0.25, 0.90, 2.25, 0.2),
  ul = c(1.75, 3.10, 3.75, 0.8),
  ci = c("0.25, 1.75", "0.90, 3.10", "2.25, 3.75", "0.20, 0.80")
)

# forest plot with OR table
library(tidyverse)
library(gridExtra)

## forest plot
forestPlot <- ggplot(df,
       aes(x = or, 
           y = reorder(label, or))) +
  geom_vline(xintercept = 1,
             color = "black",
             linetype = "dashed",
             cex = 0.5,
             alpha = 0.5) +
  geom_errorbarh(aes(xmin = ll,
                     xmax = ul),
                 height = 0) +
  geom_point(shape = 15, 
             size = 5,
             color = "#009FC3") +
  scale_x_continuous(limits = c(0, 4), 
                     expand = expansion(0, 0)) +
  labs(y = "",
       subtitle = "Razão de Chances (IC 95%)\n") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(7.5, "pt"),
        axis.line.x = element_line(color = "black"),
        axis.text.y = element_text(size = 12,
                                   color = "black",
                                   family = "Montserrat",
                                   margin = margin(r = 40),
                                   hjust = 0),
        axis.text.x.bottom = element_text(size = 12,
                                          color = "black",
                                          family = "Montserrat",
                                          margin = margin(t = 10, b = 20)),
        axis.title.x = element_blank(),
        plot.subtitle = element_text(size = 12,
                                    hjust = 0.5,
                                    color = "black",
                                    family = "Montserrat",
                                    face = "bold"))

## base table
tableBase <- ggplot(df, aes(y=label)) +
  labs(x = "",
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5,
                                  size=12), 
        axis.text.x = element_text(color="white", 
                                   hjust = -3, 
                                   size = 25), # This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## point estimate table
tableEstimate <- tableBase + 
  labs(title = "\n") +
  geom_text(aes(y = reorder(label, or),
                x = 1,
                label = paste(sprintf("%0.2f",
                                      round(or, 1)),
                              " (",
                              sprintf("%0.2f",
                                      round(ll, 1)),
                              "-",
                              sprintf("%0.2f",
                                      round(ul, 1)),
                              ")", 
                              sep = "")),
            size = 4,
            family = "Montserrat") + ## decimal places
  theme(title = element_text(size = 12,
                             color = "black",
                             family = "Montserrat",
                             face = "bold"))
tableEstimate

## P value table
tableP <- tableBase + 
  labs(title = "P\n") +
  geom_text(aes(y = reorder(label, or),
                x = 1,
                label = p),
            size = 4) +
  theme(title = element_text(size = 12,
                             color = "black",
                             family = "Montserrat",
                             face = "bold"))


## combining elements of forest plot
tableLayout <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,2,2,3,3), nrow = 1)
grid.arrange(forestPlot, tableEstimate, tableP, layout_matrix = tableLayout)

