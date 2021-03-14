## Set wd,  read in data and load packages
setwd("")

library(readxl)
library(ggplot2)

df <- read_excel("kohler_etal_2017.xls")
# library(tidyverse)


## Calculate delta years (date of sample - date of local appearance of domesticated plants)
df$Date = df$Date_AD - df$LocalNeo

str(df)

## Plot
# plotLabels <- as_labeller(c('NW' = "New World", 'OW' = 'Old World'))

ggplot(data = df) +
  aes(x = Date, y = Gini, color = factor(World)) +
  geom_point(size = 4) +
  stat_smooth(se = TRUE, fill = "grey80", size = 2) +
  labs(x = "Years since domestication",
       y = 'Gini coefficient',
       color = 'Area') +
  theme(legend.position = c(0.1, 0.88),
        legend.direction = "vertical",
        legend.text=element_text(size = 20, face = "italic"),
        legend.title = element_blank(),
        legend.key.width = unit(2,"cm"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "grey10"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80"),
        axis.text.x = element_text(color = "grey10", size = 16, face = "plain"),
        axis.text.y = element_text(color = "grey10", size = 16, face = "plain"),  
        axis.title.x = element_text(color = "grey10", size = 20, face = "italic"),
        axis.title.y = element_text(color = "grey10", size = 20, face = "italic")) +
  scale_colour_manual(labels = c("New World", "Old World"), values = c("#2b5bc2", "#bf7f08"))
