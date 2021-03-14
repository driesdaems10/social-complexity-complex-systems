## Set wd,  read in data and load packages
library(ggplot2)

setwd("")
df1 <- read.csv("Turchin_et_al_2017_1.csv")
df2 <- read.csv("Turchin_et_al_2017_2.csv")
# library(tidyverse)

## Plot

length(levels(df2$NGA)) # 30
head(df2)

# Plot Request 6 ######

ggplot(data = df2) +
  aes(x = Time, y = V1, color = factor(NGA)) +
  # geom_point() +
  geom_line(data = df2) +
  geom_smooth(color = '#3e38e8', fill = "#9794f2", size = 2) + 
  labs(x = "Time",
       y = 'PC1') +
  theme(legend.position = "none",
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
  scale_colour_manual(values = rep('#77777a', 31))

  

# Plot Request 7 ######

# Filter Konya plain for PC1 plot
konyaPC1 <- subset(df2, df2$NGA == 'Konya Plain')

ggplot(data = konyaPC1) +
  aes(x = Time, y = V1) +
  geom_line(data = konyaPC1, size = 2, colour = "black") +
  geom_point(size = 4) +
  labs(x = "Time",
       y = 'PC1') +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80"),
        axis.text.x = element_text(color = "grey10", size = 16, face = "plain"),
        axis.text.y = element_text(color = "grey10", size = 16, face = "plain"),  
        axis.title.x = element_text(color = "grey10", size = 20, face = "italic"),
        axis.title.y = element_text(color = "grey10", size = 20, face = "italic"))


#########################################

##Filter Konya Plain in overall Seshat data - in progress
konya <- subset(df1, df1$NGA == 'Konya Plain')

head(konya)
# Var1 not found ... 
# melt ???

dataplot_konya = as.data.frame(table(konya$Value.From))
str(dataplot_konya)

p2 = ggplot(data=dataplot_konya, aes(x = Var1, y = Freq)) + geom_bar(stat="identity") +
  theme_bw() + labs(x = "Complexity value", y = "Frequency") + ggtitle("Hierarchical Complexity") + theme(plot.title = element_text(hjust = 0.5))
# rotate x axis labels by modifying components of a theme:
# https://ggplot2.tidyverse.org/reference/theme.html
#p2 = ggplot(data=dataplot, aes(x = Var1, y = Freq)) + geom_bar(stat="identity") + ggtitle("BarmoseI.pp - Barchart") +
#  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = NA), panel.grid.major = element_line(colour = "grey90"), axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(x = "Artefact Type", y = "Frequency")
p2

# boxplot / dotplot
ggplot(data = konya, aes(x=as.factor(Variable), y=Value.From)) + geom_boxplot(fill = "lightblue") + labs(x ="Variable", y="") +
  ggtitle("Hierarchical Complexity") +
  theme(axis.text.x = element_text(size=10, colour = "black"), panel.border = element_blank(), plot.title = element_text(size=12, hjust = 0.5))

# dot plot
ggplot(data = konya, aes(x=as.factor(Variable), y=Value.From)) + geom_dotplot(binaxis = "y", stackdir = "center", fill = "lightblue", dotsize = 0.05) +
  ggtitle("Hierarchical Complexity") + theme(axis.text.x = element_text(size=10, colour = "black"), panel.border = element_blank(), plot.title = element_text(size=12, hjust = 0.5)) 


## Change time axis of Konya data - in progress
years <- konya[['Time']]
unique_years <- unique(years)
u_years <- matrix(data = 1:32, nrow = 32)
u_years2 <-  cbind(u_years, unique_years)

shift <- funtion(u_years2, 1){
  c(u_years2[-(seq(1))], rep(NA, 1))
  }

u_years2$unique_years <- shift(u_years2$unique_years, 2)

#  