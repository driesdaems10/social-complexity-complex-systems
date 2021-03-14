# plot complexity tables

rm(list = ls())

library(readxl)
library(ggplot2)
library(reshape2)
library(ggpubr)

setwd("")

df = read_excel("complexity-tables.xlsx", sheet = "data_twb")
df = df[,c("PeriodMod", "Period", "AC", "Flows", "Drivers", "Mechanism")]

df$PeriodMod = as.factor(df$PeriodMod)
df$PeriodMod = ordered(df$PeriodMod, levels = c("LCH","EBA","MBA/LBA","IA","Hell"))


# Flows ########
df_plot = df[c(1,4)]
df_plot = table(df_plot)
df1 = melt(as.matrix(df_plot))
tabSums = as.data.frame(rowSums(df_plot))
tabSums$PeriodMod = rownames(tabSums)
names(tabSums)[1] = "Sums"
df1 = merge(df1,tabSums, by = "PeriodMod")
df1$Freq = paste0(round(df1$value/df1$Sums,3)*100,"%")
  
g1 = ggplot(data = df1) +
  aes(x = PeriodMod, y = value, fill = Flows, label = Freq) +
  geom_bar(stat = "identity", position = "fill", width = 0.95) + 
  geom_label(position = position_fill(vjust = 0.5), colour = "#e5e6d8", label.size = 0, size = 6, aes(x = PeriodMod, y = value, fill = Flows, label = Freq)) +
  labs(x = "", y = '') +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.text=element_text(size = 16, face = "italic"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "grey10"),
        legend.key.width = unit(1,"cm"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        axis.text.x = element_text(color = "grey10", size = 16, face = "italic", margin=margin(t=+1)),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),
        axis.ticks.length=unit(0.01, "cm")) +
  scale_fill_manual(values = c("#808080", "#404040", "#1a1919"))

# Drivers ########
df_plot = df[c(1,5)]
df_plot = table(df_plot)
df1 = melt(as.matrix(df_plot))
tabSums = as.data.frame(rowSums(df_plot))
tabSums$PeriodMod = rownames(tabSums)
names(tabSums)[1] = "Sums"
df1 = merge(df1,tabSums, by = "PeriodMod")
df1$Freq = paste0(round(df1$value/df1$Sums,3)*100,"%")

g2 = ggplot(data = df1) +
  aes(x = PeriodMod, y = value, fill = Drivers, label = Freq) +
  geom_bar(stat = "identity", position = "fill", width = 0.95) + 
  geom_label(position = position_fill(vjust = 0.5), colour = "#e5e6d8", label.size = 0, size = 6, aes(x = PeriodMod, y = value, fill = Drivers, label = Freq)) +
  labs(x = "", y = '') +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.text=element_text(size = 16, face = "italic"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "grey10"),
        legend.key.width = unit(1,"cm"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        axis.text.x = element_text(color = "grey10", size = 16, face = "italic", margin=margin(t=+1)),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),
        axis.ticks.length=unit(0.01, "cm")) +
  scale_fill_manual(values = c("#808080", "#404040"))



# Mechanisms ########

df_plot = df[c(1,6)]
df_plot = table(df_plot)
df1 = melt(as.matrix(df_plot))
tabSums = as.data.frame(rowSums(df_plot))
tabSums$PeriodMod = rownames(tabSums)
names(tabSums)[1] = "Sums"
df1 = merge(df1,tabSums, by = "PeriodMod")
df1$Freq = paste0(round(df1$value/df1$Sums,3)*100,"%")

g3 = ggplot(data = df1) +
  aes(x = PeriodMod, y = value, fill = Mechanism, label = Freq) +
  geom_bar(stat = "identity", position = "fill", width = 0.95) + 
  geom_label(position = position_fill(vjust = 0.5), colour = "#e5e6d8", label.size = 0, size = 6, aes(x = PeriodMod, y = value, fill = Mechanism, label = Freq)) +
  labs(x = "", y = '') +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.text=element_text(size = 16, face = "italic"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "grey10"),
        legend.key.width = unit(1,"cm"),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        axis.text.x = element_text(color = "grey10", size = 16, face = "italic", margin=margin(t=+1)),
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),
        axis.ticks.length=unit(0.01, "cm")) +
  scale_fill_manual(values = c("#808080", "#404040", "#1a1919"))


# Arrange figures ######
figure = ggarrange(g1, g2, g3, ncol = 1, nrow = 3)
figure



