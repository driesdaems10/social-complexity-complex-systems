# Plots for simulated data
rm(list = ls())

library(ggplot2)
library(reshape2)


# Figure 2.2 #######

# small world network
library(igraph)
set.seed(9191)
g = sample_smallworld(1, 80, 5, 0.05, loops = FALSE, multiple = FALSE) # rewining prob = 0.3?
# The small world property (high local clustering and short paths) emerges for a small rewiring probability p ranging from 0.001 to 0.1
mean_distance(g)
transitivity(g, type="average")
plot(g, layout=layout_in_circle, vertex.size=4, 
     vertex.label=NA, edge.arrow.size=0, vertex.color="black")


# Figure 2.3 #######

# scale free network according to the Barabasi-Albert model
library(igraph)
par(mfrow = c(1,1))
set.seed(9019)
g = barabasi.game(200, power = 1)
plot(degree.distribution(g), type = "h")
plot(g, layout=layout.fruchterman.reingold, vertex.size=2, 
     vertex.label=NA, edge.arrow.size=0, vertex.color="black")


# Figure 2.4 #######

rank = 1:200
rank = (rank - min(rank)) / (max(rank) - min(rank))
# lognormal
lognormal = 1 - rank
lognormal = (lognormal - min(lognormal)) / (max(lognormal) - min(lognormal))
plot(rank, lognormal, type = "l")
# primate
pr1 = 1 - 0.8*rank[round(seq(1, 200, 2.5))]
primate = c(pr1, min(pr1), seq(0, min(pr1), 0.00176)[119:1])
primate = (primate - min(primate)) / (max(primate) - min(primate))
plot(rank, primate, type = "l")
# convex
set.seed(1452)
xx = rnorm(200)
convex = 1 - xx^2
convex = convex[order(convex, decreasing = TRUE)]
convex = (convex - min(convex)) / (max(convex) - min(convex))
plot(rank, convex, type = "l")

df = cbind.data.frame(convex, lognormal, primate)
df_plot = melt(df)
df_plot$VarX = rep(rank, 3)
head(df_plot)

ggplot(data = df_plot) +
  aes(x = VarX, y = value, linetype = factor(variable)) +
  geom_line(colour = "#0a0808", size = 2) +
  labs(x = "log(rank)",
       y = 'log(size)') +
  theme(legend.position = c(0.2, 0.15),
        legend.direction = "vertical",
        legend.text=element_text(size = 20, face = "italic"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "grey10"),
        legend.key.width = unit(2.2,"cm"),
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
  scale_linetype_manual(labels = c("convex", "log-normal", "primate"), values = c("dotted", "solid", "dashed"))



# Figure 2.5 #######

par(mfrow = c(1,3))
set.seed(1451)
logx = log(rnorm(1000, 5, 1))
logy = logx
logy2 = 1.15*logx
logy3 = 0.85*logx

df = cbind.data.frame(logy, logy2, logy3)
df_plot = melt(df)
df_plot$VarX = rep(logx, 3)
head(df_plot)

ggplot(data = df_plot) +
  aes(x = VarX, y = value, linetype = factor(variable)) +
  geom_smooth(method='lm', formula= y~x, se = FALSE, colour = "#0a0808", size = 2) +
  labs(x = "log(x)",
       y = 'log(y)') +
  theme(legend.position = c(0.2, 0.84),
        legend.direction = "vertical",
        legend.text=element_text(size = 20, face = "italic"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "grey10"),
        legend.key.width = unit(2.2,"cm"),
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
  scale_linetype_manual(labels = c("linear", "superlinear", "sublinear"), values=c("solid", "dashed", "dotted"))

