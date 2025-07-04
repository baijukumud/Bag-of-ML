library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(grid)
library(gridExtra)

grade_input = as.data.frame(read.csv("./grades_km_input.csv"))
kmdata_orig = as.matrix(grade_input[, c ("Student","English","Math","Science")])
kmdata = kmdata_orig[,2:4]
kmdata[1:10,]

wss = numeric(15)

for(k in 1:15) wss[k] = sum(kmeans(kmdata,centers=k,nstart=25)$withinss)
plot(1:15,wss,type="b",xlab="Number of Clusters",ylab="Within sum of square")

# K-means clustering
km <- kmeans(kmdata, centers = 3, nstart = 25)
km
c(wss[3], sum(km$withinss))

# Preparing data
df <- as.data.frame(kmdata_orig[, 2:4])
df$cluster <- factor(km$cluster)
centers <- as.data.frame(km$centers)

# Plot 1: English vs Math
g1 <- ggplot(data = df, aes(x = English, y = Math, color = cluster)) +
  geom_point() +
  theme(legend.position = "right") +
  geom_point(
    data = centers,
    aes(x = English, y = Math, color = as.factor(c(1, 2, 3))),
    size = 10,
    alpha = 0.3,
    show.legend = FALSE
  )

# Plot 2: English vs Science
g2 <- ggplot(data = df, aes(x = English, y = Science, color = cluster)) +
  geom_point() +
  geom_point(
    data = centers,
    aes(x = English, y = Science, color = as.factor(c(1, 2, 3))),
    size = 10,
    alpha = 0.3,
    show.legend = FALSE
  )

# Plot 3: Math vs Science
g3 <- ggplot(data = df, aes(x = Math, y = Science, color = cluster)) +
  geom_point() +
  geom_point(
    data = centers,
    aes(x = Math, y = Science, color = as.factor(c(1, 2, 3))),
    size = 10,
    alpha = 0.3,
    show.legend = FALSE
  )

# Arrange plots
grid.arrange(
  arrangeGrob(
    g1 + theme(legend.position = "none"),
    g2 + theme(legend.position = "none"),
    g3 + theme(legend.position = "none"),
    top = "High School Student Cluster Analysis",
    ncol = 1
  )
)
