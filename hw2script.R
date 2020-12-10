library(tidyverse)
library(plotly)
library(htmltools)
library(reshape)

download.file("https://drive.google.com/u/1/uc?id=1KDhDT0FE5lkjvn62YTCJ87vZ7A5uS5TT&export=download",
              destfile = "./uWaveGestureLibrary_X_TRAIN.txt")
download.file("https://drive.google.com/u/1/uc?id=1fZCNBdJ40Df5werSu_Ud4GUmCBcBIfaI&export=download",
              destfile = "./uWaveGestureLibrary_Y_TRAIN.txt")
download.file("https://drive.google.com/u/1/uc?id=1jdZ2_NiFil0b4EbLBAfDJ43VQcOgulpf&export=download",
              destfile = "./uWaveGestureLibrary_Z_TRAIN.txt")

x <- read.table("uWaveGestureLibrary_X_TRAIN.txt")
y <- read.table("uWaveGestureLibrary_Y_TRAIN.txt")
z<- read.table("uWaveGestureLibrary_Z_TRAIN.txt")

colnames(x)[1] <- "Class"
colnames(y)[1] <- "Class"
colnames(z)[1] <- "Class"


class1 <- cbind(x[x$Class == 1, 2:316], y[y$Class == 1, 2:316], z[z$Class == 1, 2:316])
instance_class1 <- class1[1,]
instance_class1 <- data.frame(t(instance_class1[, 1:315]), t(instance_class1[, 316:630]), t(instance_class1[, 631:945]))
instance_class1 <- data.frame(x = cumsum(instance_class1$X11), y = cumsum(instance_class1$X11.1), z = cumsum(instance_class1$X11.2))
instance_class1 <- data.frame(x = cumsum(instance_class1$x), y = cumsum(instance_class1$y), z = cumsum(instance_class1$z))
fig <- plot_ly(instance_class1, x = ~x, y = ~y, z = ~z)
fig <- div(fig, align = "center")
fig

class2 <- cbind(x[x$Class == 2, 2:316], y[y$Class == 2, 2:316], z[z$Class == 2, 2:316])
instance_class2 <- class2[1,]
instance_class2 <- data.frame(t(instance_class2[, 1:315]), t(instance_class2[, 316:630]), t(instance_class2[, 631:945]))
instance_class2 <- data.frame(x = cumsum(instance_class2$X15), y = cumsum(instance_class2$X15.1), z = cumsum(instance_class2$X15.2))
instance_class2 <- data.frame(x = cumsum(instance_class2$x), y = cumsum(instance_class2$y), z = cumsum(instance_class2$z))
fig <- plot_ly(instance_class2, x = ~x, y = ~y, z = ~z)
fig <- div(fig, align = "center")
fig

class3 <- cbind(x[x$Class == 3, 2:316], y[y$Class == 3, 2:316], z[z$Class == 3, 2:316])
instance_class3 <- class3[1,]
instance_class3 <- data.frame(t(instance_class3[, 1:315]), t(instance_class3[, 316:630]), t(instance_class3[, 631:945]))
instance_class3 <- data.frame(x = cumsum(instance_class3$X4), y = cumsum(instance_class3$X4.1), z = cumsum(instance_class3$X4.2))
instance_class3 <- data.frame(x = cumsum(instance_class3$x), y = cumsum(instance_class3$y), z = cumsum(instance_class3$z))
fig <- plot_ly(instance_class3, x = ~x, y = ~y, z = ~z)
fig <- div(fig, align = "center")
fig

class4 <- cbind(x[x$Class == 4, 2:316], y[y$Class == 4, 2:316], z[z$Class == 4, 2:316])
instance_class4 <- class4[1,]
instance_class4 <- data.frame(t(instance_class4[, 1:315]), t(instance_class4[, 316:630]), t(instance_class4[, 631:945]))
instance_class4 <- data.frame(x = cumsum(instance_class4$X5), y = cumsum(instance_class4$X5.1), z = cumsum(instance_class4$X5.2))
instance_class4 <- data.frame(x = cumsum(instance_class4$x), y = cumsum(instance_class4$y), z = cumsum(instance_class4$z))
fig <- plot_ly(instance_class4, x = ~x, y = ~y, z = ~z)
fig <- div(fig, align = "center")
fig

class5 <- cbind(x[x$Class == 5, 2:316], y[y$Class == 5, 2:316], z[z$Class == 5, 2:316])
instance_class5 <- class5[1,]
instance_class5 <- data.frame(t(instance_class5[, 1:315]), t(instance_class5[, 316:630]), t(instance_class5[, 631:945]))
instance_class5 <- data.frame(x = cumsum(instance_class5$X2), y = cumsum(instance_class5$X2.1), z = cumsum(instance_class5$X2.2))
instance_class5 <- data.frame(x = cumsum(instance_class5$x), y = cumsum(instance_class5$y), z = cumsum(instance_class5$z))
fig <- plot_ly(instance_class5, x = ~x, y = ~y, z = ~z)
fig <- div(fig, align = "center")
fig

class6 <- cbind(x[x$Class == 6, 2:316], y[y$Class == 6, 2:316], z[z$Class == 6, 2:316])
instance_class6 <- class6[1,]
instance_class6 <- data.frame(t(instance_class6[, 1:315]), t(instance_class6[, 316:630]), t(instance_class6[, 631:945]))
instance_class6 <- data.frame(x = cumsum(instance_class6$X1), y = cumsum(instance_class6$X1.1), z = cumsum(instance_class6$X1.2))
instance_class6 <- data.frame(x = cumsum(instance_class6$x), y = cumsum(instance_class6$y), z = cumsum(instance_class6$z))
fig <- plot_ly(instance_class6, x = ~x, y = ~y, z = ~z)
fig <- div(fig, align = "center")
fig

class7 <- cbind(x[x$Class == 7, 2:316], y[y$Class == 7, 2:316], z[z$Class == 7, 2:316])
instance_class7 <- class7[1,]
instance_class7 <- data.frame(t(instance_class7[, 1:315]), t(instance_class7[, 316:630]), t(instance_class7[, 631:945]))
instance_class7 <- data.frame(x = cumsum(instance_class7$X7), y = cumsum(instance_class7$X7.1), z = cumsum(instance_class7$X7.2))
instance_class7 <- data.frame(x = cumsum(instance_class7$x), y = cumsum(instance_class7$y), z = cumsum(instance_class7$z))
fig <- plot_ly(instance_class7, x = ~x, y = ~y, z = ~z)
fig <- div(fig, align = "center")
fig

class8 <- cbind(x[x$Class == 8, 2:316], y[y$Class == 8, 2:316], z[z$Class == 8, 2:316])
instance_class8 <- class8[1,]
instance_class8 <- data.frame(t(instance_class8[, 1:315]), t(instance_class8[, 316:630]), t(instance_class8[, 631:945]))
instance_class8 <- data.frame(x = cumsum(instance_class8$X6), y = cumsum(instance_class8$X6.1), z = cumsum(instance_class8$X6.2))
instance_class8 <- data.frame(x = cumsum(instance_class8$x), y = cumsum(instance_class8$y), z = cumsum(instance_class8$z))
fig <- plot_ly(instance_class8, x = ~x, y = ~y, z = ~z)
fig <- div(fig, align = "center")
fig

x$ID <- c(1:896)
y$ID <- c(1:896)
z$ID <- c(1:896)

x_melt <- melt(x, id = c("ID", "Class"))
y_melt <- melt(y, id = c("ID", "Class"))
z_melt <- melt(z, id = c("ID", "Class"))

all_data <- data.frame(ID = x_melt$ID, Class = x_melt$Class, Variable = x_melt$variable,
                       X = x_melt$value, Y = y_melt$value, Z = z_melt$value)

all_data <- all_data[order(all_data$ID), ]
colnames(all_data)[3] <- "TimeIndex"
all_data$TimeIndex <- rep(1:315, 896)

all_data_pca <- princomp(all_data[, 4:6])
summary(all_data_pca)
all_data_firstpc <- data.frame(ID = all_data$ID, Class = all_data$Class, TimeIndex = all_data$TimeIndex,
                               FirstPC = all_data_pca$scores[,1])

class1_pc <- data.frame(TimeIndex = c(1:315), Observation11 = all_data_firstpc$FirstPC[all_data$ID == 11],
                        Observation17 = all_data_firstpc$FirstPC[all_data$ID == 17])
g <- ggplot(class1_pc) +
  geom_line(aes(x = TimeIndex, y = Observation11), color = "red") +
  geom_line(aes(x = TimeIndex, y = Observation17), color = "black") +
  ylab("Observations")
g <- ggplotly(g)
g <- div(g, align = "center")
g

class2_pc <- data.frame(TimeIndex = c(1:315), Observation29 = all_data_firstpc$FirstPC[all_data$ID == 29],
                        Observation31 = all_data_firstpc$FirstPC[all_data$ID == 31])
g <- ggplot(class2_pc) +
  geom_line(aes(x = TimeIndex, y = Observation29), color = "red") +
  geom_line(aes(x = TimeIndex, y = Observation31), color = "black") +
  ylab("Observations")
g <- ggplotly(g)
g <- div(g, align = "center")
g

class3_pc <- data.frame(TimeIndex = c(1:315), Observation27 = all_data_firstpc$FirstPC[all_data$ID == 27],
                        Observation61 = all_data_firstpc$FirstPC[all_data$ID == 61])
g <- ggplot(class3_pc) +
  geom_line(aes(x = TimeIndex, y = Observation27), color = "red") +
  geom_line(aes(x = TimeIndex, y = Observation61), color = "black") +
  ylab("Observations")
g <- ggplotly(g)
g <- div(g, align = "center")
g

class4_pc <- data.frame(TimeIndex = c(1:315), Observation5 = all_data_firstpc$FirstPC[all_data$ID == 5],
                        Observation8 = all_data_firstpc$FirstPC[all_data$ID == 8])
g <- ggplot(class4_pc) +
  geom_line(aes(x = TimeIndex, y = Observation5), color = "red") +
  geom_line(aes(x = TimeIndex, y = Observation8), color = "black") +
  ylab("Observations")
g <- ggplotly(g)
g <- div(g, align = "center")
g

class5_pc <- data.frame(TimeIndex = c(1:315), Observation35 = all_data_firstpc$FirstPC[all_data$ID == 35],
                        Observation41 = all_data_firstpc$FirstPC[all_data$ID == 41])
g <- ggplot(class5_pc) +
  geom_line(aes(x = TimeIndex, y = Observation35), color = "red") +
  geom_line(aes(x = TimeIndex, y = Observation41), color = "black") +
  ylab("Observations")
g <- ggplotly(g)
g <- div(g, align = "center")
g

class6_pc <- data.frame(TimeIndex = c(1:315), Observation1 = all_data_firstpc$FirstPC[all_data$ID == 1],
                        Observation10 = all_data_firstpc$FirstPC[all_data$ID == 10])
g <- ggplot(class6_pc) +
  geom_line(aes(x = TimeIndex, y = Observation1), color = "red") +
  geom_line(aes(x = TimeIndex, y = Observation10), color = "black") +
  ylab("Observations")
g <- ggplotly(g)
g <- div(g, align = "center")
g

class7_pc <- data.frame(TimeIndex = c(1:315), Observation7 = all_data_firstpc$FirstPC[all_data$ID == 7],
                        Observation12 = all_data_firstpc$FirstPC[all_data$ID == 12])
g <- ggplot(class7_pc) +
  geom_line(aes(x = TimeIndex, y = Observation7), color = "red") +
  geom_line(aes(x = TimeIndex, y = Observation12), color = "black") +
  ylab("Observations")
g <- ggplotly(g)
g <- div(g, align = "center")
g

class8_pc <- data.frame(TimeIndex = c(1:315), Observation40 = all_data_firstpc$FirstPC[all_data$ID == 40],
                        Observation48 = all_data_firstpc$FirstPC[all_data$ID == 48])
g <- ggplot(class8_pc) +
  geom_line(aes(x = TimeIndex, y = Observation40), color = "red") +
  geom_line(aes(x = TimeIndex, y = Observation48), color = "black") +
  ylab("Observations")
g <- ggplotly(g)
g <- div(g, align = "center")
g

split_data <- split(all_data, all_data$Class)
split_data_pca <- map(split_data, ~princomp(.[,4:6]))
map(split_data_pca, summary)

x_distance <- dist(x[, 2:316], method = "euclidean")
y_distance <- dist(y[, 2:316], method = "euclidean")
z_distance <- dist(z[, 2:316], method = "euclidean")

total_distance <- x_distance + y_distance + z_distance
mds2d <- cmdscale(total_distance, k = 2)
scaled_data <- as.data.frame(mds2d)
scaled_data$Class <- x$Class

g <- ggplot(scaled_data, aes(x = V1, y = V2)) +
  geom_point(aes(color = factor(Class)))
g <- ggplotly(g)
g <- div(g, align = "center")
g