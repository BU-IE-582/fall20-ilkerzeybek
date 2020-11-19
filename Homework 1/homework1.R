library(ggplot2)
library(plotly)
library(dplyr)

url1 <- "https://www.football-data.co.uk/mmz4281/2021/E0.csv"
url2 <- "https://www.football-data.co.uk/mmz4281/1920/E0.csv"
url3 <- "https://www.football-data.co.uk/mmz4281/1819/E0.csv"

if(!file.exists("./data")){
  dir.create("./data")
}

download.file(url1, destfile = "./data/20-21.csv")
download.file(url2, destfile = "./data/19-20.csv")
download.file(url3, destfile = "./data/18-19.csv")

data2021 <- read.csv("./data/20-21.csv")
data1920 <- read.csv("./data/19-20.csv")
data1819 <- read.csv("./data/18-19.csv")

merged_data <- bind_rows(data1819, data1920, data2021)

# TASK 1
# Part 1
# Home Goals Histogram
g <- ggplot(merged_data, aes(FTHG)) +
  geom_bar(col = "black", fill = "pink") +
  ylab("Number of Games") +
  xlab("Home Goals") +
  labs(title = "Histogram of Home Goals") +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
g <- ggplotly(g)
g
# Away Goals Histogram
g <- ggplot(merged_data, aes(FTAG)) +
  geom_bar(col = "black", fill = "pink") +
  ylab("Number of Games") +
  xlab("Away Goals") +
  labs(title = "Histogram of Away Goals") +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
g <- ggplotly(g)
g
# Home Goals - Away Goals
g <- ggplot(merged_data, aes(FTHG-FTAG)) +
  geom_bar(col = "black", fill = "pink") +
  ylab("Number of Games") +
  xlab("Home Goals - Away Goals") +
  labs(title = "Histogram of Home Goals - Away Goals") +
  scale_x_continuous(breaks = seq(-9, 9, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
g <- ggplotly(g)
g
# Part 2 - Fit a distribution
lambda_home <- mean(merged_data$FTHG, na.rm = T)
lambda_home
lambda_away <- mean(merged_data$FTAG, na.rm = T)
lambda_away

g <- ggplot(merged_data, aes(FTHG)) +
  geom_bar(col = "black", fill = "pink") +
  geom_line(aes(y = dpois(FTHG, lambda = mean(FTHG, na.rm = T)) * length(FTHG))) +
  ylab("Number of Games") +
  xlab("Home Goals") +
  labs(title = "Histogram of Home Goals with Fitted Poisson Distribution") +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
g <- ggplotly(g)
g

g <- ggplot(merged_data, aes(FTAG)) +
  geom_bar(col = "black", fill = "pink") +
  geom_line(aes(y = dpois(FTAG, lambda = mean(FTAG, na.rm = T)) * length(FTAG))) +
  ylab("Number of Games") +
  xlab("Away Goals") +
  labs(title = "Histogram of Away Goals with Fitted Poisson Distribution") +
  scale_x_continuous(breaks = seq(0, 9, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
g <- ggplotly(g)
g
# TASK 2
# Part 1
filtered_data <- merged_data[,1:35]

b365 <- with(filtered_data, data.frame(HomeTeam, AwayTeam, FTHG, FTAG, FTR, HR, AR, Bookie = "Bet365",
                                       HomeOdd = B365H, DrawOdd = B365D, AwayOdd = B365A,
                                       HomeProbability = 1/B365H,
                                       DrawProbability = 1/B365D,
                                       AwayProbability = 1/B365A,
                                       TotalProbability = 1/B365H + 1/B365D + 1/B365A))

bw <- with(filtered_data, data.frame(HomeTeam, AwayTeam, FTHG, FTAG, FTR, HR, AR, Bookie = "BetAndWin",
                                     HomeOdd = BWH, DrawOdd = BWD, AwayOdd = BWA,
                                     HomeProbability = 1/BWH,
                                     DrawProbability = 1/BWD,
                                     AwayProbability = 1/BWA,
                                     TotalProbability = 1/BWH + 1/BWD + 1/BWA))

iw <- with(filtered_data, data.frame(HomeTeam, AwayTeam, FTHG, FTAG, FTR, HR, AR, Bookie = "Interwetten",
                                     HomeOdd = IWH, DrawOdd = IWD, AwayOdd = IWA,
                                     HomeProbability = 1/IWH,
                                     DrawProbability = 1/IWD,
                                     AwayProbability = 1/IWA,
                                     TotalProbability = 1/IWH + 1/IWD + 1/IWA))

ps <- with(filtered_data, data.frame(HomeTeam, AwayTeam, FTHG, FTAG, FTR, HR, AR, Bookie = "Pinnacle",
                                     HomeOdd = PSH, DrawOdd = PSD, AwayOdd = PSA,
                                     HomeProbability = 1/PSH,
                                     DrawProbability = 1/PSD,
                                     AwayProbability = 1/PSA,
                                     TotalProbability = 1/PSH + 1/PSD + 1/PSA))

bookie_data <- bind_rows(b365, bw, iw, ps)

# Normalization of probabilities
bookie_data$HomeProbabilityN <- bookie_data$HomeProbability / bookie_data$TotalProbability
bookie_data$DrawProbabilityN <- bookie_data$DrawProbability / bookie_data$TotalProbability
bookie_data$AwayProbabilityN <- bookie_data$AwayProbability / bookie_data$TotalProbability
bookie_data$TotalProbabilityN <- bookie_data$HomeProbabilityN + bookie_data$DrawProbabilityN + bookie_data$AwayProbabilityN
# Plotting of P(Home Win) - P(Away Win) vs P(Draw)
g <- ggplot(bookie_data, aes(x = HomeProbabilityN - AwayProbabilityN, y = DrawProbabilityN)) +
  geom_point(aes(col = Bookie)) + 
  facet_wrap(~Bookie) +
  ylab("P(Draw)") +
  xlab("P(Home Win) - P(Away Win)") +
  labs(title = "P(Home Win) - P(Away Win) vs P(Draw)") +
  theme(plot.title = element_text(hjust = 0.5))
g <- ggplotly(g, height = 500)
g

totalbins_b365 <- with(bookie_data[bookie_data$Bookie == "Bet365",],
                       cut(HomeProbabilityN - AwayProbabilityN,
                           breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                             seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
drawbins_b365 <- with(bookie_data[bookie_data$Bookie == "Bet365" & bookie_data$FTR == "D",],
                      cut(HomeProbabilityN - AwayProbabilityN,
                          breaks = unique(c(seq(-0.85, -0.2, 0.15), seq(-0.2, 0.2, 0.05),
                                            seq(0.2, 1, 0.15)))))
total_b365 <- as.numeric(table(totalbins_b365))
draw_b365 <- as.numeric(table(drawbins_b365))
estimated_draw_b365 <- draw_b365 / total_b365

totalbins_bw <- with(bookie_data[bookie_data$Bookie == "BetAndWin",],
                     cut(HomeProbabilityN - AwayProbabilityN,
                         breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                           seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
drawbins_bw <- with(bookie_data[bookie_data$Bookie == "BetAndWin" & bookie_data$FTR == "D",],
                    cut(HomeProbabilityN - AwayProbabilityN,
                        breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                          seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
total_bw <- as.numeric(table(totalbins_bw))
draw_bw <- as.numeric(table(drawbins_bw))
estimated_draw_bw <- draw_bw / total_bw

totalbins_iw <- with(bookie_data[bookie_data$Bookie == "Interwetten",],
                     cut(HomeProbabilityN - AwayProbabilityN,
                         breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                           seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
drawbins_iw <- with(bookie_data[bookie_data$Bookie == "Interwetten" & bookie_data$FTR == "D",],
                    cut(HomeProbabilityN - AwayProbabilityN,
                        breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                          seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
total_iw <- as.numeric(table(totalbins_iw))
draw_iw <- as.numeric(table(drawbins_iw))
estimated_draw_iw <- draw_iw / total_iw

totalbins_ps <- with(bookie_data[bookie_data$Bookie == "Pinnacle",],
                     cut(HomeProbabilityN - AwayProbabilityN,
                         breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                           seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
drawbins_ps <- with(bookie_data[bookie_data$Bookie == "Pinnacle" & bookie_data$FTR == "D",],
                    cut(HomeProbabilityN - AwayProbabilityN,
                        breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                          seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
total_ps <- as.numeric(table(totalbins_ps))
draw_ps <- as.numeric(table(drawbins_ps))
estimated_draw_ps <- draw_ps / total_ps

estimated <- data.frame(drawprob = c(estimated_draw_b365, estimated_draw_bw,
                                     estimated_draw_iw, estimated_draw_ps),
                        midpoint = rep(c(-0.775, -0.625, -0.475, -0.325, -0.225,
                                         -0.175, -0.125, -0.075, -0.025,
                                         0.025, 0.075, 0.125, 0.175, 0.275, 0.425,
                                         0.575, 0.725, 0.875), 4),
                        Bookie = rep(c("Bet365", "BetAndWin", "Interwetten", "Pinnacle"),
                                     c(18, 18, 18, 18)))

g <- ggplot(bookie_data, aes(x = HomeProbabilityN - AwayProbabilityN, y = DrawProbabilityN, col = Bookie)) +
  geom_point() +
  geom_point(estimated, mapping = aes(x = midpoint, y = drawprob), col = "black") +
  facet_wrap(~Bookie) +
  ylab("P(Draw)") +
  xlab("P(Home Win) - P(Away Win)") +
  labs(title = "P(Home Win) - P(Away Win) vs P(Draw) with Estimated Probabilities") +
  theme(plot.title = element_text(hjust = 0.5))
g <- ggplotly(g, height = 500)
g

# TASK 3

totalbins3_b365 <- with(bookie_data[bookie_data$Bookie == "Bet365" & bookie_data$HR == 0 & bookie_data$AR == 0,],
                        cut(HomeProbabilityN - AwayProbabilityN,
                            breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                              seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
drawbins3_b365 <- with(bookie_data[bookie_data$Bookie == "Bet365" & bookie_data$FTR == "D" & bookie_data$HR == 0 & bookie_data$AR == 0,],
                       cut(HomeProbabilityN - AwayProbabilityN,
                           breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                             seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
total3_b365 <- as.numeric(table(totalbins3_b365))
draw3_b365 <- as.numeric(table(drawbins3_b365))
estimated_draw3_b365 <- draw3_b365 / total3_b365

totalbins3_bw <- with(bookie_data[bookie_data$Bookie == "BetAndWin" & bookie_data$HR == 0 & bookie_data$AR == 0,],
                      cut(HomeProbabilityN - AwayProbabilityN,
                          breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                            seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
drawbins3_bw <- with(bookie_data[bookie_data$Bookie == "BetAndWin" & bookie_data$FTR == "D" & bookie_data$HR == 0 & bookie_data$AR == 0,],
                     cut(HomeProbabilityN - AwayProbabilityN,
                         breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                           seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
total3_bw <- as.numeric(table(totalbins3_bw))
draw3_bw <- as.numeric(table(drawbins3_bw))
estimated_draw3_bw <- draw3_bw / total3_bw

totalbins3_iw <- with(bookie_data[bookie_data$Bookie == "Interwetten" & bookie_data$HR == 0 & bookie_data$AR == 0,],
                      cut(HomeProbabilityN - AwayProbabilityN,
                          breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                            seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
drawbins3_iw <- with(bookie_data[bookie_data$Bookie == "Interwetten" & bookie_data$FTR == "D" & bookie_data$HR == 0 & bookie_data$AR == 0,],
                     cut(HomeProbabilityN - AwayProbabilityN,
                         breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                           seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
total3_iw <- as.numeric(table(totalbins3_iw))
draw3_iw <- as.numeric(table(drawbins3_iw))
estimated_draw3_iw <- draw3_iw / total3_iw

totalbins3_ps <- with(bookie_data[bookie_data$Bookie == "Pinnacle" & bookie_data$HR == 0 & bookie_data$AR == 0,],
                      cut(HomeProbabilityN - AwayProbabilityN,
                          breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                            seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
drawbins3_ps <- with(bookie_data[bookie_data$Bookie == "Pinnacle" & bookie_data$FTR == "D" & bookie_data$HR == 0 & bookie_data$AR == 0,],
                     cut(HomeProbabilityN - AwayProbabilityN,
                         breaks = unique(c(seq(-0.85, -0.2, 0.15),
                                           seq(-0.2, 0.2, 0.05), seq(0.2, 1, 0.15)))))
total3_ps <- as.numeric(table(totalbins3_ps))
draw3_ps <- as.numeric(table(drawbins3_ps))
estimated_draw3_ps <- draw3_ps / total3_ps

estimated3 <- data.frame(drawprob = c(estimated_draw3_b365, estimated_draw3_bw,
                                      estimated_draw3_iw, estimated_draw3_ps),
                         midpoint = rep(c(-0.775, -0.625, -0.475, -0.325, -0.225,
                                          -0.175, -0.125, -0.075, -0.025,
                                          0.025, 0.075, 0.125, 0.175, 0.275, 0.425,
                                          0.575, 0.725, 0.875), 4),
                         Bookie = rep(c("Bet365", "BetAndWin", "Interwetten", "Pinnacle"),
                                      c(18, 18, 18, 18)))
estimated <- bind_rows(estimated, estimated3)
estimated$RedCards <- rep(c("Included", "Not Included"), c(72,72))

g <- ggplot() +
  geom_point(bookie_data, mapping = aes(x = HomeProbabilityN - AwayProbabilityN, y = DrawProbabilityN, col = Bookie)) +
  geom_point(estimated, mapping = aes(x = midpoint, y = drawprob, shape = RedCards)) +
  facet_wrap(~Bookie) +
  ylab("P(Draw)") +
  xlab("P(Home Win) - P(Away Win)") +
  labs(title = "P(Home Win) - P(Away Win) vs P(Draw)") +
  theme(plot.title = element_text(hjust = 0.5))
g <- ggplotly(g, height = 500)
g
