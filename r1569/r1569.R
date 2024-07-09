rm(list = ls())
library(tidyverse)
SimilarLearn <- read_csv("240705_r1569_0_similar_learn.csv")
SimilarLearn <- SimilarLearn %>%
  select(!15:24) %>%
  filter(Value.Item1.Item2>260) %>%
  rename(pixelx = Value.Item1.Item3.X, pixely = Value.Item1.Item3.Y, TimePoint = Value.Item1.Item2) %>%
  select(!1, !14)
x1 <- c(SimilarLearn$pixelx)
y1 <- c(SimilarLearn$pixely)
df1 <- data.frame(x = x1, y = y1)
p1 <- ggplot(df1, aes(x = x, y = y)) +
  geom_point(alpha = 0.3)
p1
library(ggforce)
p1circle <- p1 + geom_circle(aes(x0 = 324, y0 = 715, r = 125), color = "blue")
p1circle <- p1circle + geom_circle(aes(x0 = 787, y0 = 271, r = 125), color = "blue")
p1circle <- p1circle +
  ggtitle("similar")
p1circle
radius <- 125
center_x1 <- 324
center_y1 <- 715
df1$distance <- sqrt((df1$x - center_x1)^2 + (df1$y - center_y1)^2)
points_in_circle1 <- sum(df1$distance <= radius)
print(points_in_circle1)
center_x2 <- 787
center_y2 <- 271
df1$distance1 <- sqrt((df1$x - center_x2)^2 + (df1$y - center_y2)^2)
points_in_circle2 <- sum(df1$distance1 <= radius)
print(points_in_circle2)

BaselineLearn <- read_csv("240705_r1569_1_baseline_learn.csv")
BaselineLearn <- BaselineLearn %>%
  select(!15:24) %>%
  filter(Value.Item1.Item2>260) %>%
  rename(pixelx = Value.Item1.Item3.X, pixely = Value.Item1.Item3.Y, TimePoint = Value.Item1.Item2) %>%
  select(!1, !14)
x2 <- c(BaselineLearn$pixelx)
y2 <- c(BaselineLearn$pixely)
df2 <- data.frame(x = x2, y = y2)
p2 <- ggplot(df2, aes(x = x2, y = y2)) +
  geom_point(alpha = 0.3)
p2
p2circle <- p2 + geom_circle(aes(x0 = 324, y0 = 710, r = 125), color = "red")
p2circle <- p2circle + geom_circle(aes(x0 = 788, y0 = 266, r = 125), color = "red")
p2circle <- p2circle +
  ggtitle("baseline")
p2circle
radius <- 125
center_x1 <- 324
center_y1 <- 710
df2$distance <- sqrt((df2$x - center_x1)^2 + (df2$y - center_y1)^2)
points_in_circle2 <- sum(df2$distance <= radius)
print(points_in_circle2)
center_x2 <- 788
center_y2 <- 266
df2$distance1 <- sqrt((df2$x - center_x2)^2 + (df2$y - center_y2)^2)
points_in_circle2 <- sum(df2$distance1 <= radius)
print(points_in_circle2)

BaselineTest <- read_csv("240705_r1569_2_baseline_test.csv")
BaselineTest <- BaselineTest %>%
  select(!15:24) %>%
  filter(Value.Item1.Item2>260) %>%
  rename(pixelx = Value.Item1.Item3.X, pixely = Value.Item1.Item3.Y, TimePoint = Value.Item1.Item2) %>%
  select(!1, !14)
x3 <- c(BaselineTest$pixelx)
y3 <- c(BaselineTest$pixely)
df3 <- data.frame(x = x3, y = y3)
p3 <- ggplot(df3, aes(x = x3, y = y3)) +
  geom_point(alpha = 0.3)
p3
p3circle <- p3 + geom_circle(aes(x0 = 324, y0 = 705, r = 125), color = "red")
p3circle <- p3circle + geom_circle(aes(x0 = 788, y0 = 271, r = 125), color = "blue")
p3circle <- p3circle +
  ggtitle("baseline")
p3circle
radius <- 125
center_x3 <- 324
center_y3 <- 705
df3$distance <- sqrt((df3$x - center_x3)^2 + (df3$y - center_y3)^2)
points_in_circle3 <- sum(df3$distance <= radius)
print(points_in_circle3)
center_x3 <- 788
center_y3 <- 271
df3$distance1 <- sqrt((df3$x - center_x3)^2 + (df3$y - center_y3)^2)
points_in_circle3 <- sum(df1$distance1 <= radius)
print(points_in_circle3)

BaselineLearn1 <- read_csv("240705_r1569_3_baseline_learn.csv")
BaselineLearn1 <- BaselineLearn1 %>%
  select(!15:24) %>%
  filter(Value.Item1.Item2>260) %>%
  rename(pixelx = Value.Item1.Item3.X, pixely = Value.Item1.Item3.Y, TimePoint = Value.Item1.Item2) %>%
  select(!1, !14)
x4 <- c(BaselineLearn1$pixelx)
y4 <- c(BaselineLearn1$pixely)
df4 <- data.frame(x = x4, y = y4)
p4 <- ggplot(df4, aes(x = x4, y = y4)) +
  geom_point(alpha = 0.3)
p4
p4circle <- p4 + geom_circle(aes(x0 = 319, y0 = 705, r = 125), color = "green")
p4circle <- p4circle + geom_circle(aes(x0 = 793, y0 = 266, r = 125), color = "green")
p4circle <- p4circle +
  ggtitle("baseline")
p4circle
radius <- 125
center_x4 <- 319
center_y4 <- 705
df4$distance <- sqrt((df4$x - center_x4)^2 + (df4$y - center_y4)^2)
points_in_circle4 <- sum(df4$distance <= radius)
print(points_in_circle4)
center_x4 <- 793
center_y4 <- 266
df4$distance1 <- sqrt((df4$x - center_x4)^2 + (df4$y - center_y4)^2)
points_in_circle4 <- sum(df4$distance1 <= radius)
print(points_in_circle4)

DifferentLearn <- read_csv("240705_r1569_4_different_learn.csv")
DifferentLearn <- DifferentLearn %>%
  select(!15:24) %>%
  filter(Value.Item1.Item2>260) %>%
  rename(pixelx = Value.Item1.Item3.X, pixely = Value.Item1.Item3.Y, TimePoint = Value.Item1.Item2) %>%
  select(!1, !14)
x5 <- c(DifferentLearn$pixelx)
y5 <- c(DifferentLearn$pixely)
df5 <- data.frame(x = x5, y = y5)
p5 <- ggplot(df5, aes(x = x5, y = y5)) +
  geom_point(alpha = 0.3)
p5
p5circle <- p5 + geom_circle(aes(x0 = 452, y0 = 989, r = 125), color = "purple")
p5circle <- p5circle + geom_circle(aes(x0 = 1030, y0 = 452, r = 125), color = "purple")
p5circle <- p5circle +
  ggtitle("different")
p5circle
radius <- 125
center_x5 <- 452
center_y5 <- 989
df5$distance <- sqrt((df5$x - center_x5)^2 + (df5$y - center_y5)^2)
points_in_circle5 <- sum(df5$distance <= radius)
print(points_in_circle5)
center_x6 <- 1030
center_y6 <- 452
df5$distance1 <- sqrt((df5$x - center_x6)^2 + (df5$y - center_y6)^2)
points_in_circle6 <- sum(df5$distance1 <= radius)
print(points_in_circle6)

DifferentTest <- read_csv("240705_r1569_5_different_test.csv")
DifferentTest <- DifferentTest %>%
  select(!15:24) %>%
  filter(Value.Item1.Item2>260) %>%
  rename(pixelx = Value.Item1.Item3.X, pixely = Value.Item1.Item3.Y, TimePoint = Value.Item1.Item2) %>%
  select(!1, !14)
x6 <- c(DifferentTest$pixelx)
y6 <- c(DifferentTest$pixely)
df6 <- data.frame(x = x6, y = y6)
p6 <- ggplot(df6, aes(x = x6, y = y6)) +
  geom_point(alpha = 0.3)
p6
p6circle <- p6 + geom_circle(aes(x0 = 452, y0 = 989, r = 125), color = "purple")
p6circle <- p6circle + geom_circle(aes(x0 = 1035, y0 = 452, r = 125), color = "green")
p6circle <- p6circle +
  ggtitle("different")
p6circle
radius <- 125
center_x6 <- 452
center_y6 <- 989
df6$distance <- sqrt((df6$x - center_x6)^2 + (df6$y - center_y6)^2)
points_in_circle6 <- sum(df6$distance <= radius)
print(points_in_circle6)
center_x6 <- 1030
center_y6 <- 452
df6$distance1 <- sqrt((df6$x - center_x6)^2 + (df6$y - center_y6)^2)
points_in_circle6 <- sum(df6$distance1 <= radius)
print(points_in_circle6)
library(gridExtra)
grid.arrange(p1circle, p2circle, p3circle, p4circle, p5circle, p6circle, nrow = 2, ncol = 3)

