rm(list = ls())
library(tidyverse)
SimilarLearn <- read_csv("240808_r1592_0_baseline_learn.csv")
SimilarLearn <- SimilarLearn %>%
  select(!15:24) %>%
  filter(Value.Item1.Item1.Item2>109) %>%
  rename(pixelx = Value.Item1.Item1.Item3.X, pixely = Value.Item1.Item1.Item3.Y, TimePoint = Value.Item1.Item1.Item2) %>%
  select(!1, !14) %>%
  filter(!Value.Item2 == TRUE) %>%
  na.omit 
x1 <- c(SimilarLearn$pixelx)
y1 <- c(SimilarLearn$pixely)
df1 <- data.frame(x = x1, y = y1)
p1 <- ggplot(df1, aes(x = x, y = y)) +
  geom_point(alpha = 0.3)
p1
library(ggforce)
#Blue represents gnome, red represents green jar.
p1circle <- p1 + geom_circle(aes(x0 = 381, y0 = 175, r = 60), color = "purple")
p1circle <- p1circle + geom_circle(aes(x0 = 130, y0 = 393, r = 60), color = "purple")
p1circle
radius <- 60
center_x1 <- 381
center_y1 <- 175
df1$distance <- sqrt((df1$x - center_x1)^2 + (df1$y - center_y1)^2)
points_in_circle1 <- sum(df1$distance <= radius)
print(points_in_circle1)
center_x2 <- 130
center_y2 <- 393
df1$distance1 <- sqrt((df1$x - center_x2)^2 + (df1$y - center_y2)^2)
points_in_circle2 <- sum(df1$distance1 <= radius)
print(points_in_circle2)

BaselineLearn <- read_csv("240808_r1592_different_1.csv")
BaselineLearn <- BaselineLearn %>%
  select(!15:24) %>%
  filter(Value.Item1.Item1.Item2>193) %>%
  rename(pixelx = Value.Item1.Item1.Item3.X, pixely = Value.Item1.Item1.Item3.Y, TimePoint = Value.Item1.Item1.Item2) %>%
  select(!1, !14) %>%
  na.omit
Baselinelearn <- BaselineLearn %>%
  filter(!Value.Item2 == TRUE) 
x2 <- c(BaselineLearn$pixelx)
y2 <- c(BaselineLearn$pixely)
df2 <- data.frame(x = x2, y = y2)
p2 <- ggplot(df2, aes(x = x2, y = y2)) +
  geom_point(alpha = 0.3)
p2
p2circle <- p2 + geom_circle(aes(x0 = 497, y0 = 258, r = 65), color = "green")
p2circle <- p2circle + geom_circle(aes(x0 = 220, y0 = 478, r = 65), color = "green")
p2circle
radius <- 65
center_x1 <- 497
center_y1 <- 258
df2$distance <- sqrt((df2$x - center_x1)^2 + (df2$y - center_y1)^2)
points_in_circle2 <- sum(df2$distance <= radius)
print(points_in_circle2)
center_x2 <- 220
center_y2 <- 478
df2$distance1 <- sqrt((df2$x - center_x2)^2 + (df2$y - center_y2)^2)
points_in_circle2 <- sum(df2$distance1 <= radius)
print(points_in_circle2)

SimilarTest <- read_csv("240808_r1592_2_baseline_test.csv")
SimilarTest <- SimilarTest %>%
  select(!15:24) %>%
  filter(Value.Item1.Item1.Item2>61) %>%
  rename(pixelx = Value.Item1.Item1.Item3.X, pixely = Value.Item1.Item1.Item3.Y, TimePoint = Value.Item1.Item1.Item2) %>%
  select(!1, !14) %>%
  na.omit
SimilarTest <- SimilarTest %>%
  filter(!Value.Item2 == TRUE)
x3 <- c(SimilarTest$pixelx)
y3 <- c(SimilarTest$pixely)
df3 <- data.frame(x = x3, y = y3)
p3 <- ggplot(df3, aes(x = x3, y = y3)) +
  geom_point(alpha = 0.3)
p3
p3circle <- p3 + geom_circle(aes(x0 = 381, y0 = 165, r = 60), color = "purple")
p3circle <- p3circle + geom_circle(aes(x0 = 140, y0 = 398, r = 60), color = "green") +
  ggtitle("baseline")
p3circle
radius <- 60
center_x3 <- 381
center_y3 <- 165
df3$distance <- sqrt((df3$x - center_x3)^2 + (df3$y - center_y3)^2)
points_in_circle3 <- sum(df3$distance <= radius)
print(points_in_circle3)
center_x3 <- 140
center_y3 <- 398
df3$distance1 <- sqrt((df3$x - center_x3)^2 + (df3$y - center_y3)^2)
points_in_circle3 <- sum(df3$distance1 <= radius)
print(points_in_circle3)

DifferentLearn <- read_csv("240808_r1592_3_baseline_learn.csv")
DifferentLearn <- DifferentLearn %>%
  select(!15:24) %>%
  filter(Value.Item1.Item1.Item2>50) %>%
  rename(pixelx = Value.Item1.Item1.Item3.X, pixely = Value.Item1.Item1.Item3.Y, TimePoint = Value.Item1.Item1.Item2) %>%
  select(!1, !14) %>%
  na.omit
DifferentLearn <- DifferentLearn %>%
  filter(!Value.Item2 == TRUE)
x4 <- c(DifferentLearn$pixelx)
y4 <- c(DifferentLearn$pixely)
df4 <- data.frame(x = x4, y = y4)
p4 <- ggplot(df4, aes(x = x4, y = y4)) +
  geom_point(alpha = 0.3)
p4
p4circle <- p4 + geom_circle(aes(x0 = 381, y0 = 175, r = 60), color = "blue")
p4circle <- p4circle + geom_circle(aes(x0 = 130, y0 = 393, r = 60), color = "blue")
p4circle
radius <- 60
center_x4 <- 381
center_y4 <- 175
df4$distance <- sqrt((df4$x - center_x4)^2 + (df4$y - center_y4)^2)
points_in_circle4 <- sum(df4$distance <= radius)
print(points_in_circle4)
center_x4 <- 130
center_y4 <- 393
df4$distance1 <- sqrt((df4$x - center_x4)^2 + (df4$y - center_y4)^2)
points_in_circle4 <- sum(df4$distance1 <= radius)
print(points_in_circle4)

BaselineLearn1 <- read_csv("240808_r1592_4_similar_learn.csv")
BaselineLearn1 <- BaselineLearn1 %>%
  select(!15:24) %>%
  filter(Value.Item1.Item1.Item2>54) %>%
  rename(pixelx = Value.Item1.Item1.Item3.X, pixely = Value.Item1.Item1.Item3.Y, TimePoint = Value.Item1.Item1.Item2) %>%
  select(!1, !14) %>%
  na.omit %>%
  filter(!Value.Item2 == TRUE)
x5 <- c(BaselineLearn1$pixelx)
y5 <- c(BaselineLearn1$pixely)
df5 <- data.frame(x = x5, y = y5)
p5 <- ggplot(df5, aes(x = x5, y = y5)) +
  geom_point(alpha = 0.3)
p5
p5circle <- p5 + geom_circle(aes(x0 = 386, y0 = 175, r = 60), color = "red")
p5circle <- p5circle + geom_circle(aes(x0 = 125, y0 = 393, r = 60), color = "red")
p5circle
radius <- 60
center_x5 <- 386
center_y5 <- 175
df5$distance <- sqrt((df5$x - center_x5)^2 + (df5$y - center_y5)^2)
points_in_circle5 <- sum(df5$distance <= radius)
print(points_in_circle5)
center_x6 <- 125
center_y6 <- 393
df5$distance1 <- sqrt((df5$x - center_x6)^2 + (df5$y - center_y6)^2)
points_in_circle6 <- sum(df5$distance1 <= radius)
print(points_in_circle6)

DifferentTest <- read_csv("240808_r1592_5_baseline_test.csv")
DifferentTest <- DifferentTest %>%
  select(!15:24) %>%
  filter(Value.Item1.Item1.Item2>87) %>%
  rename(pixelx = Value.Item1.Item1.Item3.X, pixely = Value.Item1.Item1.Item3.Y, TimePoint = Value.Item1.Item1.Item2) %>%
  select(!1, !14) %>%
  filter(!Value.Item2 == TRUE) %>%
  na.omit
x6 <- c(DifferentTest$pixelx)
y6 <- c(DifferentTest$pixely)
df6 <- data.frame(x = x6, y = y6)
p6 <- ggplot(df6, aes(x = x6, y = y6)) +
  geom_point(alpha = 0.3)
p6
p6circle <- p6 + geom_circle(aes(x0 = 366, y0 = 175, r = 60), color = "blue")
p6circle <- p6circle + geom_circle(aes(x0 = 135, y0 = 388, r = 60), color = "red")
p6circle
radius <- 60
center_x6 <- 366
center_y6 <- 175
df6$distance <- sqrt((df6$x - center_x6)^2 + (df6$y - center_y6)^2)
points_in_circle6 <- sum(df6$distance <= radius)
print(points_in_circle6)
center_x6 <- 135
center_y6 <- 388
df6$distance1 <- sqrt((df6$x - center_x6)^2 + (df6$y - center_y6)^2)
points_in_circle6 <- sum(df6$distance1 <= radius)
print(points_in_circle6)


