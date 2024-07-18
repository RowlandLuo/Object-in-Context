rm(list = ls())
library(tidyverse)
ObjectinContext <- read_csv("Bonsai-IsaRow - Object-in-context.csv")
ObjectinContext <- ObjectinContext %>%
  filter(`Trial Type`== "learn") %>%
  select(`Trial Type`, NewPosition1, NewPosition2)
NewPositions <- c(ObjectinContext$NewPosition1, ObjectinContext$NewPosition2) %>%
  na.omit
NewDF <- data.frame(FrameNumbers = NewPositions)
ggplot(NewDF, aes(x = NewPositions)) +
  geom_histogram(binwidth = 150) +
  labs(title = "Histogram of Frames at Both Positions", x = "Number of Frames")
#I use the z-score method to extract outliers.
mean_data <- mean(NewPositions)
sd_data <- sd(NewPositions)
z_score <- (NewPositions-mean_data)/sd_data
outliers <- NewPositions[abs(z_score) > 3]
print(outliers)
#According to the z-score method, there seems to be no outliers.
#Let's try interquartile range method.
Q1 <- quantile(NewPositions, 0.25)
Q3 <- quantile(NewPositions, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers1 <- NewPositions[NewPositions < lower_bound | NewPositions > upper_bound]
print(outliers1)
#It gives 2799, 2606, and 2549 as the three outliers, but it doesn't include 0 as an outlier.
#I am going to try extracting outliers again using Grubb's Test. Note that Grubb's test is used for normally distributed data. I think the data we have here is close enough to a normally distributed data.
install.packages("outliers")
library(outliers)
grubbs.test(NewPositions)
#I would probably go for the interquartile range, excluding the three-top numbers and also the lowest number 0. Because the rat will have to explore both objects to a certain degree.
ObjectinContext <- read_csv("Bonsai-IsaRow - Object-in-context.csv")
ObjectinContext <- ObjectinContext %>%
  select(1:16) %>%
  filter(!(`Rat Number` == "r1569" & `Trial Number` == 0:2)) 
#The 2799, 2606 and 2549 are corresponding to the r1572 rat and the r1573 rat. I think in the final analysis, r1572 rat definitely needs to be excluded due to extensive climbing behavior and also it contributes to the largest outlier 2799. But I am tempted to keep 1573 rat. I will need the full data to decide. For now we will keep all these three large-number outliers.
#To do this, I believe we need a Two-way ANOVA. The dependent variable is "Ratio," or "Frames", and independent variables are Rat Age and Positions. I think ultimately I will need a 3-way ANOVA or even 4-way ANOVA, but I do not have enough data now.
ObjectinContextforAnalysis <- read_csv("Bonsai-IsaRow - ObjectinContextforAnalysis.csv")
ObjectinContextforAnalysis <- ObjectinContextforAnalysis %>%
  mutate(Positions = factor(Positions, levels = c(1, 2)),
         RatAge = factor(RatAge, levels = c("p17", "p23")))
anova_model <- aov(Ratio ~ Positions * RatAge, data = ObjectinContextforAnalysis)
summary(anova_model)
install.packages("car")
library(car)
TukeyHSD(anova_model)
p <- ggplot(ObjectinContextforAnalysis, aes(x = Positions, y = Ratio, color = RatAge, group = RatAge)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Positions", y = "Ratio") +
  theme_minimal()
p <- p + geom_point()
p
anova_model1 <- aov(Frames ~ Positions * RatAge, data = ObjectinContextforAnalysis)
summary(anova_model1)
TukeyHSD(anova_model1)
p1 <- ggplot(ObjectinContextforAnalysis, aes(x = Positions, y = Frames, color = RatAge, group = RatAge)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Positions", y = "Frames") +
  theme_minimal()
p1 <- p1 + geom_point()
p1

