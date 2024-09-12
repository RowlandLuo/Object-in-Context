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
         RatAge = factor(RatAge, levels = c("p17", "p23")),
         Environment = factor(Environment, levels = c("Baseline", "Similar", "Different")))
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
p2 <- ggplot(ObjectinContextforAnalysis, aes(x = RatAge, y = Ratio, color = Positions, group = Positions)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(x = "Rat Age", y = "Ratio") +
  theme_minimal()
p2 <- p2 + geom_point()
p2
#Isa said we need to do a three-way ANOVA even if the power is not enough. So let's do that.
install.packages("emmeans")
install.packages("multcomp")
install.packages("carData")
install.packages("mvtnorm")
install.packages("survival")
install.packages("TH.data")
install.packages("MASS")
library(carData)
library(car)
library(emmeans)
library(mvtnorm)
library(survival)
library(TH.data)
library(MASS)
library(multcomp)
fit <- aov(Ratio ~ Positions * RatAge * Environment, data = ObjectinContextforAnalysis)
summary(fit)
#Nothing is significant. Now we visualize the three-way ANOVA results.
summary_data <- ObjectinContextforAnalysis %>%
  group_by(Positions, RatAge, Environment) %>%
  summarise(mean_ratio = mean(Ratio), .groups = "drop")
summary_data
p3 <- ggplot(summary_data, aes(x = Positions, y = mean_ratio, color = Environment, group = Environment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~RatAge)
#Let's try it without correcting the data with position bias.
fit1 <- aov(Frames ~ Positions * RatAge * Environment, data = ObjectinContextforAnalysis)
summary(fit1)
#The environment has a significant main effect!!!
summary_data1 <- ObjectinContextforAnalysis %>%
  group_by(Positions, RatAge, Environment) %>%
  summarise(mean_frames = mean(Frames), .groups = "drop")
summary_data1
p4 <- ggplot(summary_data1, aes(x = Positions, y = mean_frames, color = Environment, group = Environment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~RatAge)
#OKay, this is a mixed model ANOVA and I need to redo the analysis.

#Mixed Effect Model.
library(tidyverse)
analysis <- read_csv("Bonsai-IsaRow - Sheet6.csv")
analysis <- analysis %>%
  mutate(RatNumber = factor(RatNumber), 
         RatAge = factor(RatAge, levels = c("Preweaning", "Postweaning", "Adult")),
         Environment = factor(Environment, levels = c("baselineS", "similar", "baselineD", "different")))
analysis <- analysis %>% na.omit
#Prerequisite 1: No significant outliers.
install.packages("rstatix")
library(rstatix)
analysis %>%
  group_by(RatAge) %>%
  identify_outliers(`Discrimination Score`)
analysis %>%
  group_by(Environment) %>%
  identify_outliers(`Discrimination Score`)
#No outliers detected!

#Prerequisite 2: Normality within each Group.
GroupbyRatAge <- analysis %>%
  filter(RatAge == "Preweaning")
install.packages("ggpubr")
GroupbyRatAge <- analysis %>%
  filter(RatAge == "Postweaning")
GroupbyRatAge <- analysis %>%
  filter(RatAge == "Adult")
install.packages("ggpubr")
library(ggpubr)
ggqqplot(GroupbyRatAge, x = "Discrimination Score")
#Approximately normally distributed (Within the allowed range).

#Prerequisite 3: Homogeneity of Variances.
levene_test(`Discrimination Score` ~ RatAge, data = analysis)
#p = 0.771 passed the homogeneity test.
levene_test(`Discrimination Score` ~ Environment, data = analysis)
#Again, passed the homogeneity test.

#Prerequisite 4: Assumption of Sphericity.
#This will be reported when testing the model.

#Prerequisite 5: Homogeneity of Covariances.
install.packages("heplots")
boxm <- box_m(analysis[, 4], analysis$RatAge)
print(boxm)
#Passed the homogeneity of covariances test.

#Prepare summary statistics.
analysis %>%
  group_by(RatAge, Environment) %>%
  summarise(mean = mean(`Discrimination Score`),
            sd = sd(`Discrimination Score`))

#Visualization.
ggboxplot(
  analysis, x = "RatAge", y = "Discrimination Score",
  color = "Environment", palette = "jco"
)

#Computation
library(lme4)
model <- lmer(`Discrimination Score` ~ RatAge * Environment + (1|RatNumber), data = analysis)
summary(model)
#None of them are significant, unfortunately.
library(emmeans)
em <- emmeans(model, ~ RatAge * Environment)
pairs(em, by = "RatAge")
pairs(em, by = "Environment")

#Excluding low outliers in the learn trials.
library(tidyverse)
ObjectinContext <- read_csv("Bonsai-IsaRow - Object-in-context.csv")
ObjectinContext <- ObjectinContext %>%
  filter(`Trial Type` == "learn") %>%
  select(`Rat Number`,NewPosition1, NewPosition2)
NewPositions <- c(ObjectinContext$NewPosition1, ObjectinContext$NewPosition2) %>%
  na.omit
NewDF <- data.frame(FrameNumbers = NewPositions)
ggplot(NewDF, aes(x = NewPositions)) +
  geom_histogram(binwidth = 150) +
  labs(title = "Histogram of Frames at Both Positions", x = "Number of Frames")
#There are outliers on the larger end but not the smaller end. So I will just exclude any rats with 0.
ObjectinContext %>%
  filter(NewPosition1 == 0|NewPosition2 == 0)
#r1569, r1589, r1588, r1591
analysis <- read_csv("Bonsai-IsaRow - Sheet6.csv")
analysis <- analysis %>%
  mutate(RatNumber = factor(RatNumber), 
         RatAge = factor(RatAge, levels = c("Preweaning", "Postweaning", "Adult")),
         Environment = factor(Environment, levels = c("baselineS", "similar", "baselineD", "different"))) %>%
  na.omit
analysis <- analysis %>%
  filter(!RatNumber == "r1569") %>%
  filter(!RatNumber == "r1589") %>%
  filter(!RatNumber == "r1588") %>%
  filter(!RatNumber == "r1591")
#Visualization
library(ggpubr)
ggboxplot(
  analysis, x = "RatAge", y = "Discrimination Score",
  color = "Environment", palette = "jco"
)

#Computation
library(lme4)
model <- lmer(`Discrimination Score` ~ RatAge * Environment + (1|RatNumber), data = analysis)
summary(model)
library(emmeans)
em <- emmeans(model, ~ RatAge * Environment)
pairs(em, by = "RatAge")
pairs(em, by = "Environment")


#Exlude climbing rats.
#r1571, R1572, r1573, r1579, r1580
library(tidyverse)
analysis <- read_csv("Bonsai-IsaRow - Sheet6.csv")
analysis <- analysis %>%
  mutate(RatNumber = factor(RatNumber), 
         RatAge = factor(RatAge, levels = c("Preweaning", "Postweaning", "Adult")),
         Environment = factor(Environment, levels = c("baselineS", "similar", "baselineD", "different")))
analysis <- analysis %>%
  filter(!RatNumber == "r1571") %>%
  filter(!RatNumber == "r1572") %>%
  filter(!RatNumber == "r1573") %>%
  filter(!RatNumber == "r1598")
#Visualization
library(ggpubr)
ggboxplot(
  analysis, x = "RatAge", y = "Discrimination Score",
  color = "Environment", palette = "jco"
)
#Delete the one outlier in postweaning baseline D group.
PBaselineD <- analysis %>%
  filter(RatAge == "Postweaning") %>%
  filter(Environment == "baselineD")
PBaselineD

#Computation
library(lme4)
model <- lmer(`Discrimination Score` ~ RatAge * Environment + (1|RatNumber), data = analysis)
summary(model)
library(emmeans)
em <- emmeans(model, ~ RatAge * Environment)
pairs(em, by = "RatAge")
pairs(em, by = "Environment")

#Exclude climbing rats AND 0s (It should be the official one.)
library(tidyverse)
analysis <- read_csv("Bonsai-IsaRow - Sheet6.csv")
analysis <- analysis %>%
  mutate(RatNumber = factor(RatNumber), 
         RatAge = factor(RatAge, levels = c("Preweaning", "Postweaning", "Adult")),
         Environment = factor(Environment, levels = c("baselineS", "similar", "baselineD", "different")))
analysis <- analysis %>%
  filter(!RatNumber == "r1571") %>%
  filter(!RatNumber == "r1572") %>%
  filter(!RatNumber == "r1573") %>%
  filter(!RatNumber == "r1569") %>%
  filter(!RatNumber == "r1589") %>%
  filter(!RatNumber == "r1588") %>%
  filter(!RatNumber == "r1591")
  
#Visualization
library(ggpubr)
ggboxplot(
  analysis, x = "RatAge", y = "Discrimination Score",
  color = "Environment", palette = "jco"
)

#Computation
library(lme4)
model <- lmer(`Discrimination Score` ~ RatAge * Environment + (1|RatNumber), data = analysis)
summary(model)
library(emmeans)
em <- emmeans(model, ~ RatAge * Environment)
pairs(em, by = "RatAge")
pairs(em, by = "Environment")

#Count the number of animals with discrimination scores over 0.5.
library(tidyverse)
analysis <- read_csv("Bonsai-IsaRow - Sheet6.csv")
count <- analysis %>%
  filter(`Discrimination Score` > 0.5) %>%
  group_by(RatAge, Environment) %>%
  count() %>%
  as_tibble
count <- count %>%
  mutate(num = 1:12)
Totalcount <- analysis %>%
  group_by(RatAge, Environment) %>%
  count() %>%
  as_tibble %>%
  mutate(num = 1:12)
count <- count %>%
  mutate(total = Totalcount$n[match(num, Totalcount$num)])
count
count <- count %>%
  mutate(prop = n/total)
count
count$RatAge <- factor(count$RatAge, levels = c("Preweaning", "Postweaning", "Adult"))
count$Environment <- factor(count$Environment, levels = c("baselineS", "similar", "baselineD", "different"))
count

#Visualization
library(ggplot2)
ggplot(count, aes(x = RatAge, y = prop, fill = Environment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "The Proportion of Animals with Discrimination Scores Greater than 0.5",
       y = "porportion")
ggplot(count, aes(x = Environment, y = prop, fill = RatAge)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "The Proportion of Animals with Discrimination Scores Greater than 0.5",
       y = "proportion")

#Mixed Model ANOVA
analysis <- analysis %>%
  mutate(Environment = recode_factor(Environment, "baselineS" = "similar")) %>%
  mutate(Environment = recode_factor(Environment, "baselineD" = "different")) 
analysis <- analysis %>%
  filter(!RatNumber == "r1571") %>%
  filter(!RatNumber == "r1572") %>%
  filter(!RatNumber == "r1573") %>%
  filter(!RatNumber == "r1569") %>%
  filter(!RatNumber == "r1589") %>%
  filter(!RatNumber == "r1588") %>%
  filter(!RatNumber == "r1591") %>%
  na.omit
analysis <- analysis %>%
  mutate(RatNumber = factor(RatNumber), 
         RatAge = factor(RatAge, levels = c("Preweaning", "Postweaning", "Adult")),
         Environment = factor(Environment, levels = c("similar", "different")))

#Visualization
library(ggpubr)
ggboxplot(
  analysis, x = "RatAge", y = "Discrimination Score",
  color = "Environment", palette = "jco"
)

#Computation
library("tidyverse")
library("afex")
library("emmeans")
install.packages("cowplot")
library("cowplot")
install.packages("ggbeeswarm")
library(ggbeeswarm)
theme_set(theme_bw(base_size = 15)+
            theme(legend.position = "bottom"))
a1 <- aov_ez("RatNumber", "Discrimination Score", analysis, between = "RatAge", 
             within = "Environment")
a1
afex_plot(a1, "RatAge")
afex_plot(a1, "Environment")
afex_plot(a1, "RatAge", "Environment")
em1 <- emmeans(a1, c("RatAge", "Environment"))
em1
pairs(em1, by = "RatAge")
pairs(em1, by = "Environment")

#Count the number of animals with discrimination scores over 0.5.
count <- analysis %>%
  filter(`Discrimination Score` > 0.5) %>%
  group_by(RatAge, Environment) %>%
  count() %>%
  as_tibble
count <- count %>%
  mutate(num = 1:6)
Totalcount <- analysis %>%
  group_by(RatAge, Environment) %>%
  count() %>%
  as_tibble %>%
  mutate(num = 1:6)
count <- count %>%
  mutate(total = Totalcount$n[match(num, Totalcount$num)])
count
count <- count %>%
  mutate(prop = n/total)
count
count$RatAge <- factor(count$RatAge, levels = c("Preweaning", "Postweaning", "Adult"))
count$Environment <- factor(count$Environment, levels = c("similar", "different"))
count

#Visualization
library(ggplot2)
ggplot(count, aes(x = RatAge, y = prop, fill = Environment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "The Proportion of Animals with Discrimination Scores Greater than 0.5",
       y = "porportion")
ggplot(count, aes(x = Environment, y = prop, fill = RatAge)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "The Proportion of Animals with Discrimination Scores Greater than 0.5",
       y = "proportion")