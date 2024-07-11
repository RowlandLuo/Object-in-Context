library(tidyverse)
PositionBias <- read_csv("Bonsai-IsaRow - Object-in-Context.csv")
PositionBias <- PositionBias %>%
  select(23:24)
boxplot(Frames ~ FramePosition, data = PositionBias, main = "Boxplot of Interest at 2 Positions", xlab = "Positions")
t.test(Frames ~ FramePosition, data = PositionBias)

#Now I check if all objects are about equally interesting.
ObjectInterest <- read_csv("Bonsai-IsaRow - Object-in-context.csv")
ObjectInterest <- ObjectInterest %>%
  select(26:27)
boxplot(Frames1 ~ Objects, data = ObjectInterest)
one.way <- aov(Frames1 ~ Objects, data = ObjectInterest)
summary(one.way)
#So the green jar is not significantly less interesting than other objects.