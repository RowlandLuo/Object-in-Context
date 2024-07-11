library(tidyverse)
PositionBias <- read_csv("Bonsai-IsaRow - Object-in-Context.csv")
PositionBias <- PositionBias %>%
  select(23:24)
boxplot(Frames ~ FramePosition, data = PositionBias, main = "Boxplot of Interest at 2 Positions", xlab = "Positions")
t.test(Frames ~ FramePosition, data = PositionBias)
