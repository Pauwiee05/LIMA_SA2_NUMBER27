#install.packages("tidyverse")
#install.packages("rstatix")
#install.packages("car")
#install.packages("ggplot2")
#install.packages("lme4")

library(tidyverse)
library(rstatix)
library(car)
library(ggplot2)
library(lme4)

# Load the data of the study
dataframe <- data.frame('Cars' = rep(1:5, each = 4),
                        'Oil' = rep(1:4, times = 5),
                        'Mileage' = c(36, 38, 30, 29,
                                      34, 38, 30, 29,
                                      34, 28, 38, 32,
                                      38, 34, 20, 44,
                                      26, 28, 34, 50))
dataframe$Oil <- as.factor(dataframe$Oil)

# THE BOXPLOT 
ggplot(dataframe, aes(x = factor(Oil), y = Mileage)) +
  geom_boxplot() +
  labs(x = "Engine Oil", y = "Mileage") +
  ggtitle("Boxplot of Mileage for Each Engine Oil Level")

# SHAPIRO-WILK test for Test of Normality
shapiro_test <- dataframe %>%
  group_by(Oil) %>%
  summarise(p_value = shapiro.test(Mileage)$p.value)
print(shapiro_test)


# MAULCHY's Test 
mauchly_test <- dataframe %>%
  anova_test(dv = Mileage, wid = Cars, within = Oil, detailed = TRUE)
print(mauchly_test)

# POST-HOC Test
posthoc <- dataframe %>%
  pairwise_t_test(
    Mileage ~ Oil,
    paired = TRUE,
    p.adjust.method = "holm",
    effect.size = "cohen.d"
  )
print(posthoc)
