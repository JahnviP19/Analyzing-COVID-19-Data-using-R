rm(list=ls()) # removes all variables stored previously
library(Hmisc) # import
library(ggplot2) # import

data <- read.csv("E:/R/Analyzing COVID-19 Data/COVID19_line_list_data.csv")
describe(data) # Hmisc command

# cleaned up death column
data$death_dummy <- as.integer(data$death != 0)
# death rate
sum(data$death_dummy) / nrow(data)

# AGE
# claim: people who die are older
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
# is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant


# Plotting the histogram for age distribution of deceased individuals
ggplot(data = dead, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(title = "Age Distribution of Deceased Individuals", x = "Age", y = "Frequency")

# Plotting the histogram for age distribution of alive individuals
ggplot(data = alive, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Age Distribution of Alive Individuals", x = "Age", y = "Frequency")


# GENDER
# claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.5%!
mean(women$death_dummy, na.rm = TRUE) #3.7%
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so this is statistically
# significant

death_rate_men <- mean(men$death_dummy, na.rm = TRUE) * 100  # Calculating death rate as a percentage
death_rate_women <- mean(women$death_dummy, na.rm = TRUE) * 100  # Calculating death rate as a percentage

# A data frame for plotting
death_rates <- data.frame(
  Gender = c("Male", "Female"),
  Death_Rate = c(death_rate_men, death_rate_women)
                          )

# Bar plot to compare death rates by gender
ggplot(data = death_rates, aes(x = Gender, y = Death_Rate, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  labs(title = "Death Rate by Gender", x = "Gender", y = "Death Rate (%)") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  geom_text(aes(label = paste0(round(Death_Rate, 2), "%")), vjust = -0.5, 
            size = 4, position = position_dodge(width = 0.5))