# Camille Sloan
# IS 4300 Holland
# Spring 2021
# Final Project raw script file

library(tidyverse)

# Reading and saving the dataset to train the model with
urlfile <- "https://vincentarelbundock.github.io/Rdatasets/csv/DAAG/spam7.csv"
spam <- read_csv(url(urlfile))


# View the dataset and it's structure
head(spam)
str(spam)

# Create a plot to visualize the data and chosen variables
spamPlot <- spam %>% ggplot()
spamPlot + geom_point(aes(n000, dollar,col=yesno),size = 1.5)

# Filter out the outliers and then recreate the plot
filtered_spam <- spam %>% filter(n000 < 2 & dollar < 2)
spamPlot <- filtered_spam %>% ggplot()
spamPlot + geom_point(aes(n000, dollar,col=yesno),size = 1.5)


# Add in the decision lines
spamPlot + geom_point(aes(n000, dollar,col=yesno),size = 1.5) +
  geom_vline(xintercept = 0.05) +
  geom_hline(yintercept = 0.06)


# Create the model and table the results
spam$prediction <- ifelse(spam$n000 >= 0.05,"y",
                          ifelse(spam$dollar >= 0.06, "y", "n"))
T <- table(Predicted = spam$prediction, Actual = spam$yesno)
T


# Analyze the results using the accuracy, sensitivity, and specificity code
T <- as.vector(T)
accuracy <- (T[1]+T[4])/(T[1]+T[2]+T[3]+T[4]) 
sensitivity <- T[4]/(T[3]+T[4])
specificity <- T[1]/(T[1]+T[2])
metric <- c("Accuracy","Sensitivity","Specificity")
value <- c(accuracy,sensitivity,specificity)
data.frame(Metric = metric,Value = round(value,3))
