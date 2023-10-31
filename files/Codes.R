# Laden der CSV Datei mit Kommata als Trennzeichen

data <- read.csv("C:/Users/kaand/OneDrive/Desktop/Bogazici Courses/IE 582/Homework 1/all_ticks_long.csv/all_ticks_long.csv")

data2 <- read.csv("C:/Users/kaand/OneDrive/Desktop/Bogazici Courses/IE 582/Homework 1/all_ticks_wide.csv/all_ticks_wide.csv")

mean_price <- mean(data2$AEFES)

summary(data2$AEFES)

summary(data2$AEFES)

install.packages("dplyr")

#Laden Sie die dplyr-Bibliothek

library(dplyr)

grouped_data <- data %>%
  group_by(short_name)

#Wenden Sie die summary() Funktion auf jede Gruppe an 
summary_data <- grouped_data %>%
  summarize(
    Mean_Price = mean(price, na.rm = TRUE),
    Median_Price = median(price, na.rm = TRUE)
  )

#Anzeigen der Zusammenfassung
print(summary_data)

print(summary_data, n=60)

#Berechnung der Varianz und Standardabweichung für jede Gruppe

result <- grouped_data %>%
  summarize(
    Variance = var(price, na.rm = TRUE),
    StdDeviation = sd(price, na.rm = TRUE)
  )

print(result)

print(result, n=60)

#Berechnung von mean, median, var und sd

result2 <- grouped_data %>%
  summarize(
    Mean_Price = mean(price, na.rm = TRUE),
    Median_Price = median(price, na.rm = TRUE),
    Variance = var(price, na.rm = TRUE),
    StdDeviation = sd(price, na.rm = TRUE)
  )

print(result2, n=60)

# measures of shape

install.packages("e1071")

library(dplyr)
library(e1071)

result3 <- grouped_data %>%
  summarize(
    Skewness = skewness(price, na.rm = TRUE),
    Kurtosis = kurtosis(price, na.rm = TRUE)
  )

print(result3, n=60)

#histograms
install.packages("ggplot2")

library(ggplot2)
library(dplyr)

ggplot(grouped_data, aes(x = price)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogramm der Preise", x = "Preis", y = "Häufigkeit")



