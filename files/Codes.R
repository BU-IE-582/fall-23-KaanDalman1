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

#histogram
install.packages("ggplot2")

library(ggplot2)
library(dplyr)

ggplot(grouped_data, aes(x = price)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histogram", x = "price", y = "frequency")


#Task Moving Window Correlation 

install.packages("zoo")
install.packages("TTR")
install.packages("tidyr")

library(dplyr)
library(zoo)
library(ggplot2)
library(TTR)
library(tidyr)

#data_A <- subset(data, short_name == "AEFES")
#data_B <- subset(data, short_name == "AKBNK")

#data_A <- data_A %>%
 # group_by(year_month = format(as.Date(timestamp), "%Y-%m")) %>%
  #summarize(avg_price = mean(price, na.rm = TRUE))
#data_B <- data_B %>%
 # group_by(year_month = format(as.Date(timestamp), "%Y-%m")) %>%
  #summarize(avg_price = mean(price, na.rm = TRUE))

#unternehmenA <- data[data$short_name == "AEFES", ]
#unternehmenB <- data[data$short_name == "AKBNK", ]

# Annahme: Du möchtest die Moving Window Correlation für die Unternehmen "Unternehmen1" und "Unternehmen2" durchführen.



data_filtered <- data %>%
  filter(short_name %in% c("AEFES", "AKBNK")) %>%
  mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ")) %>%
  arrange(short_name, timestamp) %>%
  complete(timestamp = seq.POSIXt(min(timestamp), max(timestamp), by = "15 min")) %>%
  tidyr::fill(price)  # Hier wird explizit angegeben, dass die Funktion aus dem "tidyr"-Paket stammt.


# Gruppiere die Daten nach Timestamp und Unternehmen
grouped_data <- data_filtered %>%
  group_by(timestamp, short_name) %>%
  summarise(price = mean(price, na.rm = TRUE)) %>%
  ungroup()

# Konvertiere die Timestamps in ein korrektes Format
grouped_data$timestamp <- as.POSIXct(grouped_data$timestamp, format = "%Y-%m-%d %H:%M:%S")

# Erstelle eine zoo-Zeitreihe
zoo_data <- zoo(grouped_data$price, order.by = grouped_data$timestamp)


# Definiere die Größe des Moving Windows in Sekunden
window_size <- 10 * 3600  # 10 Stunden in Sekunden

# Berechne die Moving Window Correlation für AEFES und AKBNK
correlation <- rollapply(zoo_data, width = window_size, FUN = function(x) {
  # Entferne fehlende Werte aus dem Fenster
  x <- na.omit(x)
  # Überprüfe, ob genügend Datenpunkte vorhanden sind
  if (length(x) >= 2) {
    cor(x["AEFES"], x["AKBNK"])
  } else {
    NA  # Wenn nicht genügend Daten vorhanden sind, gib NA zurück
  }
}, by = 900, align = "right")

# Erstelle einen Dataframe mit den Ergebnissen der Moving Window Correlation
correlation_df <- data.frame(timestamp = time(correlation), correlation = coredata(correlation))

# Filtere Zeilen mit NA-Korrelationswerten aus
correlation_df <- correlation_df[!is.na(correlation_df$correlation), ]

# Erstelle einen Graphen
ggplot(correlation_df, aes(x = timestamp, y = correlation)) +
  geom_line() +
  labs(title = "Moving Window Correlation zwischen AEFES und AKBNK",
       x = "Zeitstempel",
       y = "Korrelation") +
  theme_minimal()




