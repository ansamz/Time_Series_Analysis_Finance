# This Script was used to get the Price data on MSFT, so that we can enrich
# the data with News data and their sentiment scores via python script included in this folder

## Import Libraries
library(utils)
library(dplyr)
library(ggplot2)
library(scales)
library(signal)
library(quantmod)

# MSFT <- getSymbols.yahoo("MSFT", from="2022-01-01", to="2023-12-01", periodicity = "daily", auto.assign=FALSE)[,6]
# write.zoo(MSFT, "MSFT.csv", sep = ",", row.names = TRUE)

### Sentiment analysis
data.vadar <- read.csv("data/final_data_vadar.csv")

# Drop column the reddit pots since they are not needed for the analysis
columns_to_drop <- c("worldnews", "stocks", "investing")
data.vadar <- data.vadar[, -which(names(data.vadar) %in% columns_to_drop)]

head(data.vadar)

# Inspect the data
str(data.vadar)

# Convert the "Index" column to a Date type
data.vadar$Index <- as.Date(data.vadar$Index)

# Plot the average sentiment score against the MSFT.
ggplot(data = data.vadar, aes(x = Index)) +
  geom_line(aes(y = MSFT.Adjusted, color = "MSFT.Adjusted"), size = 1) +
  ylab("MSFT Adjusted Price") +
  
  geom_line(aes(y = sentiment_average_score * 20, color = "Sentiment Average Score"), size = 1) +
  ylab("Sentiment Average Score") +
  
  scale_y_continuous(
    name = "MSFT Adjusted Price",
    sec.axis = sec_axis(~./20, name = "Sentiment Average Score"),
    limits = c(0, 400),
    breaks = seq(0, 400, by = 50)
  ) +
  
  scale_color_manual(
    values = c("MSFT.Adjusted" = "#00AFBB", "Sentiment Average Score" = "red"),
    labels = c("MSFT Adjusted Price", "Sentiment Average Score")
  ) +
  
  ggtitle("MSFT Adjusted Price and Sentiment Average Score Over Time") +
  theme_minimal()

# Convert sentiment_average_score to a time series object
sentiment_ts <- as.zoo(data.vadar$sentiment_average_score)

# Perform the Fourier Transform on the sentiment
close_fft <- fft(sentiment_ts)

# Retain 20 components
num_components <- 20
fft_list_m20 <- Re(close_fft)
fft_list_m20[(num_components + 1):(length(fft_list_m20) - num_components)] <- 0
smoothed_signal <- Re(fft_list_m20)

# Create a new column in data.vadar with the smoothed signal
data.vadar[paste("fourier", num_components, sep = "_")] <- as.vector(fft(ifft(smoothed_signal)))


# Plot after smoothing
ggplot(data = data.vadar, aes(x = Index)) +
  geom_line(aes(y = MSFT.Adjusted, color = "MSFT.Adjusted"), size = 1) +
  ylab("MSFT Adjusted Price") +
  
  # Re(fourier_20) to extract the real part of the complex number
  geom_line(aes(y = Re(fourier_20) * 2000, color = "Fourier 20"), size = 1) +
  ylab("Fourier 20") +
  
  scale_y_continuous(
    name = "MSFT Adjusted Price",
    sec.axis = sec_axis(~./2000, name = "Fourier 20"),
    limits = c(0, 400),
    breaks = seq(0, 400, by = 50)
  ) +
  
  scale_color_manual(
    values = c("MSFT.Adjusted" = "#00AFBB", "Fourier 20" = "blue"),
    labels = c("MSFT Adjusted Price", "Fourier 20")
  ) +
  
  ggtitle("MSFT Adjusted Price and Fourier 20 Over Time") +
  theme_minimal()

# dismissing the idea for lack of time 

MSFT <- read.csv("data/final_data.csv")
MSFT$Index <- as.Date(MSFT$Index)
MSFT$MSFT.Adjusted <- as.numeric(as.character(MSFT$MSFT.Adjusted))
MSFT$average_polarity <- as.numeric(as.character(MSFT$average_polarity))
MSFT_ts <- zoo(MSFT$MSFT.Adjusted, order.by = MSFT$Index)

ix.na = is.na(MSFT$average_polarity)
problematic_values <- MSFT$average_polarity[ix.na]
MSFT$average_polarity[problematic_values] <- NA
# Replace with 0
MSFT$average_polarity[problematic_values] <- 0
MSFT$Index_numeric <- as.numeric(MSFT$Index - as.Date("2000-01-01"))
MSFT$smoothed_polarity_loess <- with(MSFT, loess(average_polarity ~ Index_numeric, span = 0.2)$fitted)

ggplot(data = MSFT, aes(x = Index)) +
  geom_line(aes(y = MSFT.Adjusted, color = "MSFT.Adjusted"), size = 1) +
  ylab("MSFT Adjusted Price") +
  
  geom_line(aes(y = smoothed_polarity_loess * 2000, color = "Sentiment Average Score"), size = 1) +
  ylab("Sentiment Average Score") +
  
  scale_y_continuous(
    name = "MSFT Adjusted Price",
    sec.axis = sec_axis(~./2000, name = "Sentiment Average Score"),
    limits = c(0, 400),
    breaks = seq(0, 400, by = 50)
  ) +
  
  scale_color_manual(
    values = c("MSFT.Adjusted" = "#00AFBB", "Sentiment Average Score" = "red"),
    labels = c("MSFT Adjusted Price", "Sentiment Average Score")
  ) +
  
  ggtitle("MSFT Adjusted Price and Sentiment Average Score Over Time") +
  theme_minimal()


########################
MSFT <- read.csv("data/final_data.csv")
MSFT$Index <- as.Date(MSFT$Index)
MSFT$MSFT.Adjusted <- as.numeric(as.character(MSFT$MSFT.Adjusted))
MSFT$average_polarity <- as.numeric(as.character(MSFT$average_polarity))
# MSFT$Index <- as.Date(MSFT$Index)
MSFT_ts <- zoo(MSFT$MSFT.Adjusted, order.by = MSFT$Index)

# plot(MSFT_ts)

events <- data.frame(
  date = as.Date(c("2023-01-23", "2022-11-30", "2023-06-29", "2023-04-12", "2023-11-15")),
  event = c(
    "Microsoft investing 10 billion dollars into OpenAI",
    "ChatGPT being released and created an AI-hype",
    "Microsoft releasing Bing Co-Pilot and Microsoft Co-Pilot",
    "The release of DALL-E 3 disrupting the image gen AI market",
    "Sam Altman leaving OpenAI in November 2023"
  )
)

event_data <- merge(events, MSFT, by.x = "date", by.y = "Index", all.x = TRUE)

# Display the result
print(event_data)

ix.na = is.na(MSFT$average_polarity)
problematic_values <- MSFT$average_polarity[ix.na]
MSFT$average_polarity[problematic_values] <- NA
# Replace with 0
MSFT$average_polarity[problematic_values] <- 0
MSFT$Index_numeric <- as.numeric(MSFT$Index - as.Date("2000-01-01"))
MSFT$smoothed_polarity_loess <- with(MSFT, loess(average_polarity ~ Index_numeric, span = 0.2)$fitted)

library(ggplot2)

# Your existing plot
existing_plot <- ggplot(data = MSFT, aes(x = Index)) +
  geom_line(aes(y = MSFT.Adjusted, color = "MSFT.Adjusted"), size = 1) +
  ylab("MSFT Adjusted Price") +
  
  geom_line(aes(y = smoothed_polarity_loess * 2000, color = "Sentiment Average Score"), size = 1) +
  ylab("Sentiment Average Score") +
  
  scale_y_continuous(
    name = "MSFT Adjusted Price",
    sec.axis = sec_axis(~./2000, name = "Sentiment Average Score"),
    limits = c(0, 400),
    breaks = seq(0, 400, by = 50)
  ) +
  
  scale_color_manual(
    values = c("MSFT.Adjusted" = "#00AFBB", "Sentiment Average Score" = "red"),
    labels = c("MSFT Adjusted Price", "Sentiment Average Score")
  ) +
  
  ggtitle("MSFT Adjusted Price and Sentiment Average Score Over Time") +
  theme_minimal()

# Create the arrows layer
arrows_layer <- ggplot(event_data, aes(x = date, y = MSFT.Adjusted)) +
  geom_segment(aes(xend = date, yend = MSFT.Adjusted - 20), 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")),
               size = 0.5) +
  labs(x = NULL, y = NULL) +  # Remove axis labels
  theme_void()

# Combine the existing plot and the arrows layer
combined_plot <- existing_plot + annotation_custom(
  ggplotGrob(arrows_layer),
  xmin = min(event_data$date), xmax = max(event_data$date),
  ymin = min(event_data$MSFT.Adjusted) + 10, ymax = max(event_data$MSFT.Adjusted) + 10
)

# Print the combined plot
print(combined_plot)

