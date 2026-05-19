# =====================================
# UK Inflation Project
# =====================================

# -------------------------
# Set Working Directory
# -------------------------

setwd("C:/Users/marcu/Downloads/Inflation Project")

# -------------------------
# Load Packages
# -------------------------

library(sandwich)
library(lmtest)
library(car)
library(tseries)

# =====================================
# Import Data
# =====================================

cpi <- read.csv(
  "CPI Data.csv",
  header = FALSE,
  stringsAsFactors = FALSE
)

wages <- read.csv(
  "Wages Data.csv",
  header = FALSE,
  stringsAsFactors = FALSE
)

unemployment <- read.csv(
  "Unemployment Data.csv",
  header = FALSE,
  stringsAsFactors = FALSE
)

energy_prices <- read.csv(
  "Energy Prices Data.csv",
  header = FALSE,
  stringsAsFactors = FALSE
)

consumer_confidence <- read.csv(
  "Consumer Confidence Data.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

# =====================================
# Subset Relevant Data
# =====================================

Monthly <- subset(cpi, V1 >= "2015 JAN" & V1 <= "2025 DEC")

Wages <- wages[175:306, ]

Unemployment <- unemployment[811:942, ]

Energy_Prices <- energy_prices[334:465, ]

Consumer_Confidence <- consumer_confidence[493:624, ]

# =====================================
# Rename Columns
# =====================================

colnames(Monthly) <- c(
  "Date",
  "CPI"
)

colnames(Wages) <- c(
  "Date",
  "Wage_Growth"
)

colnames(Unemployment) <- c(
  "Date",
  "Unemployment"
)

colnames(Energy_Prices) <- c(
  "Date",
  "Energy_Prices"
)

colnames(Consumer_Confidence) <- c(
  "Date",
  "Consumer_Confidence"
)

# =====================================
# Fix Date Formats
# =====================================

Monthly$Date <- toupper(
  trimws(Monthly$Date)
)

Wages$Date <- toupper(
  trimws(Wages$Date)
)

Unemployment$Date <- toupper(
  trimws(Unemployment$Date)
)

Energy_Prices$Date <- format(
  as.Date(Energy_Prices$Date),
  "%Y %b"
)

Energy_Prices$Date <- toupper(
  Energy_Prices$Date)

Consumer_Confidence$Date <- format(
  as.Date(Consumer_Confidence$Date),
  "%Y %b"
)

Consumer_Confidence$Date <- toupper(
  trimws(Consumer_Confidence$Date)
)

# =====================================
# Reset Row Names
# =====================================

rownames(Monthly) <- NULL
rownames(Wages) <- NULL
rownames(Unemployment) <- NULL
rownames(Energy_Prices) <- NULL
rownames(Consumer_Confidence) <- NULL

# =====================================
# Merge Data
# =====================================

Data <- merge(
  Monthly,
  Wages,
  by = "Date"
)

Data <- merge(
  Data,
  Unemployment,
  by = "Date"
)

Data <- merge(
  Data,
  Energy_Prices,
  by = "Date"
)

Data <- merge(
  Data,
  Consumer_Confidence,
  by = "Date"
)

# =====================================
# Check Merge
# =====================================

print(dim(Data))

print(head(Data))

# =====================================
# Convert Variables Properly
# =====================================

Data$CPI <- as.numeric(
  as.character(Data$CPI)
)

Data$Wage_Growth <- as.numeric(
  as.character(Data$Wage_Growth)
)

Data$Unemployment <- as.numeric(
  as.character(Data$Unemployment)
)

Data$Energy_Prices <- as.numeric(
  as.character(Data$Energy_Prices)
)

Data$Consumer_Confidence <- as.numeric(
  as.character(Data$Consumer_Confidence)
)

# =====================================
# Check Missing Values
# =====================================

print(colSums(is.na(Data)))

print(summary(Data))

# =====================================
# Correlation Matrix
# =====================================

Correlation <- cor(
  Data[, c(
    "CPI",
    "Wage_Growth",
    "Unemployment",
    "Energy_Prices",
    "Consumer_Confidence"
  )],
  use = "complete.obs"
)

print(Correlation)

# =====================================
# Save Final Dataset
# =====================================

write.csv(
  Data,
  "Data_Final.csv",
  row.names = FALSE
)

# =====================================
# Create Post-COVID Dummy
# =====================================

Data$Post_COVID <- ifelse(
  Data$Date >= "2020 MAR",
  1,
  0
)

print(table(Data$Post_COVID))

# =====================================
# Baseline Regression
# =====================================

Model1 <- lm(
  CPI ~ Wage_Growth +
    Unemployment +
    Energy_Prices +
    Consumer_Confidence,
  data = Data
)

summary(Model1)

# =====================================
# Diagnostic Plots
# =====================================

plot(Model1)

# =====================================
# Robust Standard Errors
# =====================================

coeftest(
  Model1,
  vcov = vcovHC(
    Model1,
    type = "HC1"
  )
)

# =====================================
# VIF Test
# =====================================

vif(Model1)

# =====================================
# Split Pre/Post COVID
# =====================================

Pre_COVID <- subset(
  Data,
  Post_COVID == 0
)

Post_COVID_Data <- subset(
  Data,
  Post_COVID == 1
)

# =====================================
# Pre-COVID Regression
# =====================================

model_pre <- lm(
  CPI ~ Wage_Growth +
    Unemployment +
    Energy_Prices +
    Consumer_Confidence,
  data = Pre_COVID
)

summary(model_pre)

# =====================================
# Post-COVID Regression
# =====================================

model_post <- lm(
  CPI ~ Wage_Growth +
    Unemployment +
    Energy_Prices +
    Consumer_Confidence,
  data = Post_COVID_Data
)

summary(model_post)

# =====================================
# Interaction Model
# =====================================

model_interaction <- lm(
  CPI ~ Wage_Growth +
    Unemployment +
    Energy_Prices +
    Consumer_Confidence +
    Wage_Growth:Post_COVID +
    Unemployment:Post_COVID +
    Energy_Prices:Post_COVID +
    Consumer_Confidence:Post_COVID,
  data = Data
)

summary(model_interaction)

# =====================================
# Durbin-Watson Test
# =====================================

dwtest(model_interaction)

# =====================================
# Newey-West Standard Errors
# =====================================

coeftest(
  model_interaction,
  vcov = NeweyWest(model_interaction)
)

# =====================================
# Stationarity Test
# =====================================

adf.test(Data$CPI)

# =====================================
# Differencing Variables
# =====================================

Data$dCPI <- c(
  NA,
  diff(Data$CPI)
)

Data$dWage_Growth <- c(
  NA,
  diff(Data$Wage_Growth)
)

Data$dUnemployment <- c(
  NA,
  diff(Data$Unemployment)
)

Data$dEnergy_Prices <- c(
  NA,
  diff(Data$Energy_Prices)
)

Data$dConsumer_Confidence <- c(
  NA,
  diff(Data$Consumer_Confidence)
)

# =====================================
# Differenced ADF Test
# =====================================

adf.test(
  na.omit(Data$dCPI)
)

# =====================================
# Differenced Regression
# =====================================

model_diff <- lm(
  dCPI ~ dWage_Growth +
    dUnemployment +
    dEnergy_Prices +
    dConsumer_Confidence,
  data = Data
)

summary(model_diff)

# =====================================
# Dynamic Inflation Model
# =====================================

Data$Lag_CPI <- c(
  NA,
  head(Data$CPI, -1)
)

model_dynamic <- lm(
  CPI ~ Lag_CPI +
    Wage_Growth +
    Unemployment +
    Energy_Prices +
    Consumer_Confidence,
  data = Data
)

summary(model_dynamic)

# =====================================
# RESET Test
# =====================================


# =====================================
# RESET TEST
# =====================================

resettest(model_dynamic)


# =====================================
# CREATE TIME SERIES OBJECT
# =====================================

cpi_ts <- ts(
  Data$CPI,
  start = c(2015, 1),
  frequency = 12
)

print(cpi_ts)


# =====================================
# TRAIN / TEST SPLIT
# =====================================

train <- window(
  cpi_ts,
  end = c(2023, 12)
)

test <- window(
  cpi_ts,
  start = c(2024, 1)
)

print(train)

print(test)


# =====================================
# FIT ARIMA FORECAST MODEL
# =====================================

arima_model <- auto.arima(train)

summary(arima_model)


# =====================================
# GENERATE CPI FORECASTS
# =====================================

forecast_cpi <- forecast(
  arima_model,
  h = length(test)
)

print(forecast_cpi)


# =====================================
# BASE R FORECAST PLOT
# =====================================

plot(forecast_cpi)

lines(test)


# =====================================
# FORECAST ACCURACY METRICS
# =====================================

accuracy(
  forecast_cpi,
  test
)


# =====================================
# CREATE FORECAST DATAFRAME
# =====================================

forecast_df <- data.frame(
  
  Date = seq(
    as.Date("2024-01-01"),
    by = "month",
    length.out = length(test)
  ),
  
  Actual = as.numeric(test),
  
  Forecast = as.numeric(forecast_cpi$mean),
  
  Lower_95 = as.numeric(
    forecast_cpi$lower[,2]
  ),
  
  Upper_95 = as.numeric(
    forecast_cpi$upper[,2]
  )
)

print(head(forecast_df))


# =====================================
# LOAD GGPLOT2
# =====================================

library(ggplot2)


# =====================================
# PROFESSIONAL FORECAST VISUALISATION
# =====================================

ggplot(
  forecast_df,
  aes(x = Date)
) +
  
  geom_ribbon(
    aes(
      ymin = Lower_95,
      ymax = Upper_95
    ),
    fill = "skyblue",
    alpha = 0.25
  ) +
  
  geom_line(
    aes(
      y = Forecast,
      colour = "Forecast"
    ),
    linewidth = 1.2
  ) +
  
  geom_line(
    aes(
      y = Actual,
      colour = "Actual Inflation"
    ),
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  scale_colour_manual(
    values = c(
      "Forecast" = "blue",
      "Actual Inflation" = "red"
    )
  ) +
  
  labs(
    title = "UK Inflation Forecast Using ARIMA Model",
    
    subtitle = "Forecasted vs Actual CPI Inflation (2024–2025)",
    
    x = "Date",
    
    y = "Inflation Rate (%)",
    
    colour = "Series",
    
    caption = "Source: ONS CPI Data and Author's ARIMA Forecast"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 12
    ),
    
    legend.position = "bottom"
  )





