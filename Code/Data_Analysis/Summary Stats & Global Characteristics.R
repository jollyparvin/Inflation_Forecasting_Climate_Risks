install.packages('entropy')
install.packages('nonlinearTseries')
install.packages('readxl')
install.packages('e1071')
install.packages('tseries')
install.packages('pracma')
install.packages('car')
install.packages('seastests')
install.packages("openxlsx")  
library(openxlsx)

# Required Libraries
library(entropy)
library(e1071)

library(seastests)
library(nonlinearTseries)
library(pracma)
library(tseries)
library(car)
library(readxl)

#---------read the data---------------
data <- read_excel("C:/Users/DELL/OneDrive/Desktop/Data_IGC.xlsx")
data<- as.data.frame(data)

data[-1] <- data.frame(
  lapply(data[-1], function(x) as.numeric(gsub(",", "", trimws(as.character(x)))))
)

colnames(data) <- trimws(gsub("[\r\n]", "", colnames(data)))

data_ts <- na.omit(data)
str(data_ts)

#data_ts$Date <- as.Date(paste0(data_ts$Year_Month, "-01"))

target_vars <- data_ts[, 2:4]
#indep_vars <- data_ts[, 5:ncol(data_ts)]
indep_vars <- data_ts[, 5:13]

# All numeric variables
num_data <- data_ts[, -1]

########################### SUMMARY STATISTICS ###########################

# Coefficient of Variation (%)
cv_values <- sapply(num_data, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100)

# Standard Deviation
std_values <- sapply(num_data, function(x) sd(x, na.rm = TRUE))

# Entropy
entropy_values_MM <- sapply(num_data, function(x) entropy(x, method = "MM"))
 

sapply(num_data, function(x) any(x <= 0, na.rm = TRUE))
# Summary Statistics
min_values  <- sapply(num_data, min, na.rm = TRUE)
max_values  <- sapply(num_data, max, na.rm = TRUE)
mean_values <- sapply(num_data, mean, na.rm = TRUE)

# Quartiles
Q1_values <- sapply(num_data, function(x) quantile(x, probs = 0.25, na.rm = TRUE))
Q2_values <- sapply(num_data, function(x) quantile(x, probs = 0.50, na.rm = TRUE))  # Median
Q3_values <- sapply(num_data, function(x) quantile(x, probs = 0.75, na.rm = TRUE))
Q4_values <- max_values  # Maximum


entropy_values_ML <- sapply(num_data, function(x) {
  h <- hist(x, breaks = 10, plot = FALSE)
  p <- h$counts / sum(h$counts)
  entropy(p[p > 0], method = "ML")
})
entropy_values_ML

max_entropy <- log(10)

spectral_entropy_norm <- function(x) {

  x <- na.omit(as.numeric(x))

  x <- x - mean(x)

  sp <- spectrum(x, plot = FALSE)

  p <- sp$spec / sum(sp$spec)

  H <- -sum(p * log(p))

  H_norm <- H / log(length(p))

  return(H_norm)
}
spectral_entropy_values <- sapply(num_data, spectral_entropy_norm)

spectral_forecastability <- function(x) {

  x <- na.omit(as.numeric(x))
  x <- x - mean(x)

  sp <- spectrum(x, plot = FALSE)

  p <- sp$spec / sum(sp$spec)

  H <- -sum(p * log(p))

  H_norm <- H / log(length(p))

  Forecastability <- 1 - H_norm

  c(
    Spectral_Entropy = H_norm,
    Forecastability = Forecastability
  )
}
results <- t(sapply(num_data, spectral_forecastability))

results

results_summary <- data.frame(
  Variable         = colnames(num_data),
  Min              = min_values,
  Q1               = Q1_values,
  Q2               = Q2_values,
  Q3               = Q3_values,
  Q4               = Q4_values,
  Mean             = mean_values,
  SD               = std_values,
  CoV              = cv_values,
  Entropy_MM          = entropy_values_MM,
  Entropy_ML          = entropy_values_ML,
  SpectralEntropy  = results[, "Spectral_Entropy"],
  Forecastability  = results[, "Forecastability"],
  row.names = NULL
)

print(results_summary)

write.xlsx(
  results_summary,
  file = "C:/Users/DELL/OneDrive/Desktop/Research_Project/Results/Summary_Statistics_Final.xlsx",
  rowNames = FALSE
)

-----------------------------
########################### GLOBAL STATISTICS ###########################

install.packages("moments")
install.packages("forecast")
install.packages("urca")
library(urca)
library(forecast)
library(moments)

skew_values <- sapply(num_data, function(x)
  skewness(na.omit(x)))

kurt_values <- sapply(num_data, function(x)
  kurtosis(na.omit(x)))

seasonality_test <- sapply(num_data, function(x){

  x <- ts(na.omit(x), frequency = 12)

  isSeasonal(x)

})

seasonality_test

library(tseries)

kpss_results <- lapply(num_data, function(x){

  x <- na.omit(as.numeric(x))

  test <- kpss.test(x, null = "Level")

  data.frame(
    Statistic = as.numeric(test$statistic),
    P_Value   = test$p.value
  )

})

kpss_results <- do.call(rbind, kpss_results)
rownames(kpss_results) <- colnames(num_data)

kpss_results


hurst_values <- sapply(num_data, function(x){

  x <- na.omit(x)

  hurstexp(x)$Hs

})

outlier_count <- sapply(num_data, function(x){

  x <- na.omit(x)

  Q1 <- quantile(x,0.25)
  Q3 <- quantile(x,0.75)

  IQRv <- IQR(x)

  lower <- Q1 - 1.5*IQRv
  upper <- Q3 + 1.5*IQRv

  sum(x < lower | x > upper)

})

outlier_count


library(openxlsx)

# -----------------------------
# Interpretations
# -----------------------------

# Skewness interpretation
skew_interpretation <- sapply(skew_values, function(x){
  if(abs(x) < 0.5) {
    "Approximately Symmetric"
  } else if(x >= 0.5) {
    "Positively Skewed"
  } else {
    "Negatively Skewed"
  }
})

# Kurtosis interpretation
kurt_interpretation <- sapply(kurt_values, function(x){
  if(x < 3) {
    "Platykurtic"
  } else if(x > 3) {
    "Leptokurtic"
  } else {
    "Mesokurtic"
  }
})

# Seasonality interpretation
seasonality_interpretation <- ifelse(
  seasonality_test,
  "Seasonal",
  "Non-Seasonal"
)

# KPSS interpretation
kpss_interpretation <- ifelse(
  kpss_results$P_Value > 0.05,
  "Stationary",
  "Non-Stationary"
)

# Hurst interpretation
hurst_interpretation <- sapply(hurst_values, function(h){
  if(h < 0.5){
    "Mean-Reverting"
  } else if(abs(h - 0.5) < 0.05){
    "Random Walk"
  } else if(h < 0.7){
    "Persistent"
  } else {
    "Strong Long Memory"
  }
})

# -----------------------------
# Final Diagnostics Table
# -----------------------------

diagnostics_summary <- data.frame(
  Variable = colnames(num_data),

  Skewness = round(skew_values, 3),
  Skewness_Interpretation = skew_interpretation,

  Kurtosis = round(kurt_values, 3),
  Kurtosis_Interpretation = kurt_interpretation,

  Seasonal = seasonality_test,
  Seasonality_Interpretation = seasonality_interpretation,

  KPSS_Statistic = round(kpss_results$Statistic, 4),
  KPSS_PValue = round(kpss_results$P_Value, 4),
  KPSS_Interpretation = kpss_interpretation,

  Hurst = round(hurst_values, 3),
  Hurst_Interpretation = hurst_interpretation,

  Outlier_Count = outlier_count,

  row.names = NULL
)

print(diagnostics_summary)

# -----------------------------
# Export to Excel
# -----------------------------

write.xlsx(
  diagnostics_summary,
  file = "C:/Users/DELL/OneDrive/Desktop/Research_Project/Results/Global_Characteristics_Summary_Final.xlsx",
  rowNames = FALSE
)

cat("Excel file saved successfully.\n")

