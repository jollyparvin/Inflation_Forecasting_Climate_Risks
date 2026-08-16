############## VAR Model: India: 6M, 12M and 24M ahead - forecasts ####################

######################### VAR Model: INDIA: 12M ahead - forecasts ####################
# Load the relevant packages
required_pkgs <- c("vars", "readxl", "tseries", "forecast", "nnet", "dplyr", "tsDyn", "Metrics")
for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(readxl)
library(vars)
library(forecast)
library(nnet)
library(Metrics)
library(dplyr)
library(tsDyn)
library(tseries)

#Change the path accordingly
data <- read_excel("C:/Users/DELL/OneDrive/Desktop/Data_IGC.xlsx")
data<- as.data.frame(data)

data[-1] <- data.frame(
  lapply(data[-1], function(x) as.numeric(gsub(",", "", trimws(as.character(x)))))
)

colnames(data) <- trimws(gsub("[\r\n]", "", colnames(data)))

df <- na.omit(data)
str(df)

df$Ttl_Dmge_GHMC <- log1p(df$Ttl_Dmge_GHMC)
# -----------------------------
# 2) Variable blocks
# -----------------------------
inflation_vars <- c("CPI_Inf", "CPI_food_inf", "CPI_energy_inf")

response_labels <- c(
  CPI_Inf        = "CPI Inflation",
  CPI_food_inf   = "CPI Food Inflation",
  CPI_energy_inf = "CPI Energy Inflation"
)

climate_all <- c(
  "Num_GeoHydMetClimate",
  "Ttl_Dmge_GHMC",
  "Area_Weighted_Pct_Tail_Heat",
  "Area_Weighted_Pct_Tail_Flood"
  "Area_Weighted_Pct_Tail_Drought"
  )

df$Date <- as.Date(paste0(df$Year_Month, "-01"))

# Creation of Train, test and validation dataset
# Creation of train and test data
df.train = df[1:336,]
df.validation = df[337:348,]
df.test = df[349:360,]
df.full.train = df[1:348,]

str(df.train)


required_cols <- unique(c(inflation_vars, climate_all))
str(required_cols)
df.train.required <- df.train[, required_cols]
df.validation.required <- df.validation[, required_cols]
df.test.required <- df.test[, required_cols]
df.full.train.required <- df.full.train[, required_cols]

# Check for stationarity
for(col in required_cols){
  cat("\n==========", col, "==========\n")
  print(kpss.test(df.full.train.required[[col]], null = "Level"))
}
#-------------------------------



var.12M.full.train.endog <- df.full.train.required[1:348,1:3]
var.12M.full.train.exog <- df.full.train.required[1:348,4:8]
str(var.12M.full.train.exog)
str(var.12M.full.train.endog)

# Choice of Lag for the VAR Model - Use the final train dataset to choose the order of the lag
# Create a data set with only endogenous variables
varic <- VARselect(var.12M.full.train.endog, lag.max = 6, type = "const")
print(varic$criteria)

paic <- as.integer(varic$selection["AIC(n)"])
psc  <- as.integer(varic$selection["SC(n)"])

puse <- max(1, min(paic, 3))
puse

# Fitting the lineVAR model on the full train dataset with exogenous drivers
var.full.train.12m.endog.exog <- lineVar(
  data    = var.12M.full.train.endog,
  lag     = puse,
  include = "const",
  model   = "VAR",
  beta = NULL
  exogen  = var.12M.full.train.exog
)

summary(var.full.train.12m.endog.exog)

# Generate Forecast for the test horizon using the VAR Model
pred.var.12m <- predict(
  var.full.train.12m.endog.exog,
  n.ahead = 12
  exoPred = df.full.train.required[337:348,4:6]
)

pred.var.12m.df <- as.data.frame(pred.var.12m)

View(pred.var.12m.df)

install.packages("openxlsx")
library(openxlsx)
#---------------------------
######################### VAR Model: INDIA: 24M ahead - forecasts ####################


df.train = df[1:312,]
df.validation = df[313:336,]
df.test = df[337:360,]
df.full.train = df[1:336,]

str(df.train)


required_cols <- unique(c(inflation_vars, climate_all))
str(required_cols)
df.train.required <- df.train[, required_cols]
df.validation.required <- df.validation[, required_cols]
df.test.required <- df.test[, required_cols]
df.full.train.required <- df.full.train[, required_cols]

for(col in required_cols){
  cat("\n==========", col, "==========\n")
  print(kpss.test(df.full.train.required[[col]], null = "Level"))
}
#-------------------------------


#endogenous variables
var.24M.full.train.endog <- df.full.train.required[1:336,1:3]
var.24M.full.train.exog <- df.full.train.required[1:336,4:8]
str(var.24M.full.train.exog)
str(var.24M.full.train.endog)

varic <- VARselect(var.24M.full.train.endog, lag.max = 6, type = "const")
print(varic$criteria)

paic <- as.integer(varic$selection["AIC(n)"])
psc  <- as.integer(varic$selection["SC(n)"])

puse <- max(1, min(paic, 3))
puse



var.full.train.24m.endog.exog <- lineVar(
  data    = var.24M.full.train.endog,
  lag     = puse,
  include = "const",
  model   = "VAR",
  beta = NULL
#  exogen  = var.24M.full.train.exog
)

summary(var.full.train.24m.endog.exog)

pred.var.24m <- predict(
  var.full.train.24m.endog.exog,
  n.ahead = 24
#  exoPred = df.full.train.required[313:336,4:6]
)

pred.var.24m.df <- as.data.frame(pred.var.24m)

View(pred.var.24m.df)
--------------------------------
######################### VAR Model: INDIA: 6M ahead - forecasts ####################

df.train = df[1:348,]
df.validation = df[349:354,]
df.test = df[355:360,]
df.full.train = df[1:354,]

str(df.train)


required_cols <- unique(c(inflation_vars, climate_all))
str(required_cols)
df.train.required <- df.train[, required_cols]
df.validation.required <- df.validation[, required_cols]
df.test.required <- df.test[, required_cols]
df.full.train.required <- df.full.train[, required_cols]

for(col in required_cols){
  cat("\n==========", col, "==========\n")
  print(kpss.test(df.full.train.required[[col]], null = "Level"))
}
#-------------------------------

var.6M.full.train.endog <- df.full.train.required[1:354,1:3]
var.6M.full.train.exog <- df.full.train.required[1:354,4:8]
str(var.6M.full.train.exog)

varic <- VARselect(var.6M.full.train.endog, lag.max = 6, type = "const")
print(varic$criteria)

paic <- as.integer(varic$selection["AIC(n)"])
psc  <- as.integer(varic$selection["SC(n)"])

puse <- max(1, min(paic, 3))
puse


var.full.train.6m.endog.exog <- lineVar(
  data    = var.6M.full.train.endog,
  lag     = puse,
  include = "const",
  model   = "VAR",
  beta = NULL
#  exogen  = var.6M.full.train.exog
)

summary(var.full.train.6m.endog.exog)

pred.var.6m <- predict(
  var.full.train.6m.endog.exog,
  n.ahead = 6
 # exoPred = df.full.train.required[349:354,4:6]
)

pred.var.6m.df <- as.data.frame(pred.var.6m)

View(pred.var.6m.df)


wb <- createWorkbook()

addWorksheet(wb, "6M")
writeData(wb, "6M", pred.var.6m)

addWorksheet(wb, "12M")
writeData(wb, "12M", pred.var.12m)

addWorksheet(wb, "24M")
writeData(wb, "24M", pred.var.24m)

saveWorkbook(wb,
             "VAR_Results_without_exog.xlsx",
             overwrite = TRUE)
--------------------------------------------------------
#metrices

forecast_metrics <- function(actual, predicted){

  vars <- colnames(actual)

  results <- data.frame(
    Variable = vars,
    RMSE = NA,
    MAE = NA
  )

  for(i in seq_along(vars)){

    a <- actual[[i]]
    p <- predicted[[i]]

    results$RMSE[i] <- sqrt(mean((a - p)^2, na.rm = TRUE))
    results$MAE[i]  <- mean(abs(a - p), na.rm = TRUE)

  }

  return(results)
}

metrics.6m  <- forecast_metrics(actual.6m, pred.6m)
metrics.12m <- forecast_metrics(actual.12m, pred.12m)
metrics.24m <- forecast_metrics(actual.24m, pred.24m)

#add horizon
metrics.6m$Forecast_Horizon  <- "6M"
metrics.12m$Forecast_Horizon <- "12M"
metrics.24m$Forecast_Horizon <- "24M"

forecast.results.var <- rbind(
  metrics.6m,
  metrics.12m,
  metrics.24m
)

forecast.results.var

#library(openxlsx)

write.xlsx(
  forecast.results.var,
  file = "C:/Users/DELL/OneDrive/Desktop/Research_Project/Results/VAR_Metrices_All_.xlsx",
  rowNames = FALSE
)
