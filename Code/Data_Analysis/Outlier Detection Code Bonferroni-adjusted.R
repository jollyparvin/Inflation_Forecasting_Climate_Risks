library(car)
library(readxl)


data <- read_excel("C:/Users/DELL/OneDrive/Desktop/Data_IGC.xlsx")
data<- as.data.frame(data)

data[-1] <- data.frame(
  lapply(data[-1], function(x) as.numeric(gsub(",", "", trimws(as.character(x)))))
)

colnames(data) <- trimws(gsub("[\r\n]", "", colnames(data)))

data_ts <- na.omit(data)
str(data_ts)

data_ts$Date <- as.Date(paste0(data_ts$Year_Month, "-01"))

target_vars <- data_ts[, 2:4]
indep_vars <- data_ts[, 5:ncol(data_ts)]

# All numeric variables
num_data <- data_ts[, -1]


outlier_results <- data.frame(
  Variable = character(),
  Outlier_Count = integer(),
  Outlier_Indices = character(),
  stringsAsFactors = FALSE
)

for(v in names(num_data)){

  # Create time series
  ts_var <- ts(
    num_data[[v]],
    start = 1996,
    end = 2025,
    frequency = 12
  )

  # Time index
  time <- 1:length(ts_var)

  # Fit model
  fit <- lm(ts_var ~ time)

  # Bonferroni-adjusted outlier test
  test_out <- outlierTest(
    fit,
    cutoff = 0.05,
    n.max = Inf,
    order = TRUE
    )

  # Count outliers
  if(is.null(test_out)){
    count <- 0
    indices <- NA
  } else {
    count <- length(test_out$rstudent)
    indices <- paste(names(test_out$rstudent), collapse = ", ")
  }

  # Save results
  outlier_results <- rbind(
    outlier_results,
    data.frame(
      Variable = v,
      Outlier_Count = count,
      Outlier_Indices = indices,
      stringsAsFactors = FALSE
    )
  )
}

outlier_results

# Install once (if needed)
install.packages("openxlsx")

library(openxlsx)

write.xlsx(
  outlier_results,
  file = "Bonferroni_adjusted_outlier_results.xlsx",
  rowNames = FALSE
)