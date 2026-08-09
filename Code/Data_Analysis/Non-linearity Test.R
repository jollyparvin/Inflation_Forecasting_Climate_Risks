library(nonlinearTseries)
library(openxlsx)
library(readxl)

# Numeric variables (excluding Date column)
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
indep_vars <- data_ts[, 5:ncol(data_ts)]

# All numeric variables
num_data <- data_ts[, -1]

nonlinear_results <- data.frame(
  Variable = character(),
  Tsay_F = numeric(),
  Tsay_p = numeric(),
  Tsay_Decision = character(),
  Keenan_F = numeric(),
  Keenan_p = numeric(),
  Keenan_Decision = character(),
  Final_Conclusion = character(),
  stringsAsFactors = FALSE
)

for(v in names(num_data)){

  res <- nonlinearityTest(ts(num_data[[v]]), verbose = FALSE)

  tsay_p <- res$Tsay$p.value
  keenan_p <- res$Keenan$p.value

  nonlinear_results <- rbind(
    nonlinear_results,
    data.frame(
      Variable = v,
      Tsay_F = res$Tsay$test.stat,
      Tsay_p = tsay_p,
      Tsay_Decision =
        ifelse(tsay_p < 0.05,
               "Reject H0 (Nonlinear)",
               "Fail to Reject H0 (Linear)"),

      Keenan_F = res$Keenan$test.stat,
      Keenan_p = keenan_p,
      Keenan_Decision =
        ifelse(keenan_p < 0.05,
               "Reject H0 (Nonlinear)",
               "Fail to Reject H0 (Linear)"),

      Final_Conclusion =
        if(is.na(tsay_p) || is.na(keenan_p)){
  final <- "NA"
} else if(tsay_p < 0.05 && keenan_p < 0.05){
  final <- "Strong evidence of nonlinearity"
} else if(tsay_p >= 0.05 && keenan_p >= 0.05){
  final <- "No evidence of nonlinearity"
} else{
  final <- "Mixed evidence of nonlinearity"
}
    )
  )
}

nonlinear_results