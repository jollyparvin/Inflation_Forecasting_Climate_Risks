#=========================
# Test for Endogeneity
# Wu-Hausman & Sargan Test
#=========================

if(!require(AER)) install.packages("AER")
if(!require(dplyr)) install.packages("dplyr")
if(!require(readxl)) install.packages("readxl")

library(AER)
library(dplyr)
library(readxl)

#-------------------------
# Read Data
#-------------------------
data <- read_excel("C:/Users/DELL/OneDrive/Desktop/Data_IGC.xlsx")
data <- as.data.frame(data)

data[-1] <- data.frame(
  lapply(data[-1], function(x)
    as.numeric(gsub(",", "", trimws(as.character(x)))))
)

#-------------------------
# Output Directory
#-------------------------
output_dir <- "C:/Users/DELL/OneDrive/Desktop/Endogeneity_Test"

dir.create(output_dir,
           showWarnings = FALSE,
           recursive = TRUE)

#-------------------------
# Function
#-------------------------
analyze_model <- function(dep_var){

  message("-------------------------------------")
  message("Processing: ", dep_var)

  # OLS Formula
  base_formula <- as.formula(
    paste(dep_var,
          "~ IIP_Gap + CallMoney + Brent + ExRate_USD")
  )

  # IV Formula
  iv_formula <- as.formula(
    paste(dep_var,
          "~ IIP_Gap + CallMoney + Brent + ExRate_USD |",
          "Num_GeoHydMetClimate +",
          "Ttl_Dmge_GHMC +",
          "Area_Weighted_Pct_Tail_Heat +",
          "Area_Weighted_Pct_Tail_Flood +",
          "Area_Weighted_Pct_Tail_Drought")
  )

  # OLS
  ols_fit <- lm(base_formula,
                data = data)

  # IV
  iv_fit <- ivreg(iv_formula,
                  data = data)

  # Diagnostics
  diag <- summary(iv_fit,
                  diagnostics = TRUE)$diagnostics

  # Wu-Hausman
  wu_hausman <- diag["Wu-Hausman", ]

  # Sargan
  sargan <- if("Sargan" %in% rownames(diag))
    diag["Sargan", ]
  else
    rep(NA, ncol(diag))

  # Weak Instrument Test
  weak_instr <- diag[grepl("Weak instruments",
                           rownames(diag)),
                     c("statistic","p-value"),
                     drop = FALSE]

  # Return Results
  list(
    Dependent_Variable = dep_var,
    Wu_Hausman_Statistic = wu_hausman["statistic"],
    Wu_Hausman_Pvalue = wu_hausman["p-value"],
    Sargan_Statistic = sargan["statistic"],
    Sargan_Pvalue = sargan["p-value"],
    Weak_Instrument_Statistic =
      if(nrow(weak_instr)>0) weak_instr[1,"statistic"] else NA,
    Weak_Instrument_Pvalue =
      if(nrow(weak_instr)>0) weak_instr[1,"p-value"] else NA,
    OLS_R2 = summary(ols_fit)$r.squared,
    IV_R2 = summary(iv_fit)$r.squared
  )

}

#-------------------------
# Models
#-------------------------
dep_vars <- c(
  "CPI_energy_inf",
  "CPI_food_inf",
  "CPI_Inf"
)

results_list <- lapply(dep_vars,
                       analyze_model)

results_df <- bind_rows(lapply(results_list,
                               as.data.frame))

#-------------------------
# Save CSV
#-------------------------
csv_path <- file.path(output_dir,
                      "Wu_Hausman_Sargan_Results_India.csv")

write.csv(results_df,
          csv_path,
          row.names = FALSE)

print(results_df)

#-------------------------
# Interpretation
#-------------------------
cat("\n==============================\n")
cat("Wu-Hausman Endogeneity Test Results\n")
cat("==============================\n")

for(i in seq_along(results_list)){

  r <- results_list[[i]]

  cat(sprintf("\nDependent Variable: %s\n",
              r$Dependent_Variable))

  cat(sprintf("Wu-Hausman Statistic = %.3f | p-value = %.5f\n",
              as.numeric(r$Wu_Hausman_Statistic),
              as.numeric(r$Wu_Hausman_Pvalue)))

  if(!is.na(r$Sargan_Statistic)){
    cat(sprintf("Sargan Statistic = %.3f | p-value = %.5f\n",
                as.numeric(r$Sargan_Statistic),
                as.numeric(r$Sargan_Pvalue)))
  }else{
    cat("Sargan Statistic = NA\n")
  }

  if(r$Wu_Hausman_Pvalue < 0.10){
    cat("=> Endogeneity detected (Reject H0)\n")
  }else{
    cat("=> No evidence of endogeneity (Fail to reject H0)\n")
  }

}