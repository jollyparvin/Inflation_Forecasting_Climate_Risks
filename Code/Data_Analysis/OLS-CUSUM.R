############### Check for Structural Breaks: OLS-CUSUM test : India countries ######################
# ========================= Final Stacked Chart Generation ======================

install.packages(c(
  "strucchange",
  "ggplot2",
  "zoo",
  "cowplot",
  "gtable",
  "readxl",
  "patchwork",
  "openxlsx"
))

library(strucchange)
library(ggplot2)
library(zoo)
library(cowplot)
library(readxl)

#data file names
info <- list(
  India  = list(path = ".",  file = "Data_IGC.xlsx")
)

main_dir <- "C:\\Users\\Sunny kumar\\Desktop\\Jolly\\Dataset\\"

out_dir <- file.path(main_dir, "output")

var_names <- c("CPI_Inf", "CPI_food_inf", "CPI_energy_inf", "IIP_Gap", "CallMoney", "Brent",  "ExRate_USD")
pretty_labels <- c("Headline_Inflation", "Food_Inflation", "Energy_Inflation", "IIP_Gap", "CallMoney", "Oil_Price", "ExRate")

# Custom plot function: no axis labels, no legend, small font
create_cusum_plot <- function(ts_var, time_index, bp, var_name, country_name) {
  cusum <- efp(ts_var ~ breakfactor(bp), type = "OLS-CUSUM")
  process <- as.numeric(cusum$process)
  n <- length(process)
  bound <- as.numeric(strucchange:::boundary.efp(cusum, alpha = 0.05, alt.boundary = FALSE, functional = "max"))
  if (is.ts(ts_var)) {
    time_seq <- as.yearmon(time(ts_var))
    date_seq <- as.Date(time_seq, frac = 0)
    date_seq <- date_seq[1:n]
  } else {
    date_seq <- 1:n
  }
  process_data <- data.frame(
    Time = date_seq,
    Score = process,
    UpperBound = bound,
    LowerBound = -bound
  )
  p <- ggplot(process_data, aes(x = Time)) +
    geom_line(aes(y = Score), color = "blue", size = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(aes(y = UpperBound), linetype = "dashed", color = "red", size = 0.6) +
    geom_line(aes(y = LowerBound), linetype = "dashed", color = "red", size = 0.6) +
    ggtitle(bquote(bold(.(paste0(var_name))))) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(size = 7, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 6),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = margin(2, 2, 2, 2),
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6),
      legend.position = "none"
    )
  return(p)
}

# Collect all plots in a list: 2 rows 4 columns, 7 plots
all_plots <- list()
country_names <- names(info)
for (row in seq_along(country_names)) {
  country <- country_names[row]
  data_file <- file.path(
    main_dir,
    info[[country]]$path,
    info[[country]]$file
  )

  var_data <- read_excel(data_file)
  var_data$Date <- as.Date(
    paste0(var_data$Year_Month,"-01")
  )
  var_data$date_n <- as.numeric(var_data$Date)
  var_data <- var_data[1:336,]
  time <- c(1:336)
  ts_list <- lapply(var_names, function(v) ts(var_data[[v]], start = c(1996,1), end = c(2023,12), frequency = 12))
  bp_list <- lapply(ts_list, function(tsv) breakpoints(tsv ~ time, h = 12))
  for (col in 1:length(var_names)) {
    all_plots[[length(all_plots) + 1]] <- create_cusum_plot(
      ts_list[[col]], time, bp_list[[col]],
      pretty_labels[col], country
    )
  }
}

# Arrange all plots in a 7x5 grid with cowplot
main_grid <- cowplot::plot_grid(
  plotlist = all_plots,
  nrow = 2, ncol = 4,
  align = "hv",
  axis = "tblr",
  rel_heights = rep(1, 7),
  rel_widths = rep(1, 5)
)

# Add global axis labels (smaller font, x label moved up, overall plot slightly smaller)
final_plot <- cowplot::ggdraw() +
  #cowplot::draw_plot(main_grid, 0, 0, 1, 1) +
  cowplot::draw_plot(main_grid, 0.05, 0.08, 0.93, 0.90) +
   cowplot::draw_label(
     "Empirical Fluctuation Process Score (OLS-residuals)",
     x = 0.008, y = 0.5, angle = 90, vjust = 0.5, hjust = 0.5, fontface = "bold", size = 7
   ) +
   cowplot::draw_label(
     "Time horizon (Training)",
     x = 0.5, y = 0.03, angle = 0, vjust = 0.5, hjust = 0.5, fontface = "bold", size = 7
   )

# Save as PNG and JPG (Overleaf-friendly, slightly smaller)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
png_file <- file.path(out_dir, "India_OLS_CUSUM_grid_paper.png")
jpg_file <- file.path(out_dir, "India_OLS_CUSUM_grid_revised.jpg")
ggsave(png_file, final_plot, width = 11.5, height = 13.5, dpi = 300)
ggsave(jpg_file, final_plot, width = 11.5, height = 13.5, dpi = 300)

# Show the plot in RStudio/interactive
print(final_plot)
####################### End of Code ###############################
