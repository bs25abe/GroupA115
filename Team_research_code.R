# Movie Budget vs IMDb Rating - Correlation Analysis
# Following the Statistical Decision Tree
# FILTERS: Years 2002-2019, Budget ≤ $50 million

# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)

# Set ULTRA high-resolution graphics parameters
options(repr.plot.width=16, repr.plot.height=10, repr.plot.res=600)

# Read the data
data <- read.csv("C:\\Users\\Bhavithra\\Downloads\\IMDb movies.csv", stringsAsFactors = FALSE)


# DATA CLEANING FUNCTION


clean_budget <- function(budget_col) {
  budget_col <- trimws(budget_col)
  budget_col <- as.character(budget_col)
  
  # Exchange rates to USD
  exchange_rates <- list(
    ITL = 0.00052, ROL = 0.000025, SEK = 0.095,
    FRF = 0.15, NOK = 0.092, GBP = 1.30, EUR = 1.10
  )
  
  cleaned <- sapply(budget_col, function(x) {
    if (is.na(x) || x == "" || x == "0") return(NA)
    
    for (currency in names(exchange_rates)) {
      if (grepl(paste0("^", currency), x)) {
        amount <- as.numeric(gsub(paste0("^", currency, "\\s*"), "", x))
        return(amount * exchange_rates[[currency]])
      }
    }
    
    if (grepl("\\$", x)) {
      amount <- as.numeric(gsub("[\\$,\\s]", "", x))
      return(amount)
    }
    
    amount <- as.numeric(gsub("[^0-9.]", "", x))
    return(amount)
  })
  
  return(as.numeric(cleaned))
}


# CLEAN THE DATA WITH FILTERS


data$budget_cleaned <- clean_budget(data$budget)
data$avg_vote_cleaned <- as.numeric(data$avg_vote)
data$year_cleaned <- as.numeric(data$year)

data_clean <- data %>%
  filter(!is.na(budget_cleaned) & !is.na(avg_vote_cleaned) & !is.na(year_cleaned)) %>%
  filter(budget_cleaned > 0) %>%
  filter(year_cleaned >= 2002 & year_cleaned <= 2019) %>%
  filter(budget_cleaned <= 50000000)  # Filter for budget ≤ $50 million

cat("\n=== DATA SUMMARY ===\n")
cat("Total observations:", nrow(data), "\n")
cat("Clean observations (2002-2019, budget ≤ $50M):", nrow(data_clean), "\n")
cat("Year range:", min(data_clean$year_cleaned), "-", max(data_clean$year_cleaned), "\n")
cat("Budget range: $", scales::comma(min(data_clean$budget_cleaned)), " - $", 
    scales::comma(max(data_clean$budget_cleaned)), "\n\n")

# STEP 1: CREATE HISTOGRAM (Dependent Variable Only) WITH FREQUENCY


cat("STEP 1: Checking for Normal Distribution\n")


# Calculate mean and standard deviation for normal curve
mean_rating <- mean(data_clean$avg_vote_cleaned)
sd_rating <- sd(data_clean$avg_vote_cleaned)

# Create histogram with FREQUENCY on y-axis and normal distribution curve
# Calculate bin width for the histogram
bin_width <- (max(data_clean$avg_vote_cleaned) - min(data_clean$avg_vote_cleaned)) / 30

p_histogram <- ggplot(data_clean, aes(x = avg_vote_cleaned)) +
  geom_histogram(bins = 30, fill = "#2E86AB", alpha = 0.7, color = "white") +
  stat_function(
    fun = function(x) dnorm(x, mean = mean_rating, sd = sd_rating) * nrow(data_clean) * bin_width,
    color = "red", 
    size = 1.5, 
    linetype = "solid"
  ) +
  labs(
    title = "Histogram: Average IMDb Rating Distribution",
    subtitle = "Blue bars = Observed data | Red curve = Theoretical distribution",
    x = "Average IMDb Rating (rating score)",
    y = "Frequency",
    caption = paste0("Mean = ", round(mean_rating, 2), 
                     " | SD = ", round(sd_rating, 2),
                     " | n = ", nrow(data_clean),
                     " | Years: 2002-2019 | Budget ≤ $50M")
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(color = "gray30", size = 11, hjust = 0),
    panel.border = element_rect(fill = NA, color = "black", size = 1),
    axis.line = element_blank(),
    plot.caption = element_text(hjust = 0, color = "gray50")
  )

# Save histogram with ULTRA HIGH resolution
dir.create("correlation_plots", showWarnings = FALSE)
ggsave("correlation_plots/1_histogram.png", p_histogram, 
       width = 16, height = 10, dpi = 600, bg = "white")

# NORMALITY TESTS (Using ALL Data)


cat("Testing normality using ALL", nrow(data_clean), "observations\n")
cat("Using Kolmogorov-Smirnov test (works with any sample size)\n\n")

# Kolmogorov-Smirnov test for budget
ks_budget <- ks.test(data_clean$budget_cleaned, "pnorm", 
                     mean = mean(data_clean$budget_cleaned), 
                     sd = sd(data_clean$budget_cleaned))

cat("Budget Variable:\n")
cat("  Kolmogorov-Smirnov D =", round(ks_budget$statistic, 4), "\n")
cat("  p-value =", format(ks_budget$p.value, digits = 5), "\n")

if (ks_budget$p.value < 0.05) {
  cat("  Result: NOT normally distributed (p < 0.05)\n\n")
  budget_normal <- FALSE
} else {
  cat("  Result: Appears normally distributed (p >= 0.05)\n\n")
  budget_normal <- TRUE
}

# Kolmogorov-Smirnov test for avg_vote
ks_vote <- ks.test(data_clean$avg_vote_cleaned, "pnorm", 
                   mean = mean(data_clean$avg_vote_cleaned), 
                   sd = sd(data_clean$avg_vote_cleaned))

cat("Avg_Vote Variable:\n")
cat("  Kolmogorov-Smirnov D =", round(ks_vote$statistic, 4), "\n")
cat("  p-value =", format(ks_vote$p.value, digits = 5), "\n")

if (ks_vote$p.value < 0.05) {
  cat("  Result: NOT normally distributed (p < 0.05)\n\n")
  vote_normal <- FALSE
} else {
  cat("  Result: Appears normally distributed (p >= 0.05)\n\n")
  vote_normal <- TRUE
}

# STEP 2: DECIDE WHICH CORRELATION TEST TO USE


cat("STEP 2: Selecting Appropriate Correlation Test\n")


both_normal <- budget_normal && vote_normal

if (both_normal) {
  cat("DECISION: Both variables are normally distributed\n")
  cat("→ Using PEARSON correlation test\n\n")
  
  test_result <- cor.test(data_clean$budget_cleaned, 
                          data_clean$avg_vote_cleaned, 
                          method = "pearson")
  test_name <- "Pearson's r"
  test_method <- "Pearson"
  
} else {
  cat("DECISION: At least one variable is NOT normally distributed\n")
  cat("→ Using SPEARMAN and KENDALL correlation tests (non-parametric)\n\n")
  
  # Run both Spearman and Kendall
  spearman_result <- cor.test(data_clean$budget_cleaned, 
                              data_clean$avg_vote_cleaned, 
                              method = "spearman")
  
  kendall_result <- cor.test(data_clean$budget_cleaned, 
                             data_clean$avg_vote_cleaned, 
                             method = "kendall")
  
  test_result <- spearman_result  # Primary test for plotting
  test_name <- "Spearman's ρ"
  test_method <- "Spearman & Kendall"
}

# STEP 3: CORRELATION TEST RESULTS


cat("STEP 3: Correlation Test Results\n")


if (both_normal) {
  # Pearson only
  cat(test_name, "=", round(test_result$estimate, 4), "\n")
  cat("p-value =", format(test_result$p.value, digits = 5), "\n")
  
  if (!is.null(test_result$conf.int)) {
    cat("95% CI: [", round(test_result$conf.int[1], 4), ",", 
        round(test_result$conf.int[2], 4), "]\n")
  }
} else {
  # Spearman and Kendall
  cat("SPEARMAN'S ρ =", round(spearman_result$estimate, 4), "\n")
  cat("  p-value =", format(spearman_result$p.value, digits = 5), "\n\n")
  
  cat("KENDALL'S τ =", round(kendall_result$estimate, 4), "\n")
  cat("  p-value =", format(kendall_result$p.value, digits = 5), "\n")
}

cat("\n")

# Determine correlation strength
coef_val <- abs(as.numeric(test_result$estimate))
if (coef_val < 0.3) {
  strength <- "WEAK"
} else if (coef_val < 0.7) {
  strength <- "MODERATE"
} else {
  strength <- "STRONG"
}

direction <- ifelse(test_result$estimate > 0, "positive", "negative")

# Statistical significance
if (test_result$p.value < 0.05) {
  cat("CONCLUSION: Statistically significant correlation (p < 0.05)\n")
  cat("Strength:", strength, direction, "correlation\n")
} else {
  cat("CONCLUSION: NO statistically significant correlation (p >= 0.05)\n")
  cat("Budget does NOT significantly influence IMDb ratings\n")
}

# STEP 4: CREATE SCATTERPLOT WITH IMPROVED AXES


cat("\n\nSTEP 4: Creating Scatterplot with Analysis Summary\n")


# Format p-value for display (avoid scientific notation)
if (test_result$p.value < 0.001) {
  p_value_text <- "< 0.001"
} else {
  p_value_text <- paste("=", round(test_result$p.value, 4))
}

# Create subtitle based on test type - REMOVED KENDALL FROM SUBTITLE
if (both_normal) {
  plot_subtitle <- paste0(test_name, " = ", round(test_result$estimate, 3), 
                          ", p-value ", p_value_text, " | ", strength, " ", direction, " correlation")
} else {
  # Only show Spearman in subtitle
  spearman_p <- ifelse(spearman_result$p.value < 0.001, "< 0.001", 
                       paste("=", round(spearman_result$p.value, 4)))
  
  plot_subtitle <- paste0(
    "Spearman's ρ = ", round(spearman_result$estimate, 3), " (p ", spearman_p, ") | ",
    strength, " ", direction, " correlation"
  )
}

# Create detailed summary note
if (both_normal) {
  summary_note <- paste0(
    "ANALYSIS SUMMARY:\n",
    "• Data Filters: Years 2002-2019, Budget ≤ $50 million\n",
    "• Data Distribution: Both variables are NORMALLY DISTRIBUTED\n",
    "  (Tested on ALL ", nrow(data_clean), " observations using Kolmogorov-Smirnov test)\n",
    "• Test Used: PEARSON CORRELATION\n",
    "• Pearson's r = ", round(test_result$estimate, 3), " (p ", p_value_text, ")\n",
    "• Correlation Strength: ", strength, " ", direction, " correlation\n",
    "• Interpretation: ", 
    ifelse(test_result$p.value < 0.05, 
           paste0("There is a statistically significant ", tolower(strength), " ", direction, " relationship"),
           "No statistically significant relationship found")
  )
} else {
  summary_note <- paste0(
    "ANALYSIS SUMMARY:\n",
    "• Data Filters: Years 2002-2019, Budget ≤ $50 million\n",
    "• Data Distribution: Variables are NOT NORMALLY DISTRIBUTED\n",
    "  (Tested on ALL ", nrow(data_clean), " observations using Kolmogorov-Smirnov test)\n",
    "• Tests Used: SPEARMAN & KENDALL CORRELATION (non-parametric)\n",
    "• Spearman's ρ = ", round(spearman_result$estimate, 3), " (p ", 
    ifelse(spearman_result$p.value < 0.001, "< 0.001", paste("=", round(spearman_result$p.value, 4))), ")\n",
    "• Kendall's τ = ", round(kendall_result$estimate, 3), " (p ", 
    ifelse(kendall_result$p.value < 0.001, "< 0.001", paste("=", round(kendall_result$p.value, 4))), ")\n",
    "• Correlation Strength: ", strength, " ", direction, " correlation\n",
    "• Interpretation: ", 
    ifelse(test_result$p.value < 0.05, 
           paste0("There is a statistically significant ", tolower(strength), " ", direction, " relationship"),
           "No statistically significant relationship found")
  )
}

cat(summary_note)
cat("\n\n")

# IMPROVED SCATTERPLOT with cleaner x-axis (no $, no M)
# Convert budget to millions for cleaner numbers
data_clean$budget_millions <- data_clean$budget_cleaned / 1000000

# Calculate nice break points for x-axis (5 million increments)
x_breaks <- seq(0, 50, by = 5)  # 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50

p_scatter <- ggplot(data_clean, aes(x = budget_millions, y = avg_vote_cleaned)) +
  geom_point(alpha = 0.5, color = "#2E86AB", size = 3) +
  geom_smooth(method = "lm", color = "#A23B72", fill = "#F18F01", 
              alpha = 0.2, size = 1.2) +
  # Clean x-axis with plain numbers
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_breaks,
    limits = c(0, 50),
    expand = c(0.02, 0.02)
  ) +
  # Clean y-axis with better breaks
  scale_y_continuous(
    breaks = seq(0, 10, by = 1),
    limits = c(floor(min(data_clean$avg_vote_cleaned)), 
               ceiling(max(data_clean$avg_vote_cleaned))),
    expand = c(0.02, 0.02)
  ) +
  labs(
    title = "Scatterplot: Movie Budget vs IMDb Average Rating",
    subtitle = plot_subtitle,
    x = "Budget (millions USD)",
    y = "Average IMDb Rating",
    caption = paste0("n = ", nrow(data_clean), " movies (2002-2019, budget ≤ $50M) | Test: ", test_method)
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(color = "gray30", size = 11, hjust = 0),
    panel.border = element_rect(fill = NA, color = "black", size = 1),
    axis.line = element_blank(),
    plot.caption = element_text(hjust = 0, color = "gray50"),
    # Improve axis text
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 13, face = "bold"),
    # Add minor grid lines for better readability
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_line(color = "gray95", size = 0.2)
  )

ggsave("correlation_plots/2_scatterplot.png", p_scatter, 
       width = 16, height = 10, dpi = 600, bg = "white")

# Save summary note as text file
writeLines(summary_note, "correlation_plots/ANALYSIS_SUMMARY.txt")

cat("✓ ULTRA HIGH-RESOLUTION plots saved to 'correlation_plots/' directory\n")
cat("  1. 1_histogram.png - Dependent variable (16x10 inches @ 600 DPI)\n")
cat("  2. 2_scatterplot.png - Correlation plot (16x10 inches @ 600 DPI)\n")
cat("  3. ANALYSIS_SUMMARY.txt - Complete analysis summary\n\n")

cat("=" , rep("=", 70), "\n", sep = "")
cat(summary_note)
cat("\n", rep("=", 71), "\n\n", sep = "")

# Display plots
print(p_histogram)
print(p_scatter)
