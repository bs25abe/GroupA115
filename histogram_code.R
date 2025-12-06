# Movie Budget vs IMDb Rating - Correlation Analysis
# Following the Statistical Decision Tree
# FILTERS: Years 2002-2019, Budget ≤ $50 million

# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)

# Set high-resolution graphics parameters
options(repr.plot.width=12, repr.plot.height=8, repr.plot.res=300)

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
# STEP 1: CREATE HISTOGRAM (Dependent Variable Only)


cat("STEP 1: Checking for Normal Distribution\n")


# Calculate mean and standard deviation for normal curve
mean_rating <- mean(data_clean$avg_vote_cleaned)
sd_rating <- sd(data_clean$avg_vote_cleaned)

# Create histogram with normal curve overlay and borders
p_histogram <- ggplot(data_clean, aes(x = avg_vote_cleaned)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, 
                 fill = "#2E86AB", alpha = 0.7, color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean_rating, sd = sd_rating),
                color = "#A23B72", size = 1.5, linetype = "dashed") +
  labs(
    title = "Histogram: Average IMDb Rating Distribution",
    subtitle = "Blue bars = Observed data | Purple dashed line = Theoretical normal curve (for comparison)",
    x = "Average IMDb Rating (rating score)",
    y = "Density",
    caption = paste0("Mean = ", round(mean_rating, 2), 
                     " | SD = ", round(sd_rating, 2),
                     " | n = ", nrow(data_clean))
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(color = "gray30", size = 11, hjust = 0),
    panel.border = element_rect(fill = NA, color = "black", size = 1),
    axis.line = element_blank(),
    plot.caption = element_text(hjust = 0, color = "gray50")
  )

# Save histogram
dir.create("correlation_plots", showWarnings = FALSE)
ggsave("correlation_plots/1_histogram.png", p_histogram, 
       width = 12, height = 8, dpi = 300, bg = "white")


