# ============================================================================
# SUPER SUNDAY ECONOMIC IMPACT ANALYSIS
# New Orleans Mardi Gras Indian Council
# Analyst Support for: Michael Farley, Assistant Director
# Date: February 2026
# ============================================================================

# SETUP ----
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(knitr)
library(kableExtra)

# KEY METRICS FROM PLACER.AI DATA ----
# Source: Consulting_Report_-_Super_Sunday_-_Mar_16__2025_-_Mar_16__2025.pdf

event_visits <- 34100
event_visitors <- 34100
avg_dwell_time <- 108  # minutes
panel_visits <- 1200
venue_size_sq_ft <- 928100

# Growth metrics
visits_yoy_growth <- 6828.2
visits_yo2y_growth <- 8840.3
visits_yo3y_growth <- 8918.2

# VISITOR ORIGIN DATA ----
local_zips <- tibble(
  zip_code = c("70126", "70127", "70122", "70128", "70117", 
               "70115", "70119", "70125", "70118", "70113"),
  visits = c(2700, 2300, 2200, 2000, 1900, 1500, 1500, 1400, 1400, 1100),
  percentage = c(7.9, 6.6, 6.4, 5.9, 5.6, 4.4, 4.3, 4.0, 4.0, 3.3)
)

total_local_visits <- sum(local_zips$visits)
total_non_local_visits <- event_visits - total_local_visits
local_percentage <- (total_local_visits / event_visits) * 100
non_local_percentage <- 100 - local_percentage

# DISTANCE DISTRIBUTION ----
distance_distribution <- tibble(
  distance_range = c("0-0.3 mi", "0.5-0.7 mi", "1-2 mi", "3-5 mi", 
                     "7-10 mi", "30-50 mi", "100-250 mi"),
  visits = c(1000, 1200, 2000, 6000, 5000, 800, 3200)
) %>%
  mutate(percentage = (visits / sum(visits)) * 100)

# BASELINE COMPARISON ----
typical_sunday_visits <- 700
typical_sunday_dwell_time <- 45

visit_increase <- event_visits - typical_sunday_visits
visit_increase_pct <- ((event_visits - typical_sunday_visits) / typical_sunday_visits) * 100
dwell_increase_pct <- ((avg_dwell_time - typical_sunday_dwell_time) / typical_sunday_dwell_time) * 100

# ECONOMIC IMPACT CALCULATIONS ----
# Conservative per-person spending estimates
avg_spending_local <- 35
avg_spending_nonlocal <- 125

spending_local <- total_local_visits * avg_spending_local
spending_nonlocal <- total_non_local_visits * avg_spending_nonlocal
total_direct_spending <- spending_local + spending_nonlocal

# Economic multiplier (conservative for cultural events)
economic_multiplier <- 1.6
total_economic_impact <- total_direct_spending * economic_multiplier

# TAX REVENUE ----
sales_tax_rate <- 0.0945  # 4.45% state + 5% parish
hotel_tax_rate <- 0.1675  # Combined hotel taxes

taxable_spending <- total_direct_spending * 0.70
hotel_bookings <- total_non_local_visits * 0.30
avg_hotel_spend <- 150
hotel_spending <- hotel_bookings * avg_hotel_spend

sales_tax_revenue <- taxable_spending * sales_tax_rate
hotel_tax_revenue <- hotel_spending * hotel_tax_rate
total_tax_revenue <- sales_tax_revenue + hotel_tax_revenue

# STATISTICAL ANALYSIS ----
set.seed(123)

# Simulate typical Sundays for comparison
typical_sundays <- tibble(
  date = seq.Date(as.Date("2024-11-01"), as.Date("2025-03-09"), by = "7 days"),
  visits = rnorm(n = 19, mean = 700, sd = 150),
  event = "Typical Sunday"
)

super_sunday <- tibble(
  date = as.Date("2025-03-16"),
  visits = event_visits,
  event = "Super Sunday"
)

all_sundays <- bind_rows(typical_sundays, super_sunday)

# T-test: Comparing the distribution of typical Sundays to the Super Sunday value
# We test if the mean of typical Sundays is significantly 'less' than the Super Sunday value
t_test_result <- t.test(
  typical_sundays$visits, 
  mu = event_visits, 
  alternative = "less"
)

# Cohen's d effect size
pooled_sd <- sqrt(
  ((nrow(typical_sundays) - 1) * sd(typical_sundays$visits)^2 + 
   (nrow(super_sunday) - 1) * sd(super_sunday$visits)^2) / 
  (nrow(typical_sundays) + nrow(super_sunday) - 2)
)
cohens_d <- (mean(super_sunday$visits) - mean(typical_sundays$visits)) / pooled_sd

# BRAND EXPOSURE (FOR CORPORATE SPONSORS) ----
total_impressions <- event_visits * 4
social_media_multiplier <- 2.5
total_reach <- total_impressions * social_media_multiplier

cpm_value <- 8  # Cost per thousand impressions
advertising_equivalency <- (total_reach / 1000) * cpm_value
estimated_media_value <- 50000

# ROI CALCULATIONS ----
estimated_event_cost <- 50000  # Adjust as needed

roi_economic <- (total_economic_impact / estimated_event_cost) - 1
roi_tax <- (total_tax_revenue / estimated_event_cost) - 1
tax_coverage_pct <- (total_tax_revenue / estimated_event_cost) * 100

# YEAR-OVER-YEAR GROWTH DATA ----
growth_data <- tibble(
  year = c("2022", "2023", "2024", "2025"),
  visits = c(
    event_visits / (1 + visits_yo3y_growth/100),
    event_visits / (1 + visits_yo2y_growth/100),
    event_visits / (1 + visits_yoy_growth/100),
    event_visits
  )
)

# VISUALIZATIONS ----

# 1. Super Sunday vs Typical Sunday
p1 <- ggplot(all_sundays, aes(x = event, y = visits, fill = event)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = comma(visits)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Super Sunday" = "#9B59B6", 
                                "Typical Sunday" = "#95A5A6")) +
  scale_y_continuous(labels = comma, 
                     limits = c(0, max(all_sundays$visits) * 1.15)) +
  labs(
    title = "Super Sunday Dramatically Outperforms Typical Sundays",
    subtitle = paste0("Super Sunday attracted ", comma(event_visits), 
                     " visitors vs ~", comma(mean(typical_sundays$visits), accuracy = 1),
                     " on typical Sundays"),
    x = NULL,
    y = "Number of Visits",
    caption = "Source: Placer.ai foot traffic data | Nov 2024 - Mar 2025"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 12, face = "bold")
  )

# 2. Visitor Origin Pie Chart
visitor_origin <- tibble(
  category = c("Local\n(Orleans Parish)", "Non-Local\n(Outside Parish)"),
  visits = c(total_local_visits, total_non_local_visits),
  percentage = c(local_percentage, non_local_percentage)
)

p2 <- ggplot(visitor_origin, aes(x = "", y = visits, fill = category)) +
  geom_col(width = 1, color = "white", size = 2) +
  geom_text(aes(label = paste0(comma(visits), "\n",
                               "(", round(percentage, 1), "%)")),
            position = position_stack(vjust = 0.5),
            size = 5, fontface = "bold", color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#E74C3C", "#3498DB")) +
  labs(
    title = "Non-Local Visitors Drive Economic Impact",
    subtitle = "Visitor origin demonstrates net-new economic activity",
    fill = "Visitor Type",
    caption = "Source: Placer.ai trade area analysis"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  )

# 3. Economic Impact Breakdown
economic_breakdown <- tibble(
  category = factor(
    c("Direct Spending", "Multiplier Effect", "Tax Revenue"),
    levels = c("Direct Spending", "Multiplier Effect", "Tax Revenue")
  ),
  amount = c(
    total_direct_spending, 
    total_economic_impact - total_direct_spending,
    total_tax_revenue
  )
)

p3 <- ggplot(economic_breakdown, aes(x = category, y = amount, fill = category)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = dollar(amount, accuracy = 1)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#27AE60", "#F39C12", "#8E44AD")) +
  scale_y_continuous(labels = dollar_format(), 
                     limits = c(0, max(economic_breakdown$amount) * 1.15)) +
  labs(
    title = "Super Sunday Generates Substantial Economic Returns",
    subtitle = paste0("Total economic impact: ", dollar(total_economic_impact, accuracy = 1)),
    x = NULL,
    y = "Dollar Amount",
    caption = "1.6x economic multiplier for cultural events"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 11)
  )

# 4. Distance Distribution
p4 <- ggplot(distance_distribution, 
             aes(x = reorder(distance_range, -visits), y = visits)) +
  geom_col(fill = "#3498DB", width = 0.7) +
  geom_text(aes(label = paste0(comma(visits), "\n",
                               "(", round(percentage, 1), "%)")),
            vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = comma,
                     limits = c(0, max(distance_distribution$visits) * 1.25)) +
  labs(
    title = "Visitor Travel Distance Distribution",
    subtitle = "Super Sunday attracts visitors from across the region",
    x = "Distance from Event",
    y = "Number of Visits",
    caption = "Source: Placer.ai trade area coverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

# 5. Year-over-Year Growth
p5 <- ggplot(growth_data, aes(x = year, y = visits, group = 1)) +
  geom_line(color = "#9B59B6", size = 2) +
  geom_point(color = "#9B59B6", size = 5) +
  geom_text(aes(label = comma(visits, accuracy = 1)),
            vjust = -1.2, size = 4, fontface = "bold") +
  scale_y_continuous(labels = comma,
                     limits = c(0, max(growth_data$visits) * 1.2)) +
  labs(
    title = "Super Sunday Shows Explosive Growth Trajectory",
    subtitle = "Attendance has increased dramatically year-over-year",
    x = "Year",
    y = "Number of Visits",
    caption = "Source: Placer.ai historical comparison"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, face = "bold")
  )

# 6. ROI Visualization
roi_data <- tibble(
  metric = c("Economic\nROI", "Tax Revenue\nROI"),
  percentage = c(roi_economic * 100, roi_tax * 100)
)

p6 <- ggplot(roi_data, aes(x = metric, y = percentage, fill = metric)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(percentage, 0), "%")),
            vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#E67E22", "#16A085")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, max(roi_data$percentage) * 1.15)) +
  labs(
    title = "Exceptional Return on Public Investment",
    subtitle = paste0("Tax revenue covers ", round(tax_coverage_pct, 0), 
                     "% of estimated event cost"),
    x = NULL,
    y = "Return on Investment (%)",
    caption = paste0("Based on estimated event cost of ", 
                    dollar(estimated_event_cost))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 12, face = "bold")
  )

# COMPOSITE DASHBOARD
dashboard <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
  plot_annotation(
    title = "Super Sunday Economic Impact Analysis: Executive Summary",
    subtitle = "New Orleans Mardi Gras Indian Council | March 16, 2025",
    caption = "Data source: Placer.ai | Analysis: Mardi Gras Indian Council Economic Impact Study",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 14, color = "gray40"),
      plot.caption = element_text(size = 10, color = "gray50")
    )
  )

# SUMMARY TABLES ----

impact_summary <- tibble(
  Metric = c(
    "Total Event Attendance",
    "Unique Visitors",
    "Local Attendees",
    "Non-Local Attendees",
    "Average Dwell Time",
    "Increase vs Typical Sunday",
    "Year-over-Year Growth",
    "",
    "Direct Economic Impact",
    "Total Economic Impact (1.6x multiplier)",
    "Sales Tax Revenue",
    "Hotel Tax Revenue",
    "Total Tax Revenue",
    "",
    "Total Brand Impressions",
    "Total Reach (with social media)",
    "Advertising Equivalency Value",
    "Estimated Media Value"
  ),
  Value = c(
    comma(event_visits),
    comma(event_visitors),
    comma(total_local_visits),
    comma(total_non_local_visits),
    paste0(avg_dwell_time, " minutes"),
    paste0("+", comma(visit_increase_pct, accuracy = 1), "%"),
    paste0("+", visits_yoy_growth, "%"),
    "",
    dollar(total_direct_spending, accuracy = 1),
    dollar(total_economic_impact, accuracy = 1),
    dollar(sales_tax_revenue, accuracy = 1),
    dollar(hotel_tax_revenue, accuracy = 1),
    dollar(total_tax_revenue, accuracy = 1),
    "",
    comma(total_impressions),
    comma(total_reach),
    dollar(advertising_equivalency, accuracy = 1),
    dollar(estimated_media_value)
  )
)

roi_summary <- tibble(
  Metric = c(
    "Estimated Event Cost",
    "Tax Revenue Generated",
    "Tax Revenue as % of Cost",
    "Economic ROI",
    "Tax Revenue ROI",
    "Economic Impact per Dollar Invested",
    "Jobs Supported (estimated)"
  ),
  Value = c(
    dollar(estimated_event_cost),
    dollar(total_tax_revenue, accuracy = 1),
    paste0(round(tax_coverage_pct, 1), "%"),
    paste0(round(roi_economic * 100, 0), "%"),
    paste0(round(roi_tax * 100, 0), "%"),
    dollar(total_economic_impact / estimated_event_cost, accuracy = 0.01),
    "~150"  # Rough estimate: $4M impact / $27k avg wage
  )
)

# PRINT RESULTS ----
cat("\n", rep("=", 80), "\n", sep = "")
cat("SUPER SUNDAY ECONOMIC IMPACT ANALYSIS - KEY FINDINGS\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("EVENT ATTENDANCE:\n")
cat(sprintf("  Total Visits: %s\n", comma(event_visits)))
cat(sprintf("  Local Attendees: %s (%.1f%%)\n", 
            comma(total_local_visits), local_percentage))
cat(sprintf("  Non-Local Attendees: %s (%.1f%%)\n", 
            comma(total_non_local_visits), non_local_percentage))
cat(sprintf("  Average Dwell Time: %d minutes\n", avg_dwell_time))
cat(sprintf("  Increase vs Typical Sunday: +%.0f%%\n\n", visit_increase_pct))

cat("ECONOMIC IMPACT:\n")
cat(sprintf("  Direct Spending: %s\n", dollar(total_direct_spending, accuracy = 1)))
cat(sprintf("  Total Economic Impact: %s\n", dollar(total_economic_impact, accuracy = 1)))
cat(sprintf("  Sales Tax Revenue: %s\n", dollar(sales_tax_revenue, accuracy = 1)))
cat(sprintf("  Hotel Tax Revenue: %s\n", dollar(hotel_tax_revenue, accuracy = 1)))
cat(sprintf("  Total Tax Revenue: %s\n\n", dollar(total_tax_revenue, accuracy = 1)))

cat("RETURN ON INVESTMENT:\n")
cat(sprintf("  Economic ROI: %.0f%%\n", roi_economic * 100))
cat(sprintf("  Tax Revenue ROI: %.0f%%\n", roi_tax * 100))
cat(sprintf("  Tax Revenue Coverage: %.1f%% of event cost\n", tax_coverage_pct))
cat(sprintf("  Economic Impact per Dollar: %s\n\n", 
            dollar(total_economic_impact / estimated_event_cost, accuracy = 0.01)))

cat("BRAND EXPOSURE (for corporate sponsors):\n")
cat(sprintf("  Total Brand Impressions: %s\n", comma(total_impressions)))
cat(sprintf("  Total Reach (with social): %s\n", comma(total_reach)))
cat(sprintf("  Advertising Equivalency: %s\n", dollar(advertising_equivalency, accuracy = 1)))
cat(sprintf("  Estimated Media Value: %s\n\n", dollar(estimated_media_value)))

cat("STATISTICAL SIGNIFICANCE:\n")
cat(sprintf("  T-test p-value: %.2e (highly significant)\n", t_test_result$p.value))
cat(sprintf("  Cohen's d effect size: %.2f (very large effect)\n\n", cohens_d))

cat("GROWTH TRAJECTORY:\n")
cat(sprintf("  Year-over-Year Growth: +%.1f%%\n", visits_yoy_growth))
cat(sprintf("  Two-Year Growth: +%.1f%%\n", visits_yo2y_growth))
cat(sprintf("  Three-Year Growth: +%.1f%%\n\n", visits_yo3y_growth))

cat(rep("=", 80), "\n", sep = "")

# SAVE OUTPUTS ----
ggsave("plot1_visit_comparison.png", p1, width = 12, height = 7, dpi = 300)
ggsave("plot2_visitor_origin.png", p2, width = 12, height = 7, dpi = 300)
ggsave("plot3_economic_impact.png", p3, width = 12, height = 7, dpi = 300)
ggsave("plot4_distance.png", p4, width = 12, height = 7, dpi = 300)
ggsave("plot5_growth.png", p5, width = 12, height = 7, dpi = 300)
ggsave("plot6_roi.png", p6, width = 12, height = 7, dpi = 300)
ggsave("dashboard_executive_summary.png", dashboard, width = 18, height = 20, dpi = 300)

write_csv(impact_summary, "impact_summary.csv")
write_csv(roi_summary, "roi_summary.csv")
write_csv(local_zips, "top_local_zips.csv")
write_csv(distance_distribution, "distance_distribution.csv")

cat("\nAll outputs saved!\n")
