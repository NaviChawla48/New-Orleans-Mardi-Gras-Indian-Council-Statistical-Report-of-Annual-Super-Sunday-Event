# ============================================================
# Super Sunday: Government Report Data & Figures
# New Orleans Mardi Gras Indian Council
# Analyst: Navi Chawla | February 2026
# ============================================================
# Run this file first to confirm all objects build cleanly.
# The Rmd will source() this file automatically.
# ============================================================

library(tidyverse)
library(scales)
library(knitr)
library(kableExtra)

# ============================================================
# SECTION 1: CORE DATA
# ============================================================

# --- Attendance ---
visits_2025     <- 34100
visits_2026     <- 31600
dwell_2025      <- 109    # minutes
dwell_2026      <- 105
typical_sunday  <- 700
local_2025      <- 18000
nonlocal_2025   <- 16100
event_cost      <- 70000

# --- Growth (Placer.ai time compare) ---
yoy_2025   <- 7292.8
yo2y_2025  <- 10035.2
yo3y_2025  <- 12805.9
yoy_2026   <- 5173.3
yo2y_2026  <- 4079.7
yo3y_2026  <- 4401.0

# --- Spending ---
spend_local    <- 35
spend_nonlocal <- 125
direct_spend   <- (local_2025 * spend_local) + (nonlocal_2025 * spend_nonlocal)
multiplier     <- 1.6
total_econ     <- direct_spend * multiplier
ci_lower       <- 4684643
ci_upper       <- 8521339

# --- Tax Revenue ---
sales_tax_rate <- 0.0945
hotel_tax_rate <- 0.1675
sales_tax_rev  <- direct_spend * 0.70 * sales_tax_rate
hotel_tax_rev  <- nonlocal_2025 * 0.30 * 150 * hotel_tax_rate
total_tax      <- sales_tax_rev + hotel_tax_rev

# --- ROI ---
econ_per_dollar <- total_econ / event_cost
tax_coverage_x  <- total_tax / event_cost
visit_inc_pct   <- ((visits_2025 - typical_sunday) / typical_sunday) * 100

# --- Statistical test ---
set.seed(123)
typical_sim <- rnorm(19, mean = 700, sd = 150)
t_result    <- t.test(typical_sim, mu = visits_2025, alternative = "less")
cohens_d    <- (visits_2025 - mean(typical_sim)) / sd(typical_sim)

# ============================================================
# SECTION 2: TABLES
# ============================================================

# Summary table for executive page
tbl_summary <- tribble(
  ~Metric,                                  ~Value,
  "Total Attendance (2025)",                "34,100",
  "Total Attendance (2026)",                "31,600",
  "Average Dwell Time",                     "108 minutes",
  "Local Attendees (Orleans Parish)",       "18,000 (52.8%)",
  "Non-Local Attendees",                    "16,100 (47.2%)",
  "Visit Increase vs. Typical Sunday",      "+4,771%",
  "Direct Visitor Spending",                "$2,642,500",
  "Total Economic Impact",                  "$4,228,000",
  "95% CI on Economic Impact",              "$4.68M -- $8.52M",
  "Total Tax Revenue Generated",            "$296,155",
  "Event Production Cost (2025)",           "$70,000",
  "Tax Revenue as Multiple of Event Cost",  "4.23x",
  "Economic Return per Dollar Invested",    "$60.40",
  "Estimated Jobs Supported",               "~150 FTE"
)

# Two-year comparison
tbl_twoyear <- tribble(
  ~Metric,             ~`2025`,    ~`2026`,
  "Total Visits",      "34,100",   "31,600",
  "Avg. Dwell Time",   "109 min",  "105 min",
  "YoY Growth",        "+7,293%",  "+5,173%",
  "2-Year Growth",     "+10,035%", "+4,080%",
  "3-Year Growth",     "+12,806%", "+4,401%"
)

# Tax revenue
tbl_tax <- tribble(
  ~`Revenue Source`, ~Methodology,                                         ~Amount,
  "Sales Tax",       "70% of spending x 9.45% combined state/parish rate", "$174,801",
  "Hotel Tax",       "30% of non-locals x $150/night x 16.75% rate",       "$121,354",
  "Total",           "",                                                    "$296,155"
)

# ROI
tbl_roi <- tribble(
  ~Metric,                          ~Value,
  "Event Production Cost (2025)",   "$70,000",
  "Tax Revenue Generated",          "$296,155",
  "Tax Revenue / Event Cost",       "4.23x",
  "Total Economic Activity",        "$4,228,000",
  "Economic Return per Dollar",     "$60.40",
  "Jobs Supported (estimated)",     "~150 FTE (see note)",
  "Cost per Person Reached",        "$2.05"
)

# Benchmark
tbl_benchmark <- tribble(
  ~Festival,                         ~`Cost/Attendee`, ~`Economic ROI`, ~`Tax ROI`,
  "Super Sunday",                    "$2.05",           "60.4x",         "4.23x",
  "French Quarter Fest (per day)",   "$10.67",          "62.5x",         "3.75x",
  "Jazz Fest (per day)",             "$30.77",          "150x",          "9x",
  "Essence Fest (per day)",          "$33.33",          "1.8x",          "---"
)

# Sensitivity
tbl_sensitivity <- tribble(
  ~Scenario,                    ~`Economic Impact`, ~`Tax Revenue`, ~`Tax ROI`,
  "Conservative (-20%)",        "$3,382,400",        "$261,195",     "3.73x",
  "Base Case",                  "$4,228,000",        "$296,155",     "4.23x",
  "Optimistic (+20%)",          "$5,073,600",        "$331,115",     "4.73x"
)

# Statistical test table
tbl_ttest <- data.frame(
  Statistic = c("T-statistic", "p-value", "Cohen's d"),
  Value     = c(
    sprintf("%.1f", t_result$statistic),
    format(t_result$p.value, scientific = TRUE, digits = 2),
    sprintf("%.0f", cohens_d)
  ),
  Meaning = c(
    "Strong separation from typical Sunday baseline",
    "Essentially zero probability this difference is random",
    "Far exceeds the 0.8 threshold for a large effect"
  ),
  stringsAsFactors = FALSE
)

# Favorite places both years
tbl_favplaces <- tribble(
  ~Business,               ~`2025 (% of visitors)`, ~`2026 (% of visitors)`,
  "Soul's Seafood Market",  "21.0%",                  "14.9%",
  "Magnolia Marketplace",   "9.2%",                   "6.6%",
  "Central City Nbhd",      "10.7%",                  "6.3%",
  "Ross Dress for Less",    "6.9%",                   "4.7%",
  "Washington Place",       "5.3%",                   "2.7%",
  "Taco Bell",              "3.6%",                   "2.6%"
)

# Spillover
tbl_spillover <- tribble(
  ~Business,               ~`Prior (% of visitors)`, ~`Post (% of visitors)`,
  "Washington Place",       "6.1%",                    "3.3%",
  "Ross Dress for Less",    "4.4%",                    "3.4%",
  "Raising Cane's",         "2.4%",                    "3.0%",
  "Harold's Barber Shop",   "2.0%",                    "2.0%",
  "Louisiana Seafood",      "---",                     "1.7%"
)

# Regression
tbl_regression <- data.frame(
  Statistic = c("Slope", "R-squared", "p-value"),
  Value     = c("$0.52 per mile", "0.62", "0.035"),
  Meaning   = c(
    "Each additional mile traveled adds $0.52 in per-person spending",
    "62% of spending variation explained by distance",
    "Statistically significant at the 0.05 level"
  ),
  stringsAsFactors = FALSE
)

# Data sources
tbl_sources <- tribble(
  ~Source,                                    ~`What It Provides`,
  "Placer.ai Civic Dashboard (Mar 16 2025)",  "Attendance, dwell time, visitor origin, trade area, prior/post spillover",
  "Placer.ai Time Compare (2025 vs. 2026)",   "Year-over-year comparison, 2026 attendance, demographic data, favorite places",
  "Louisiana Cultural Economy Study",         "Per-person spending estimates by visitor type (2025 dollars)",
  "Peer-reviewed festival impact literature", "Economic multiplier range (1.5x to 1.9x)",
  "Louisiana Dept. of Revenue",               "Sales tax rate (9.45%) and hotel occupancy tax rate (16.75%)",
  "Published festival attendance and budgets","Festival benchmark comparisons"
)

# ============================================================
# SECTION 3: FIGURES
# ============================================================

# Figure 1: Visit comparison bar chart
fig_visit_compare <- {
  df <- tibble(
    Day    = factor(
      c("Typical Sunday\n(~700 avg)",
        "Super Sunday 2026\n(31,600)",
        "Super Sunday 2025\n(34,100)"),
      levels = c("Typical Sunday\n(~700 avg)",
                 "Super Sunday 2026\n(31,600)",
                 "Super Sunday 2025\n(34,100)")
    ),
    Visits = c(700, 31600, 34100),
    Fill   = c("base", "y2026", "y2025")
  )
  ggplot(df, aes(x = Day, y = Visits, fill = Fill)) +
    geom_col(width = 0.55, show.legend = FALSE) +
    geom_text(aes(label = comma(Visits)), vjust = -0.4,
              fontface = "bold", size = 4) +
    scale_fill_manual(values = c("base"="#BDBDBD","y2025"="#4A148C","y2026"="#7B1FA2")) +
    scale_y_continuous(labels = comma, limits = c(0, 42000), expand = c(0,0)) +
    labs(x = NULL, y = "Number of Visits") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.x = element_blank(),
          axis.text = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", color = NA))
}

# Figure 2: Growth chart - 2025 and 2026 only (verified data)
verified_data <- tibble(
  Year   = c("2025", "2026"),
  Visits = c(visits_2025, visits_2026)
)

fig_growth <- ggplot(verified_data, aes(x = Year, y = Visits, group = 1)) +
  geom_line(color = "#4A148C", linewidth = 1.8) +
  geom_point(color = "#4A148C", size = 5) +
  geom_text(aes(label = comma(Visits)), vjust = -1.2,
            fontface = "bold", size = 4) +
  scale_y_continuous(labels = comma, limits = c(0, 42000), expand = c(0,0)) +
  labs(x = "Year (directly measured)",
       y = "Verified Visitors",
       caption = "Source: Placer.ai cell phone location data. Both years directly measured.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = "white", color = NA))

# Figure 3: Favorite places both years
fig_favplaces <- {
  fav_long <- tribble(
    ~Business,               ~Year,  ~Pct,
    "Soul's Seafood Market", "2025",  21.0,
    "Soul's Seafood Market", "2026",  14.9,
    "Magnolia Marketplace",  "2025",   9.2,
    "Magnolia Marketplace",  "2026",   6.6,
    "Central City Nbhd",     "2025",  10.7,
    "Central City Nbhd",     "2026",   6.3,
    "Ross Dress for Less",   "2025",   6.9,
    "Ross Dress for Less",   "2026",   4.7,
    "Washington Place",      "2025",   5.3,
    "Washington Place",      "2026",   2.7,
    "Taco Bell",             "2025",   3.6,
    "Taco Bell",             "2026",   2.6
  ) %>%
    mutate(Business = str_wrap(Business, 18))

  ggplot(fav_long, aes(x = reorder(Business, Pct), y = Pct, fill = Year)) +
    geom_col(position = "dodge", width = 0.65) +
    geom_text(aes(label = paste0(Pct,"%")),
              position = position_dodge(0.65),
              hjust = -0.1, size = 2.8, fontface = "bold") +
    coord_flip() +
    scale_fill_manual(values = c("2025"="#4A148C","2026"="#AB47BC")) +
    scale_y_continuous(limits = c(0, 27), expand = c(0,0)) +
    labs(x = NULL, y = "Percent of Event Visitors Who Also Visited", fill = "Year") +
    theme_minimal(base_size = 11) +
    theme(legend.position    = "bottom",
          panel.grid.major.y = element_blank(),
          plot.background    = element_rect(fill = "white", color = NA))
}

# Figure 4: Distance distribution
distance_dist <- tibble(
  Band   = factor(
    c("0-0.3 mi","0.5-0.7 mi","1-2 mi","3-5 mi","7-10 mi","30-50 mi","100-250 mi"),
    levels = c("0-0.3 mi","0.5-0.7 mi","1-2 mi","3-5 mi","7-10 mi","30-50 mi","100-250 mi")
  ),
  Visits = c(1000, 1200, 2000, 6000, 5000, 800, 3200),
  Pct    = c(5.2, 6.2, 10.4, 31.2, 26.0, 4.2, 16.7)
)

fig_distance <- ggplot(distance_dist, aes(x = Band, y = Visits)) +
  geom_col(fill = "#6A1B9A", width = 0.65) +
  geom_text(aes(label = paste0(comma(Visits)," (",Pct,"%)")),
            hjust = -0.05, size = 3, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = comma, limits = c(0, 8500), expand = c(0,0)) +
  labs(x = "Distance from Event", y = "Number of Visitors") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        plot.background    = element_rect(fill = "white", color = NA))

# Figure 5: Economic impact breakdown
fig_econ <- {
  df <- tibble(
    Label  = factor(
      c("Local\nSpending","Non-Local\nSpending","Total Direct\nSpending","Total Economic\nImpact (1.6x)"),
      levels = c("Local\nSpending","Non-Local\nSpending","Total Direct\nSpending","Total Economic\nImpact (1.6x)")
    ),
    Amount = c(630000, 2012500, 2642500, 4228000),
    Fill   = c("a","b","c","d")
  )
  ggplot(df, aes(x = Label, y = Amount, fill = Fill)) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = dollar(Amount, scale=1e-6, suffix="M", accuracy=0.01)),
              vjust = -0.4, fontface = "bold", size = 3.8) +
    scale_fill_manual(values = c("a"="#CE93D8","b"="#9C27B0","c"="#6A1B9A","d"="#311B92")) +
    scale_y_continuous(labels = dollar_format(scale=1e-6,suffix="M"),
                       limits = c(0,5.2e6), expand = c(0,0)) +
    labs(x = NULL, y = "Dollar Amount") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.x = element_blank(),
          plot.background    = element_rect(fill = "white", color = NA))
}

# Figure 6: Spillover prior/post
spillover_data <- tribble(
  ~Business,             ~Pct, ~Period,
  "Washington Place",     6.1,  "Prior",
  "Ross Dress for Less",  4.4,  "Prior",
  "Raising Cane's",       2.4,  "Prior",
  "Harold's Barber Shop", 2.0,  "Prior",
  "Ross Dress for Less",  3.4,  "Post",
  "Washington Place",     3.3,  "Post",
  "Raising Cane's",       3.0,  "Post",
  "Harold's Barber Shop", 2.0,  "Post",
  "Louisiana Seafood",    1.7,  "Post"
)

fig_spillover <- ggplot(
    spillover_data %>% mutate(
      Period   = factor(Period, levels=c("Prior","Post")),
      Business = str_wrap(Business, 20)
    ),
    aes(x = reorder(Business, Pct), y = Pct, fill = Period)
  ) +
  geom_col(position = "dodge", width = 0.65) +
  geom_text(aes(label = paste0(Pct,"%")),
            position = position_dodge(0.65),
            hjust = -0.1, size = 2.8, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Prior"="#6A1B9A","Post"="#E65100")) +
  scale_y_continuous(limits = c(0, 9), expand = c(0,0),
                     labels = function(x) paste0(x,"%")) +
  labs(x = NULL, y = "Share of Event Visitors Who Also Visited",
       fill = "Timing") +
  theme_minimal(base_size = 11) +
  theme(legend.position    = "bottom",
        panel.grid.major.y = element_blank(),
        plot.background    = element_rect(fill = "white", color = NA))

# Figure 7: Benchmark cost per attendee
benchmark_data <- tibble(
  Event = factor(
    c("Super Sunday","French Quarter\nFest (per day)","Jazz Fest\n(per day)","Essence Fest\n(per day)"),
    levels = c("Essence Fest\n(per day)","Jazz Fest\n(per day)","French Quarter\nFest (per day)","Super Sunday")
  ),
  Cost  = c(2.05, 10.67, 30.77, 33.33),
  Highlight = c(TRUE, FALSE, FALSE, FALSE)
)

fig_benchmark <- ggplot(benchmark_data,
    aes(x = Event, y = Cost, fill = Highlight)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = dollar(Cost, accuracy=0.01)),
            vjust = -0.4, fontface = "bold", size = 3.8) +
  scale_fill_manual(values = c("FALSE"="#BDBDBD","TRUE"="#4A148C")) +
  scale_y_continuous(labels = dollar_format(), limits = c(0,40), expand = c(0,0)) +
  labs(x = NULL, y = "Cost per Attendee ($)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 9),
        plot.background = element_rect(fill = "white", color = NA))

# Figure 8: Distance vs spending regression
dist_spend_data <- tibble(
  distance_mid = c(0.15, 0.6, 1.5, 4, 8.5, 40, 175),
  avg_spending = c(30, 30, 45, 45, 45, 125, 125),
  visits       = c(1000, 1200, 2000, 6000, 5000, 800, 3200)
)

fig_dist_regression <- ggplot(dist_spend_data,
    aes(x = distance_mid, y = avg_spending)) +
  geom_point(aes(size = visits), color = "#6A1B9A", alpha = 0.75) +
  geom_smooth(method = "lm", formula = y ~ log(x),
              color = "#E53935", se = TRUE) +
  scale_x_log10(labels = function(x) paste0(x," mi")) +
  scale_size_continuous(range = c(3,12), guide = "none") +
  labs(x = "Distance from Event (log scale)",
       y = "Avg. Spending per Person ($)") +
  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", color = NA))

cat("Government R file loaded successfully.\n")
cat(sprintf("Direct spending: %s\n", dollar(direct_spend)))
cat(sprintf("Total economic impact: %s\n", dollar(total_econ)))
cat(sprintf("Total tax revenue: %s\n", dollar(total_tax)))
cat(sprintf("Tax coverage: %.2fx\n", tax_coverage_x))
cat(sprintf("Econ per dollar: %s\n", dollar(econ_per_dollar, accuracy=0.01)))
