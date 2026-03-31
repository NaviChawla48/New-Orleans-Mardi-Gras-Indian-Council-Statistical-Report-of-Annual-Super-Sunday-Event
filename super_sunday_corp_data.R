# ============================================================
# Super Sunday: Corporate Sponsors Report Data & Figures
# New Orleans Mardi Gras Indian Council
# Analyst: Navi Chawla | February 2026
# ============================================================

library(tidyverse)
library(scales)
library(knitr)
library(kableExtra)

# ============================================================
# SECTION 1: CORE DATA
# ============================================================

visits_2025    <- 34100
visits_2026    <- 31600
dwell_2025     <- 109
dwell_2026     <- 105
local_2025     <- 18000
nonlocal_2025  <- 16100
event_cost     <- 70000

yoy_2025  <- 7292.8
yo2y_2025 <- 10035.2
yo3y_2025 <- 12805.9
yoy_2026  <- 5173.3
yo2y_2026 <- 4079.7

spend_local    <- 35
spend_nonlocal <- 125
direct_spend   <- (local_2025 * spend_local) + (nonlocal_2025 * spend_nonlocal)
multiplier     <- 1.6
total_econ     <- direct_spend * multiplier
total_tax      <- 296155

# Brand exposure
exposures_per_person <- 4
direct_impressions   <- visits_2025 * exposures_per_person
social_multiplier    <- 2.5
total_reach          <- direct_impressions * social_multiplier
cpm_rate             <- 8
advertising_equiv    <- (total_reach / 1000) * cpm_rate
presenting_fee       <- 50000
cpp_presenting       <- (presenting_fee / total_reach) * 1000

# ============================================================
# SECTION 2: TABLES
# ============================================================

tbl_summary_corp <- tribble(
  ~Metric,                                ~Value,
  "Total Attendance (2025)",              "34,100",
  "Total Attendance (2026)",              "31,600",
  "Average Engagement Duration",          "108 minutes",
  "Non-Local Visitors",                   "16,100 (47%)",
  "Direct Brand Impressions",             "136,400",
  "Total Reach (with social media)",      "341,000",
  "CPM for Presenting Sponsor ($50K)",    "$146.60",
  "Advertising Equivalency Value",        "$2,728",
  "Conservative Earned Media Value",      "$50,000+",
  "True PR Value (professional estimate)","$200,000 -- $500,000",
  "National Media Outlets (2025)",        "NPR, CNN, National Geographic, NYT",
  "Estimated Combined Media Reach",       "23.5 million+"
)

tbl_twoyear <- tribble(
  ~Metric,             ~`2025`,    ~`2026`,
  "Total Visits",      "34,100",   "31,600",
  "Avg. Dwell Time",   "109 min",  "105 min",
  "YoY Growth",        "+7,293%",  "+5,173%",
  "2-Year Growth",     "+10,035%", "+4,080%"
)

tbl_demographics <- tribble(
  ~Characteristic,                        ~`Super Sunday`, ~`National Average`,
  "Identify as Black / African American",  "71%",           "13%",
  "College-educated",                      "59%",           "38%",
  "Employed",                              "90%",           "63%",
  "Household income $60K+",               "~46%",          "~38%",
  "Multi-generational attendance",         "Yes",           "N/A"
)

tbl_media <- tribble(
  ~Outlet,               ~`Est. Reach`, ~Type,
  "NPR Morning Edition",  "15M+",        "National Radio",
  "CNN",                  "5M+",         "National Television",
  "National Geographic",  "2M+",         "National Print/Digital",
  "The New York Times",   "1M+",         "National Print/Digital",
  "Local TV (all NOLA)",  "500K+",       "Local Television"
)

tbl_cpm <- tribble(
  ~Metric,                                  ~Value,
  "Direct Impressions",                      "136,400",
  "Total Reach (with social amplification)", "341,000",
  "Presenting Sponsor Investment",           "$50,000",
  "CPM (cost per 1,000 impressions)",        "$146.60",
  "Advertising Equivalency Value",           "$2,728",
  "Average Visitor Engagement Duration",     "108 minutes"
)

tbl_csr <- tribble(
  ~`Impact Category`,          ~Metric,
  "Total Economic Activity",    "$4.2 million",
  "Jobs Supported",             "~150 FTE (direct and indirect)",
  "Tax Revenue to City/State",  "$296,155",
  "Free Public Access",         "100% -- no tickets, no admission",
  "Neighborhood Served",        "Central City, New Orleans",
  "Tradition Preserved",        "200-year Mardi Gras Indian heritage",
  "Cultural Recognition",       "Smithsonian Institution, National Geographic"
)

tbl_benchmark <- tribble(
  ~Festival,                        ~`Cost/Attendee`, ~`Economic ROI`, ~`Tax ROI`,
  "Super Sunday",                    "$2.05",           "60.4x",         "4.23x",
  "French Quarter Fest (per day)",   "$10.67",          "62.5x",         "3.75x",
  "Jazz Fest (per day)",             "$30.77",          "150x",          "9x",
  "Essence Fest (per day)",          "$33.33",          "1.8x",          "---"
)

tbl_sensitivity <- tribble(
  ~Scenario,                ~`Economic Impact`, ~`Tax Revenue`, ~`Tax ROI`,
  "Conservative (-20%)",    "$3,382,400",        "$261,195",     "3.73x",
  "Base Case",              "$4,228,000",        "$296,155",     "4.23x",
  "Optimistic (+20%)",      "$5,073,600",        "$331,115",     "4.73x"
)

tbl_sources <- tribble(
  ~Source,                                    ~`What It Provides`,
  "Placer.ai Civic Dashboard (Mar 16 2025)",  "Attendance, dwell time, visitor origin, prior/post spillover",
  "Placer.ai Time Compare (2025 vs. 2026)",   "Year-over-year comparison, 2026 attendance, favorite places",
  "Louisiana Cultural Economy Study",         "Per-person spending estimates by visitor type",
  "Peer-reviewed festival literature",        "Multiplier range (1.5x to 1.9x), CPM benchmarks",
  "Industry social media research",           "Social amplification benchmarks (2x to 4x)",
  "PR measurement standards",                 "Earned media value methodology"
)

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

# ============================================================
# SECTION 3: FIGURES
# ============================================================

# Growth data - 2025 and 2026 only (verified)
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

# DMA origin chart
fig_dma <- {
  dma_data <- tibble(
    DMA    = factor(
      c("New Orleans Metro (86.4%)", "Baton Rouge (2.9%)", "Houston (1.8%)",
        "Atlanta (1.1%)", "Dallas (0.6%)", "DC/NoVA (0.5%)",
        "New York (0.4%)", "Other (6.3%)"),
      levels = rev(c("New Orleans Metro (86.4%)", "Baton Rouge (2.9%)", "Houston (1.8%)",
                     "Atlanta (1.1%)", "Dallas (0.6%)", "DC/NoVA (0.5%)",
                     "New York (0.4%)", "Other (6.3%)"))
    ),
    Visits = c(29300, 985, 626, 381, 206, 166, 142, 1294),
    Fill   = c("local", rep("nonlocal", 7))
  )
  ggplot(dma_data, aes(x = DMA, y = Visits, fill = Fill)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = comma(Visits)), hjust = -0.1,
              size = 2.8, fontface = "bold") +
    coord_flip() +
    scale_fill_manual(values = c("local"="#4A148C","nonlocal"="#AB47BC")) +
    scale_y_continuous(labels = comma, limits = c(0, 35000), expand = c(0,0)) +
    labs(x = NULL, y = "Visitors") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major.y = element_blank(),
          plot.background    = element_rect(fill = "white", color = NA))
}

# Impression funnel
fig_impressions <- {
  imp_df <- tibble(
    Stage = factor(
      c("Attendees (34,100)", "Direct Impressions\n(4x = 136,400)",
        "Total Reach\n(2.5x social = 341,000)"),
      levels = c("Attendees (34,100)", "Direct Impressions\n(4x = 136,400)",
                 "Total Reach\n(2.5x social = 341,000)")
    ),
    Count = c(34100, 136400, 341000)
  )
  ggplot(imp_df, aes(x = Stage, y = Count)) +
    geom_col(fill = "#6A1B9A", width = 0.55) +
    geom_text(aes(label = comma(Count)), vjust = -0.4,
              fontface = "bold", size = 4) +
    scale_y_continuous(labels = comma, limits = c(0, 420000), expand = c(0,0)) +
    labs(x = NULL, y = "Count") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.x = element_blank(),
          plot.background    = element_rect(fill = "white", color = NA))
}

# Favorite places both years
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
  ) %>% mutate(Business = str_wrap(Business, 18))

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

# Spillover
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
  labs(x = NULL, y = "Share of Event Visitors Who Also Visited", fill = "Timing") +
  theme_minimal(base_size = 11) +
  theme(legend.position    = "bottom",
        panel.grid.major.y = element_blank(),
        plot.background    = element_rect(fill = "white", color = NA))

# Benchmark
benchmark_plot_data <- tibble(
  Event = factor(
    c("Super Sunday","French Quarter\nFest (per day)","Jazz Fest\n(per day)","Essence Fest\n(per day)"),
    levels = c("Essence Fest\n(per day)","Jazz Fest\n(per day)","French Quarter\nFest (per day)","Super Sunday")
  ),
  Cost  = c(2.05, 10.67, 30.77, 33.33),
  Highlight = c(TRUE, FALSE, FALSE, FALSE)
)

fig_benchmark <- ggplot(benchmark_plot_data,
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

# Distance regression
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

cat("Corporate R file loaded successfully.\n")
cat(sprintf("Direct impressions: %s\n", comma(direct_impressions)))
cat(sprintf("Total reach: %s\n", comma(total_reach)))
cat(sprintf("CPM for presenting sponsor: $%.2f\n", cpp_presenting))
