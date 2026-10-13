#===============================================================================
# PSEO Earnings Comparison: UL Lafayette Economics vs. Business & All Majors
# Medians and 75th percentiles at 1/5/10 years since graduation.
# Re-downloads the current PSEO release on every run.
#===============================================================================

rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(readxl)
library(janitor)
library(writexl)
#===============================================================================



write_output <- T
wd <- "c:/work/dropbox/Apps/Overleaf/ul-econ-major/"
mdir <- "./data/Census/pseo/meta/"
odir_fig <- str_c(wd, "figures/")
odir_tab <- str_c(wd, "tables/")





# meta data ====================================================================
INSTITUTION_ID  <- "00203100"   # ULL, IPEDS UnitID
DEGREE_LEVEL    <- "05"         # Bachelor's
CIP_LEVEL_MAJOR <- "4"          # 4-digit CIP
CIP_LEVEL_ALL   <- "A"          # All Degree Fields (pairs with cipcode "00")
CIP_CODE_ALL    <- "00"
GRAD_COHORT     <- "All Cohorts"

PERCENTILES      <- c(50, 75)
YEARS_SINCE_GRAD <- c(1, 5, 10)

BASE_URL <- "https://lehd.ces.census.gov/data/pseo/latest_release/la/"
RAW_FILE <- "pseo_la.xlsx"

# PSEO publishes in constant dollars; base year is in the release docs.
# CPI-U, U.S. city average, all items, NSA (BLS CUUR0000SA0 / FRED CPIAUCNS).
PSEO_BASE_YEAR <- 2023
CPI_BASE       <- 304.702       # 2023 annual average
CPI_TARGET     <- 325.252       # January 2026
DOLLAR_LABEL   <- "January 2026 dollars"

cpi_factor <- CPI_TARGET / CPI_BASE

# 4-digit CIP -> ULL major. Period stripped: "52.02" -> "5202".
# Unlisted codes are excluded.
MAJOR_MAP <- tribble(
  ~cip4,   ~major,
  "5202",  "Management",        # Business Administration & Operations
  "5207",  "Management",        # Entrepreneurial & Small Business
  "5210",  "Management",        # Human Resources
  "5211",  "Management",        # International Business
  "5212",  "Management",        # Management Information Systems
  "5213",  "Management",        # Management Sciences & Quantitative Methods
  "5203",  "Accounting",        # Accounting & Related Services
  "5216",  "Accounting",        # Taxation
  "5208",  "Finance",           # Finance & Financial Management
  "5215",  "Finance",           # Real Estate
  "5217",  "Insurance",         # Insurance
  "5206",  "Economics",         # Business/Managerial Economics
  "5209",  "Hospitality",       # Hospitality Administration
  "5214",  "Marketing",         # Marketing
  "5218",  "Marketing",         # General Sales & Merchandising
  "5219",  "Marketing"          # Specialized Sales & Merchandising
)

ECON_MAJOR  <- "Economics"
ECON_COLOR  <- "#CE0E2D"   # UL vermilion
OTHER_COLOR <- "#7C8894"   # neutral gray, >= 3:1 on white

# figure size, matched to the landscape text block (11 x 8.5 page, 0.7in margins)
FIG_W <- 9.6

# axis labels: $50K, $100K ... but plain "$0" at the origin
dollar_axis <- function(x) {
  ifelse(x == 0, "$0", scales::dollar(x, scale = 1e-3, suffix = "K", accuracy = 1))
}
#===============================================================================



theme_pseo <- theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 15, color = ECON_COLOR),
    plot.subtitle = element_text(color = "grey35", size = 11),
    plot.caption = element_text(color = "grey45", size = 8, hjust = 0),
    strip.background = element_rect(fill = "grey92", color = NA),
    strip.text = element_text(face = "bold", size = 11),
    strip.text.y = element_text(angle = 0),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 11, color = "grey35"),
    legend.position = "top",
    legend.title = element_blank()
  )



# download =====================================================================
pull_date <- Sys.Date()
raw_path  <- str_c(mdir, RAW_FILE)

download.file(str_c(BASE_URL, RAW_FILE), destfile = raw_path, quiet = TRUE, mode = "wb")

pseo_citation <- str_c(
  "U.S. Census Bureau, Longitudinal Employer-Household Dynamics Program, ",
  "Post-Secondary Employment Outcomes (PSEO), Louisiana file, ",
  "https://lehd.ces.census.gov/data/pseo_experimental.html, generated using ",
  "PSEO Beta Release Data; accessed ", format(pull_date, "%B %d, %Y"), "."
)

# technical notes live in the LaTeX handout, not on the figures.
# printed at the end so the dated citation can be pasted into the .tex.



# load and reshape =============================================================
pseo_raw <- read_excel(raw_path, skip = 4) %>%
  clean_names()

colnames(pseo_raw)

pseo_raw %>%
  filter(institution == INSTITUTION_ID, deglevl_code == DEGREE_LEVEL) %>%
  distinct(label_grad_cohort, label_grad_cohort_years)


ull <- pseo_raw %>%
  filter(
    institution == INSTITUTION_ID,
    deglevl_code == DEGREE_LEVEL,
    label_grad_cohort == GRAD_COHORT
  ) %>%
  mutate(cip4 = str_sub(str_remove_all(as.character(cipcode), "\\D"), 1, 4))

# earnings: y<year>_p<percentile>_earnings
earn <- ull %>%
  select(cip_level, cipcode, cip4, label_cipcode, matches("^y\\d+_p\\d+_earnings$")) %>%
  mutate(across(matches("^y\\d+_p\\d+_earnings$"), as.numeric)) %>%
  pivot_longer(matches("^y\\d+_p\\d+_earnings$"), names_to = "var", values_to = "earnings") %>%
  mutate(
    year = as.numeric(str_extract(var, "\\d+")),
    percentile = as.numeric(str_extract(var, "(?<=_p)\\d+"))
  ) %>%
  select(-var) %>%
  drop_na(earnings) %>%
  filter(year %in% YEARS_SINCE_GRAD, percentile %in% PERCENTILES)

# weights: graduates with observed earnings, one count per year
grads <- ull %>%
  select(cip_level, cipcode, cip4, label_cipcode, matches("^y\\d+_grads_earn$")) %>%
  mutate(across(matches("^y\\d+_grads_earn$"), as.numeric)) %>%
  pivot_longer(matches("^y\\d+_grads_earn$"), names_to = "var", values_to = "n_grads") %>%
  mutate(year = as.numeric(str_extract(var, "\\d+"))) %>%
  select(-var)

pseo_ull <- earn %>%
  left_join(grads, by = c("cip_level", "cipcode", "cip4", "label_cipcode", "year")) %>%
  mutate(
    earnings = earnings * cpi_factor,
    yrs = factor(
      str_c(year, ifelse(year == 1, " yr", " yrs")),
      levels = str_c(YEARS_SINCE_GRAD, ifelse(YEARS_SINCE_GRAD == 1, " yr", " yrs"))
    )
  )



# comparison groups ============================================================
mapped <- pseo_ull %>%
  filter(cip_level == CIP_LEVEL_MAJOR) %>%
  inner_join(MAJOR_MAP, by = "cip4") %>%
  filter(!is.na(n_grads), n_grads > 0)

bus <- mapped %>%
  group_by(major, yrs, percentile) %>%
  summarise(
    earnings = weighted.mean(earnings, n_grads),
    n_grads = sum(n_grads),
    n_cip = n(),
    .groups = "drop"
  ) %>%
  mutate(is_econ = major == ECON_MAJOR)

all_maj <- pseo_ull %>%
  filter(cip_level == CIP_LEVEL_ALL, as.character(cipcode) == CIP_CODE_ALL)

cmp <- bind_rows(
  bus %>% filter(is_econ) %>% transmute(yrs, percentile, group = "Economics", earnings),
  all_maj %>% transmute(yrs, percentile, group = "All majors", earnings)
)



# --- Economics vs. other business majors ------------------------------------
YEARS_SHOW <- c("5 yrs", "10 yrs")

d   <- bus %>% filter(yrs %in% YEARS_SHOW)
ord <- d %>% filter(percentile == 50, yrs == last(YEARS_SHOW)) %>% arrange(earnings) %>% pull(major)

d <- d %>%
  mutate(
    major = factor(major, levels = ord),
    panel = factor(ifelse(percentile == 50, "Median earnings", "75th percentile earnings"),
                   levels = c("Median earnings", "75th percentile earnings"))
  )

p_bus <- ggplot(d, aes(x = earnings, y = major, fill = is_econ)) +
  geom_col() +
  geom_text(aes(label = scales::dollar(earnings, accuracy = 1)),
            hjust = -0.15, size = 3, color = "grey25") +
  facet_grid(yrs ~ panel,
             labeller = labeller(yrs = function(x) str_c(x, "\npost-graduation"))) +
  scale_fill_manual(values = c(`TRUE` = ECON_COLOR, `FALSE` = OTHER_COLOR),
                    labels = c(`TRUE` = "Economics", `FALSE` = "Other business majors"),
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels = dollar_axis,
                     expand = expansion(mult = c(0, 0.28))) +
  labs(x = str_c("Earnings (", DOLLAR_LABEL, ")")) +
  theme_pseo

p_bus

ggsave(str_c(odir_fig, "econ_vs_business_majors_",
             str_c(str_remove_all(YEARS_SHOW, " "), collapse = "_"), "_", pull_date, ".pdf"),
       p_bus, width = FIG_W, height = 5.3, bg = "white", device = cairo_pdf)



# --- Economics vs. all UL majors --------------------------------------------
d_all <- cmp %>%
  filter(yrs %in% YEARS_SHOW) %>%
  mutate(
    group = factor(group, levels = c("All majors", "Economics")),
    panel = factor(ifelse(percentile == 50, "Median earnings", "75th percentile earnings"),
                   levels = c("Median earnings", "75th percentile earnings"))
  )

p_all <- ggplot(d_all, aes(x = earnings, y = group, fill = group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::dollar(earnings, accuracy = 1)),
            hjust = -0.15, size = 3.4, color = "grey25") +
  facet_grid(yrs ~ panel,
             labeller = labeller(yrs = function(x) str_c(x, "\npost-graduation"))) +
  scale_fill_manual(values = c("Economics" = ECON_COLOR, "All majors" = OTHER_COLOR)) +
  scale_x_continuous(labels = dollar_axis,
                     expand = expansion(mult = c(0, 0.28))) +
  labs(x = str_c("Earnings (", DOLLAR_LABEL, ")")) +
  theme_pseo +
  theme(legend.position = "none")

p_all

ggsave(str_c(odir_fig, "econ_vs_all_majors_",
             str_c(str_remove_all(YEARS_SHOW, " "), collapse = "_"), "_", pull_date, ".pdf"),
       p_all, width = FIG_W, height = 5.0, bg = "white", device = cairo_pdf)


message(pseo_citation)

