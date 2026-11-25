library(readxl)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(countrycode)
library(wbstats)
library(dplyr)
library(fixest)
library(plm)
library(lmtest)
library(broom)
library(clubSandwich)

# load data
setwd("C:/Users/lukas/OneDrive/Desktop/0_Alle_Ordner/9_NOVA/01_S1/Econometrics/Report/Data")
set.seed(79173)
data_zhang <- read_xlsx("Inflation targeting dataset_Zhang 2025 online version.xlsx", sheet = "Data")
data_romelli <- read_xlsx("CBIData_Romelli_2025.xlsx", sheet = "CBI data")

#renaming variables as required, and removing unnecessary columns
data_zhang$ISO[data_zhang$ISO == "ROM"] <- "ROU"

data_zhang_b <- data_zhang %>%
  rename(iso3 = ISO, year = year, pi = CPI_yoychange) %>%
  mutate(quarter = (quarterid - 1) %% 4 + 1,
         year    = 1980 + (quarterid - 1) %/% 4) %>%
  filter(quarter == 4) %>%
  mutate(band_width = inflation_target_upper - inflation_target_lower) %>% 
  select(iso3, pi, year, ITTR_A_1yr, ITTR_R_1yr, ITTR_M_1yr, band_width)

data_romelli_b <- data_romelli %>%
  rename(iso3 = iso_a3, year = year) %>% 
  select(iso3, year, cbie_index, cbie_board, 
         cbie_policy, cbie_obj, cbie_lending, 
         cbie_finindep, cbie_report)

# view gaps in Zhang's track records
## assign quarter's

data_zhang$quarter_no <- (data_zhang$quarterid - 1) %% 4 + 1

## filter only fourth quarters
data_avail <- data_zhang %>% filter(quarter_no == 4)                     

## visualise when data is available for each country
### create helper table
avail <- data_avail %>%
  mutate(observed = !is.na(ITTR_A_1yr))

first_obs <- avail %>%
  group_by(country) %>%
  summarise(first_year = suppressWarnings(min(year[observed])),  
            .groups = "drop")

avail <- avail %>%
  left_join(first_obs, by = "country") %>%
  mutate(country = reorder(country, -first_year))

n_countries <- n_distinct(avail$country)

### Cut-off lines for 20, 15 and 10 years of data
cutoffs <- tibble(year  = c(2004, 2009, 2014),
                  label = c("20 yrs", "15 yrs", "10 yrs"))

### Plot data_zhangvailability_zhang
ggplot(avail, aes(year, country, fill = observed)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  geom_vline(data = cutoffs, aes(xintercept = year - 0.5),
             linetype = "dashed", linewidth = 0.4) +
  geom_text(data = cutoffs,
            aes(x = year - 0.5, y = n_countries + 1.2, label = label),
            inherit.aes = FALSE, size = 3) +
  scale_fill_manual(values = c(`FALSE` = "#F8766D", `TRUE` = "#00BFC4"),
                    labels = c(`FALSE` = "Missing", `TRUE` = "Observed"),
                    name = NULL) +
  scale_x_continuous(breaks = seq(1990, 2020, 5),
                     expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.8))) +
  labs(x = NULL, y = NULL,
       title = "Fig. 1 - Inflation Targeting CBs by Country and Duration") +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

ggsave("data_zhangvailability_zhang.svg", width = 7.2, height = 6.1)

# build data set only including the measurements required for the Panel OLS
## building the base data frame
v_countries <- c("Australia", "Brazil", "Canada", "Chile",
                 "Colombia", "Czech Republic", "Ghana", "Iceland",
                 "Indonesia", "Mexico", "Moldova","New Zealand",
                 "Paraguay", "Peru", "Philippines", "Poland", "Romania",
                 "South Africa", "Thailand", "Türkiye", "Uruguay")
v_iso3 <- c("AUS", "BRA", "CAN", "CHL", "COL", "CZE", "GHA",
            "ISL", "IDN", "MEX", "MDA", "NZL", "PRY", "PER",
            "PHL", "POL", "ROU", "ZAF", "THA", "TUR", "URY")
v_years <- 1993:2023

wb_countries <- wb_countries()

v_inc <- wb_countries[,c("iso3c", "income_level")] %>%
  mutate(h_inc_dummy = wb_countries$income_level == "High income") %>%
  rename(iso3 = iso3c)
v_inc$h_inc_dummy[v_inc$h_inc_dummy == TRUE] <- "High income"
v_inc$h_inc_dummy[v_inc$h_inc_dummy == FALSE] <- "Not high income"

data_study <- data.frame(country = rep(v_countries, each = length(v_years)),
                         iso3 = rep(v_iso3, each = length(v_years)),
                         year = rep(v_years, times = length(v_iso3)))

## left join both data_romelli_b and data_zhang_b to data_study
data_study <- data_study %>%
  left_join(data_romelli_b, by = c("iso3", "year")) %>%
  left_join(data_zhang_b,   by = c("iso3", "year")) %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(across(starts_with("cbie"), \(x) lag(x, order_by = year),
                .names = "{.col}_l1")) %>%
  ungroup() %>%
  mutate(region = countrycode(iso3, "iso3c", "region")) %>%
  left_join(v_inc, by = "iso3", relationship = "many-to-one")

### check within country variance
var_check <- data_study %>%
  group_by(iso3) %>%
  summarise(n_changes = sum(cbie_index != cbie_index_l1, na.rm = TRUE),
            sd_within = sd(cbie_index, na.rm = TRUE)) %>%
  select(iso3, n_changes, sd_within)

### share of total variance that is within-country
with(data_study, var(cbie_index - ave(cbie_index, iso3, FUN = \(x) mean(x, na.rm = TRUE)),
                     na.rm = TRUE) / var(cbie_index, na.rm = TRUE))

### check share of variance that is left over after both time and country fixed effects
r <- resid(feols(cbie_index ~ 1 | iso3 + year, data = data_study))
var(r) / var(data_study$cbie_index[!is.na(data_study$cbie_index)])

# run model
## baseline models
### panel ols with random effects
m_re <- plm(ITTR_A_1yr ~ cbie_index_l1 + band_width,
         data = data_study, 
         index = c("iso3","year"), 
         model = "random")
summary(m_re)
### panel ols with fixed effects
m_fe <- plm(ITTR_A_1yr ~ cbie_index_l1 + band_width,
          data = data_study, 
          index = c("iso3","year"), 
          model = "within",
          effect = "twoways")
summary(m_fe)

## Hausman Test
phtest(m_fe,m_re)

## robust Hausman
phtest(ITTR_A_1yr ~ cbie_index_l1 + band_width, data = data_study, method = "aux",
       vcov = function(x) vcovHC(x, method = "arellano", type = "sss", cluster = "group"))

## robust SE's
G <- length(unique(index(m_fe, "iso3"))) 

m_fe_robust <- coeftest(m_fe,
                        vcov = vcovHC(m_fe, method = "arellano", type = "sss", cluster = "group"),
                        df   = G - 1)
print(m_fe_robust)

m_fe_robust_tidy <- tidy(coeftest(m_fe,
                                  vcov = vcovHC(m_fe, method = "arellano", type = "sss", cluster = "group"),
                                  df   = G - 1))

## Cluster by Region with more than 3 countries
m_fe_regions <- lapply(split(data_study, data_study$region), function(d) {
  m <- plm(ITTR_A_1yr ~ cbie_index_l1 + band_width, data = d,
           index = c("iso3", "year"), model = "within")
  coeftest(m,
           vcov = vcovHC(m, method = "arellano", type = "sss", cluster = "group"),
           df   = G - 1)
})

print(m_fe_regions)

## Cluster by income (high vs not-high)
m_fe_inc <- lapply(split(data_study, data_study$h_inc_dummy), function(d) {
  m <- plm(ITTR_A_1yr ~ cbie_index_l1 + band_width, data = d,
           index = c("iso3", "year"), model = "within")
  coeftest(m,
           vcov = vcovHC(m, method = "arellano", type = "sss", cluster = "group"),
           df   = G - 1)
})

print(m_fe_inc)

### leave one out check to identify outliers that mess with the results
idx <- c("iso3", "year")
m_loo <- bind_rows(lapply(unique(data_study$iso3), function(cc) {
  m   <- plm(ITTR_A_1yr ~ cbie_index_l1 + band_width, data = data_study[data_study$iso3 != cc, ], index = idx, model = "within")
  G_m <- pdim(m)$nT$n                       
  ct  <- coeftest(m,
                  vcov = vcovHC(m, method = "arellano", type = "sss", cluster = "group"),
                  df   = G_m - 1)
  tidy(ct) %>% mutate(dropped = cc, G = G_m)
}))

m_loo <- m_loo %>%
  left_join(m_fe_robust_tidy %>% select(term, est_full = estimate, se_full = std.error, p_full = p.value),
            by = "term") %>%
  mutate(shift = (estimate - est_full) / se_full)

ggplot(m_loo, aes(x = estimate, y = reorder(dropped, estimate),
                xmin = estimate - 1.96 * std.error,
                xmax = estimate + 1.96 * std.error)) +
  geom_pointrange(size = 0.2) +
  geom_vline(aes(xintercept = est_full), colour = "red") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ term, scales = "free_x") +
  labs(title = "Fig. 3 - Leave-one-out check visualised", x = "Estimate when country is dropped (95% CI, cluster-robust)", y = "Country dropped") + 
  theme_minimal()

ggsave("leave_one_out_check.svg", width = 5.86, height = 3.7)

### visualise thailand CBIE by itself 

### Plot CBIE of all studied Countries
ggplot(data_study) +
  geom_line(aes(x = year, y = cbie_index, group = iso3, color = iso3)) +
  labs(x = NULL, y = NULL,
       title = "Fig. 2 - CBI over the studied period") +
  facet_wrap(region ~ .) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "none")

ggsave("cbie_by_region.svg", width = 5.86, height = 3.7)

### Plot CBIE of Thailand
ggplot(subset(data_study, iso3 == "THA")) +
  geom_line(aes(x = year, y = cbie_index)) +
  labs(x = NULL, y = NULL,
       title = "Fig. 4 - CBIE in Thailand over the studied period") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "none")

ggsave("cbie_THA.svg", width = 5.86, height = 3.7)

### plot CBP for Thailand

ggplot(subset(data_study, iso3 == "THA"), aes(x = year, y = ITTR_A_1yr)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = NULL, y = NULL,
       title = "Fig. 5 - CBP over the studied period in Thailand") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "none")

ggsave("CBP_thailand.svg", width = 5.86, height = 3.7)

### Plot CBP of all studied Countries
ggplot(data_study) +
  geom_line(aes(x = year, y = ITTR_A_1yr, group = iso3, color = iso3)) +
  labs(x = NULL, y = NULL,
       title = "Fig. 3 - CBP over the studied period") +
  facet_wrap(region ~ .) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "none")

ggsave("CBP_by_region.svg", width = 5.86, height = 3.7)

### Plot countries on world map for Cover Page
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- subset(world, continent != "Antarctica")   # optional

world$highlight <- world$iso_a3_eh %in% v_iso3

ggplot(world) +
  geom_sf(aes(fill = highlight), colour = "white", linewidth = 0.1) +
  scale_fill_manual(values = c(`TRUE` = "#d7301f", `FALSE` = "grey85"),
                    guide = "none") +
  coord_sf(crs = "+proj=robin") +   # Robinson projection; remove for plain lat/long
  theme_void()

ggsave("cover_page_plot.svg", width = 5.86, height = 3.7)