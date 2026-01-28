# Session 07: Tax Reforms

# In session 07, we estimate revenue and welfare functions to compute the revenue 
# and welfare effects of small tax reforms in line with Bierbrauer et al. (2023).

# Preamble -------------------------------------------------------------------------

library(tidyverse)
library(pracma) # numerical math functions
library(readr) # read in csv files

## Read in MTR / tax liability data -------------------------------------------------------------------------

### A: Single without Children -------------------------------------------------------------------------

load("04-data/processed/data_gettsim_single_nochild_2025.RData")

# Limit data 

data_gettsim_nochild <- data_gettsim_nochild %>% 
  select(earnings_y, net_earnings_y, net_tax_liability_y, emtr) %>%
  rename(income=earnings_y)

### B: Single with two Children -------------------------------------------------------------------------

load("04-data/processed/data_gettsim_single_twochild_2025.RData")

# Limit data 

data_gettsim_twochild <- data_gettsim_twochild %>% 
  select(earnings_y, net_earnings_y, net_tax_liability_y, emtr) %>%
  rename(income=earnings_y)

## Participation tax rate -------------------------------------------------------------------------

### A: Single without Children -------------------------------------------------

zeroinc_taxliability_nochild <- data_gettsim_nochild %>%
  filter(income == 0) %>%
  pull(net_tax_liability_y)

# PTR shows how much taxes I pay in relation to my 
# income when I start working, consists of two parts:
# - When I start working at specific income level, I pay a net tax liability.
# - When I start working at specific income level, I lose my transfer.

# Net tax liability already includes lump-sum component, thus need to substract 
# for estimation of PTR.

data_gettsim_nochild <- data_gettsim_nochild %>%
  mutate(ptr=(net_tax_liability_y-zeroinc_taxliability_nochild)/income,
         atr=net_tax_liability_y/income)

ggplot(data_gettsim_nochild, aes(x = income)) +
  geom_hline(yintercept = 0)+
  geom_line(aes(y = ptr * 100, color = "PTR"), linewidth = 1) +
  geom_line(aes(y = atr * 100, color = "ATR"), linewidth = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(),limits = c(-80, 100)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(),limits = c(0,250000)) +
  scale_color_manual(values = c("PTR" = "#1f77b4", "ATR" = "#ff7f0e"), name = NULL) +
  labs(x = "Annual Gross Earnings (EUR)", y = "Tax Rate (% )") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session07_01_gettsim_single_nochild_ptr_2025.pdf", width = 5, height = 4, dpi = 300)

rm(zeroinc_taxliability_nochild)

### B: Single with two Children -------------------------------------------------

zeroinc_taxliability_twochild <- data_gettsim_twochild %>%
  filter(income == 0) %>%
  pull(net_tax_liability_y)

# PTR shows how much taxes I pay in relation to my 
# income when I start working, consists of two parts:
# - When I start working at specific income level, I pay a net tax liability.
# - When I start working at specific income level, I lose my transfer.

data_gettsim_twochild <- data_gettsim_twochild %>%
  mutate(ptr=(net_tax_liability_y-zeroinc_taxliability_twochild)/income,
         atr=net_tax_liability_y/income)

ggplot(data_gettsim_twochild, aes(x = income)) +
  geom_hline(yintercept = 0)+
  geom_line(aes(y = ptr * 100, color = "PTR"), linewidth = 1) +
  geom_line(aes(y = atr * 100, color = "ATR"), linewidth = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(),limits = c(-80, 100)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(),limits = c(0,250000)) +
  scale_color_manual(values = c("PTR" = "#1f77b4", "ATR" = "#ff7f0e"), name = NULL) +
  labs(x = "Annual Gross Earnings (EUR)", y = "Tax Rate (% )") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session07_02_gettsim_single_twochild_ptr_2025.pdf", width = 5, height = 4, dpi = 300)

rm(zeroinc_taxliability_twochild)

# Combine info about tax system for both types -------------------------------------------------------------------------

data_gettsim_nochild <- data_gettsim_nochild %>%
  mutate(type="Single without Children")

data_gettsim_twochild <- data_gettsim_twochild %>%
  mutate(type="Single with two Children")

data_gettsim <- rbind(data_gettsim_nochild,data_gettsim_twochild)

rm(data_gettsim_nochild,data_gettsim_twochild)

# Read in distribution data  -------------------------------------------------------------------------

# Read in distribution data from soep + pareto interpolation from session 03.

load("04-data/processed/data_opttax_long.RData")

# Limit data 

data_distr <- data_opttax_long %>% 
  select(income,pdf_pareto,cdf_pareto) %>%
  distinct()

rm(data_opttax_long)

# Combine distribution data with info about tax rates  -------------------------------------------------------------------------

data_gettsim <- data_gettsim %>%
  mutate(income = round(income / 500) * 500)

data_combined <- left_join(data_gettsim, data_distr, by = "income")

rm(data_gettsim,data_distr)

