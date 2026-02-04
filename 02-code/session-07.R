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
  mutate(
    ptr = if_else(income > 0, (net_tax_liability_y - zeroinc_taxliability_nochild) / income, 0),
    atr = if_else(income > 0, net_tax_liability_y / income, 0)
  )

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

# Estimate Revenue Functions -------------------------------------------------------------------------

## Some corrections  -------------------------------------------------------------------

# We also set all EMTR values above 1 to an arbitrary value close to but below one. Reason is that
# an EMTR of >=100 percent is not consistent with anyone chosing this income level.

data_combined <- data_combined %>%
  mutate(emtr = if_else(emtr >= 0.98, 0.98, emtr),
         ptr = if_else(ptr >= 0.98, 0.98, ptr))

## Assumptions about behavioral responses -------------------------------------------------------------------------

## With intensive margin ---------------------------------------------------

epsilon_50 = 0.5
epsilon_10 = 0.1

# Compute two different revenue functions with different elasticities 
data_combined <- data_combined %>%
  mutate(R_int_50 = (1-cdf_pareto) - epsilon_50 * income * pdf_pareto * emtr/(1-emtr),
         R_int_10 = (1-cdf_pareto) - epsilon_10 * income * pdf_pareto * emtr/(1-emtr)
  )

### Plot Revenue Function with e=0.5 -------------------------------------------------------------------------

ggplot(data_combined %>% filter(type=="Single without Children"), aes(x = income, y = R_int_50, color = type)) +
  geom_hline(yintercept = 0 ) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(-8,2,1), limits = c(-8, 2)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 50000)) +
  scale_color_manual(values = c("#1f77b4"), name = NULL) +
  labs(x = "Annual Gross Earnings (EUR)", y = "Revenue Function (e = 0.5)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session07_03_revenue_int_nochild_epsilon_50.pdf", width = 5, height = 4, dpi = 300)

### Plot Revenue Function with e=0.10 -------------------------------------------------------------------------

ggplot(data_combined %>% filter(type=="Single without Children"), aes(x = income, y = R_int_10, color = type)) +
  geom_hline(yintercept = 0 ) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(-1,1.5,0.5), limits = c(-1, 1.5)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 50000)) +
  scale_color_manual(values = c("#1f77b4"), name = NULL) +
  labs(x = "Annual Gross Earnings (EUR)", y = "Revenue Function (e = 0.1)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session07_04a_revenue_int_nochild_epsilon_10.pdf", width = 5, height = 4, dpi = 300)

# Plot Revenue Function (e=10) with two lines 

ggplot(data_combined %>% filter(type=="Single without Children"), aes(x = income, y = R_int_10, color = type)) +
  geom_hline(yintercept = 0 ) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(-1,1.5,0.5), limits = c(-1, 1.5)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 50000)) +
  scale_color_manual(values = c("#1f77b4"), name = NULL) +
  labs(x = "Annual Gross Earnings (EUR)", y = "Revenue Function (e = 0.1)") +
  theme_minimal() +
  theme(legend.position = "bottom")+ 
  geom_vline(xintercept = 28000, linetype = "dashed") +
  geom_vline(xintercept = 29000, linetype = "dashed") +
  annotate("text", x = 26500, y = 1.45, label = "y[1]", parse = TRUE, vjust = -0.2) +
  annotate("text", x = 30500, y = 1.45, label = "y[2]", parse = TRUE, vjust = -0.2)

ggsave("03-output/gr_session07_04b_revenue_int_nochild_epsilon_10.pdf", width = 5, height = 4, dpi = 300)

# Combined revenue functions for singles with and without children (e=10) for comparison.

# Note: There are some numerical issues in the calculation of EMTR by gettsim for singles with children leading to small bumps
# where MTR should increase continuously. 

ggplot(data_combined, aes(x = income, y = R_int_10, color = type, linetype = type)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(-3,2,1), limits = c(-3, 2)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 50000)) +
  scale_color_manual(values = c("Single without Children" = "#1f77b4", "Single with two Children" = "#ff7f0e"), name = NULL) +
  scale_linetype_manual(values = c("Single without Children" = "dashed", "Single with two Children" = "solid"), name = NULL) +
  labs(x = "Annual Gross Earnings (EUR)", y = "Revenue Function (e = 0.1)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session07_05_revenue_int_both_epsilon_10.pdf", width = 5, height = 4, dpi = 300)

# EMTR for singles without children

mtrplot <- ggplot(data_combined %>% filter(type == "Single without Children"), 
       aes(x = income, y = emtr * 100)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1, color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 50000)) +
  labs(x = "Annual Gross Earnings (EUR)", y = "EMTR (%)") +
  theme_minimal()

ggsave("03-output/gr_session07_06_emtr_single_nochild_2025.pdf", mtrplot, width = 5, height = 4, dpi = 300)

## With intensive and extensive margin ---------------------------------------------------

pi = 0.15

# First compute inner part 

data_combined <- data_combined %>%
  mutate(ext_inner = pdf_pareto * pi * (ptr/(1-ptr)))

# Compute integral 

data_combined <- data_combined %>%
  arrange(type, income) %>%
  group_by(type) %>%
  mutate(rev_ext_inner = rev(ext_inner),
         rev_ext_integral = cumtrapz(income, rev_ext_inner),
         ext_integral = -rev(rev_ext_integral)) %>%
  ungroup()

# Plot extensive margin part ext_int

ggplot(data_combined, aes(x = income, y = ext_integral, color = type, linetype = type)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 50000)) +
  scale_color_manual(values = c("Single without Children" = "#1f77b4", "Single with two Children" = "#ff7f0e"), name = NULL) +
  scale_linetype_manual(values = c("Single without Children" = "dashed", "Single with two Children" = "solid"), name = NULL) +
  labs(x = "Annual Gross Earnings (EUR)", y = "Extensive Margin Part") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session07_07_extmargin_integral_both.pdf", width = 5, height = 4, dpi = 300)

# Estimate revenue function including extensive margin part 
# Simply needs to be added to intensive margin revenue function

data_combined <- data_combined %>% 
  mutate(R_extint_10 = R_int_10 + ext_integral,
         R_extint_50 = R_int_50 + ext_integral)

# Plot revenue function with and without extensive margin responses

ggplot(data_combined %>% filter(type == "Single without Children")) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = income, y = R_int_10, color = "Intensive Only"), linewidth = 1) +
  geom_line(aes(x = income, y = R_extint_10, color = "Intensive + Extensive"), linewidth = 1, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-2, 2, 1), limits = c(-2, 2)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 50000)) +
  scale_color_manual(values = c("Intensive Only" = "#1f77b4", "Intensive + Extensive" = "#1f77b4"), name = NULL) +
  labs(x = "Annual Gross Earnings (EUR)", y = "Revenue Function (e = 0.1)", title="Single without Children") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session07_08_revenue_int_vs_extint_nochild_10.pdf", width = 5, height = 4, dpi = 300)

# Welfare Effects -------------------------------------------------------------------------

## Welfare Weights -------------------------------------------------------------------------

# Generate non-normalized welfare weigts 
# Working poor: only for incomes 0<y<=30000
# Inequality averse: income^-0.25. Need to take care of zero income (assign same value as
# smallest income value on grid)
# Rich loving: weight increases linearly with income

data_combined <- data_combined %>%
  mutate(g_workingpoor=if_else(income<=30000,1,0),
         g_inequalityaverse=ifelse(income>0,income^(-0.25),500^(-0.25)),
         g_richloving=income)

# Normalize all weights 

data_combined <- data_combined %>%
  arrange(type, income) %>%
  group_by(type) %>%
  mutate(
    g_workingpoor_norm = g_workingpoor / trapz(income, g_workingpoor * pdf_pareto),
    g_inequalityaverse_norm = g_inequalityaverse / trapz(income, g_inequalityaverse * pdf_pareto),
    g_richloving_norm = g_richloving / trapz(income, g_richloving * pdf_pareto)
  )



# Compute average welfare weight above income level 
# First reverse the g*f(y) term to be able to compute integral from respective starting value
# Then reverse cumulative integral and divide by (1-cdf)

data_combined <- data_combined %>%
  arrange(type, income) %>%
  group_by(type) %>%
  mutate(rev_g_workingpoor_norm = rev(g_workingpoor_norm*pdf_pareto),
         rev_g_inequalityaverse_norm = rev(g_inequalityaverse_norm*pdf_pareto),
         rev_g_richloving_norm = rev(g_richloving_norm*pdf_pareto),
         rev_g_integral_workingpoor_norm = cumtrapz(income, rev_g_workingpoor_norm),
         rev_g_integral_inequalityaverse_norm = cumtrapz(income, rev_g_inequalityaverse_norm),
         rev_g_integral_richloving_norm = cumtrapz(income, rev_g_richloving_norm),
         G_workingpoor= rev(rev_g_integral_workingpoor_norm)/(1-cdf_pareto),
         G_inequalityaverse = rev(rev_g_integral_inequalityaverse_norm)/(1-cdf_pareto),
         G_richloving = rev(rev_g_integral_richloving_norm)/(1-cdf_pareto)) %>%
  ungroup()

# Plot evolution of average welfare weights

ggplot(data_combined, aes(x = income)) +
  geom_line(aes(y = G_workingpoor, color = "Working Poor")) +
  geom_line(aes(y = G_inequalityaverse, color = "Inequality Averse")) +
  geom_line(aes(y = G_richloving, color = "Rich Loving")) +
  scale_color_manual(values = c("Working Poor" = "#1f77b4", "Inequality Averse" = "#ff7f0e", "Rich Loving" = "#2ca02c"), name = "Welfare Weight") +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 50000)) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 5)) +
  labs(x = "Annual Gross Earnings (EUR)", y = "G(y)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session07_09_welfare_weights.pdf", width = 5, height = 4, dpi = 300)

## Welfare Function -------------------------------------------------------------------------

# Add welfare function by modifying revenue functions (here only for e=0.1)

data_combined <- data_combined %>% 
  mutate(W_int_10_workingpoor = R_int_10 - (1-cdf_pareto)*G_workingpoor,
         W_int_10_inequalityaverse = R_int_10 - (1-cdf_pareto)*G_inequalityaverse,
         W_int_10_richloving = R_int_10 - (1-cdf_pareto)*G_richloving)

# Plot welfare functions with legend 

ggplot(data_combined %>% filter(type == "Single without Children")) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = income, y = W_int_10_workingpoor, color = "Working Poor")) +
  geom_line(aes(x = income, y = W_int_10_inequalityaverse, color = "Inequality Averse")) +
  geom_line(aes(x = income, y = W_int_10_richloving, color = "Rich Loving")) +
  geom_line(aes(x = income, y = R_int_10, color = "Revenue Function (Rawlsian)")) +
  scale_color_manual(values = c("Working Poor" = "#1f77b4", "Inequality Averse" = "#ff7f0e", "Rich Loving" = "#2ca02c", "Revenue Function (Rawlsian)" = "black"), name = "Welfare Function") +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 50000)) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) +
  labs(x = "Annual Gross Earnings (EUR)", y = "Welfare Function", title = "Single without Children") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "vertical", legend.box.just = "left", legend.direction = "horizontal", legend.text = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 2))

ggsave("03-output/gr_session07_10_welfare_functions.pdf", width = 5, height = 4, dpi = 300)

# Small Reform, Start and End Income Level -------------------------------------------------------------------------

# Let us now focus on the inefficiencies indicated by increasing revenue functions. 
# We now want to know based on the revenue function, at which points we need to increase / decrease MTR

# Plot again revenue function 

rplot <- ggplot(data_combined %>% filter(type == "Single without Children"), aes(x = income, y = R_int_10)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1, color = "black") +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(), limits = c(0, 50000)) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks()) +
  labs(x = "Annual Gross Earnings (EUR)", y = "Revenue Function (e = 0.1)", title = "R-Function, Single without Children (e = 0.1)") +
  theme_minimal()
rplot

# Check the points where revenue function is increasing substantially 

data_combined %>%
  filter(type == "Single without Children") %>%
  mutate(diff = R_int_10 - lag(R_int_10, n = 1)) %>%
  filter(diff > 0.1) %>%
  pull(income)

# Let's take a look at big jump

incomes_jump <- c(19000)

# Show the income levels where revenue function jumps 

rplot2 <- rplot +
  geom_vline(xintercept = incomes_jump,
             linetype = "dashed", color = "red") 
rplot2

# Now we are interested in a symmetric search around the income levels 
# where revenue function jumps. 

find_symmetric_deviation <- function(income0, data, offset_max = 5000, step = 500) {
  offsets <- seq(step, offset_max, step)
  diffs <- map_dbl(offsets, ~{
    y_left <- income0 - .x
    y_right <- income0 + .x
    r_left <- data %>% filter(income == y_left) %>% pull(R_int_10)
    r_right <- data %>% filter(income == y_right) %>% pull(R_int_10)
    if(length(r_left) == 0 | length(r_right) == 0) return(NA_real_)
    abs(r_left - r_right)
  })
  idx <- which.min(diffs)
  tibble(
    income0 = income0,
    offset = offsets[idx],
    income_left = income0 - offsets[idx],
    income_right = income0 + offsets[idx],
    R_diff = diffs[idx]
  )
}

data_reduced <- data_combined %>% filter(type=="Single without Children")

symmetric_deviations <- map_dfr(incomes_jump, ~find_symmetric_deviation(.x, data_reduced))
print(symmetric_deviations)

rplot3 <- rplot2 + 
  geom_vline(data = symmetric_deviations, aes(xintercept = income_left), linetype = "dashed", color = "darkorange") +
  geom_vline(data = symmetric_deviations, aes(xintercept = income_right), linetype = "dashed", color = "darkorange") 
rplot3

rplot4 <- rplot3 + 
  geom_segment(
    data = symmetric_deviations %>% 
      rowwise() %>% 
      mutate(
        R_left = data_reduced %>% filter(income == income_right) %>% pull(R_int_10),
        R_right = data_reduced %>% filter(income == income_right) %>% pull(R_int_10)
      ) %>% 
      ungroup(),
    aes(x = income_left, xend = income_right, y = R_left, yend = R_right),
    color = "grey", linewidth = 1, linetype = "solid"
  )+
  geom_text(
    aes(x = symmetric_deviations$income_left, y = -0.25, label = paste0(symmetric_deviations$income_left)),
    vjust = -1, hjust = 1.25, color = "darkorange", fontface = "bold", size = 3.5
  ) +
  geom_text(
    aes(x = symmetric_deviations$income_right, y = -0.25, label = paste0(symmetric_deviations$income_right)),
    vjust = -1, hjust = -0.25, color = "darkorange", fontface = "bold", size = 3.5
  )
rplot4

ggsave("03-output/gr_session07_11a_reform.pdf", rplot, width = 5, height = 4, dpi = 300)
ggsave("03-output/gr_session07_11b_reform.pdf", rplot2, width = 5, height = 4, dpi = 300)
ggsave("03-output/gr_session07_11c_reform.pdf", rplot3, width = 5, height = 4, dpi = 300)
ggsave("03-output/gr_session07_11d_reform.pdf", rplot4, width = 5, height = 4, dpi = 300)

# Plot MTR with income levels 

mtrplot2 <- mtrplot+
  geom_vline(xintercept = symmetric_deviations$income_left, linetype = "dashed", color = "darkorange") +
  geom_vline(xintercept = symmetric_deviations$income_right, linetype = "dashed", color = "darkorange") +
  geom_text(aes(x = symmetric_deviations$income_left, y = 0, label = symmetric_deviations$income_left),
            vjust = -1, hjust = 1.25, color = "darkorange", fontface = "bold", size = 3.5) +
  geom_text(aes(x = symmetric_deviations$income_right, y = 0, label = symmetric_deviations$income_right),
            vjust = -1, hjust = -0.25, color = "darkorange", fontface = "bold", size = 3.5) +
  geom_vline(xintercept = incomes_jump, linetype = "dashed", color = "red")

ggsave("03-output/gr_session07_12_mtr_reform.pdf", mtrplot2, width = 5, height = 4, dpi = 300)

# Can check this similarly for other MTR drop / Revenue Function Jump