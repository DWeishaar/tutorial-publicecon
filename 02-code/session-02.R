# Session 02: Estimate CDF and Pareto Parameters

# Preamble -------------------------------------------------------------------------

library(tidyverse) # collection of packages for data wrangling, plotting, etc.
library(haven) # import SAS/Stata files
library(labelled) # handle labeled data 
library(readxl) # read excel files
library(scales) # for graph aesthetics

# Load and Filter Data ---------------------------------------------------------------

# Load data produced in session 01. 

load(file="04-data/processed/soep.RData")

# Focus on individuals with earnings larger than 0

soep <- soep %>% filter(income_tot_y>0 & syear==2019)

# Empirical CDF  ---------------------------------------------------

# Generate empirical CDF of the total income variable

soep <- soep %>%
  arrange(income_tot_y) %>% # sort data according to income 
  mutate(
    cdf_income = cume_dist(income_tot_y), # Get the empirical CDF 
  )

# Plot 1: Empirical CDF 

gr_session02_01_cdf <- ggplot(soep, aes(x = income_tot_y)) +
  geom_line(aes(y = cdf_income), linewidth = 1) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1),
    limits = c(0,1)
  ) +
  scale_x_continuous(
    breaks = seq(0, max(soep$income_tot_y, na.rm = TRUE), by = 50000),
    labels = label_number(big.mark=",", accuracy = 1)
  ) +
  labs(x = "Total Income", y = "Empirical CDF", title = "Empirical CDF of Total Income") +
  theme_minimal()

gr_session02_01_cdf
ggsave(gr_session02_01_cdf,file="03-output/gr_session02_01_cdf.pdf")

# Estimate Pareto Parameter -------------------------------------------------------------------------

## 01 Direct Estimation of a from CDF -------------------------------------------------------------------------

# Generate log(1-cdf) and plot against income 

soep <- soep %>%
  mutate(log1mcdf = log(1 - cdf_income),
         log_income_tot_y = log(income_tot_y))

# Plot log(1-cdf) against log(income)

gr_session02_02_log1mcdf <- ggplot(data=soep, aes(x = log_income_tot_y, y = log1mcdf)) +
  geom_point(alpha = 0.3) +
  labs(x = "log(Total Income)", y = "log(1 - CDF)", title = "log(1-CDF) vs. log(Total Income)") +
  theme_minimal()

gr_session02_02_log1mcdf
ggsave(gr_session02_02_log1mcdf, file = "03-output/gr_session02_02_log1mcdf.pdf")

# Generate information on top 5 percent  

soep <- soep %>%
  mutate(
    group = case_when(
      cdf_income > 0.95  ~ "Top 5%",
      TRUE ~ "Other"
    )
  )

# Estimate linear model for top 5% 

fit_top5  <- lm(log1mcdf ~ log_income_tot_y, data = soep %>% filter(group=="Top 5%" & is.finite(log1mcdf)))
summary(fit_top5)

# Plot both (1-cdf) against income for top 10 percent visualize top 5 percent 

gr_session02_03_toptails <- ggplot(soep %>% filter(cdf_income>0.9), 
                                   aes(x = log_income_tot_y, y = log1mcdf, color = group)) +
  geom_point(alpha = 0.4) +
  labs(x = "log(Total Income)", y = "log(1 - CDF)", color = "Group",
       title = "log(1-CDF) vs log(Total Income): Top 10 Percent") +
  theme_minimal()

gr_session02_03_toptails
ggsave(gr_session02_03_toptails, file = "03-output/gr_session02_03_toptails.pdf")

# Add linear model estimated fit to the graph 

coef_top5 <- coef(fit_top5)[["log_income_tot_y"]]  # Extract coefficient for log_income 

gr_session02_04_toptails_fitted_top5 <- gr_session02_03_toptails +
  geom_abline(
    intercept = coef(fit_top5)[["(Intercept)"]],
    slope = coef(fit_top5)[["log_income_tot_y"]],
    color = "black",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = 12.25, y = -7,
    label = paste0(round(coef_top5, 3)),
    size = 4, color = "black"
  )

gr_session02_04_toptails_fitted_top5
ggsave(gr_session02_04_toptails_fitted_top5, file = "03-output/gr_session02_04_toptails_fitted_top5.pdf")

# Store Pareto coefficient 

pareto_a_direct = -coef_top5

## 02  Function to Estimate Top Tax Rate  -----------------------------------------------

# Define function that takes marginal social welfare weight g at the top,
# elasticity, and Pareto parameter as inputs, and returns the optimal top tax rate 

estimate_T_top <- function(g, elasticity, pareto_a) {
  TprimeRatio = (1-g)*(1/elasticity)*(1/pareto_a) # Estimate T'/(1-T')
  Tprime = TprimeRatio/(1+TprimeRatio) # From T'/(1-T') to T'
  return(Tprime) # What the function returns 
}

# Example of how to call the function and get the top tax rate for g=0, e=0.25 and a=pareto_a_direct
estimate_T_top(0,0.25,pareto_a_direct)

# Now we want to make use of function and estimate optimal T' for different g and e values. 

toptaxes <- expand_grid( # generate table 
  g = c(0, 0.25, 0.5), # three different g values 
  elasticity = seq(0.1,1.5, by=0.1) # multiple elasticity values ranging from 0.1 to 1.5 in steps of 0.1
) 

# Add optimal top tax rates to the table using function 

toptaxes <-  toptaxes %>%
  mutate(top_tax_rate_direct = estimate_T_top(g, elasticity, pareto_a_direct))

# Plot how top tax rates vary over elasticity values, different groups for different g values 

gr_session02_05_toptaxes <- ggplot(toptaxes, aes(x = elasticity, y = top_tax_rate_direct, color = factor(g))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(0,1,0.2),limits = c(0,1))+
  labs(x = "Elasticity", y = "Optimal Top Tax Rate", color = "g",
       title = paste0("Optimal Top Tax Rate, a=", round(pareto_a_direct,digits = 2))) +
  theme_minimal()

gr_session02_05_toptaxes
ggsave(gr_session02_05_toptaxes, file = "03-output/gr_session02_05_toptaxes.pdf")

## 03 Indirect Estimation from Tabulated Data -------------------------------------------------------------------------

# Statistischer Bericht - Lohn- und Einkommensteuer 2021 (statistical report on payroll and income tax 2021)
# Download from https://www.destatis.de/DE/Themen/Staat/Steuern/Lohnsteuer-Einkommensteuer/Publikationen/_publikationen-innen-lohn-einkommen.html
# Store in raw data folder 

# Read in only those with incomes above zero, ignore total amount 
# Check which sheet and cells to read in 

incometaxstats_class <- read_excel("04-data/raw/statistischer-bericht-lohn-einkommensteuer-2140710217005.xlsx", sheet = "73111-03", range="A7:C26",col_names = F)

# Rename variables 
incometaxstats_class <- incometaxstats_class %>%
  rename(
    class = ...1,
    N = ...2,
    income = ...3
  )

# Measured in 1000 EUR 

incometaxstats_class <- incometaxstats_class %>% 
  mutate(
    income = income*1000
  )

# Eliminate all spaces from class and extract 
# lower and upper bound numeric values 

incometaxstats_class <- incometaxstats_class %>%
  mutate(class = str_replace_all(class, " ", ""))

# Extract lower bound and upper bound of class 
# (uses regular expressions - detect specific patterns in string)

incometaxstats_class <- incometaxstats_class %>%
  mutate(
    lower_bound = as.numeric(str_extract(class, "^[0-9]+")), # extract something at the start of a string (^) that is of numeric ([0-9]) and can be more than one occurence (+)
    upper_bound = as.numeric(str_extract(class, "(?<=-)[0-9]+")) # extract characters where you have "-" directly before (?<=-)
  )

# Total income and total number of tax payers 
# (uses regular expressions)

incometaxstats_class <- incometaxstats_class %>%
  mutate(
    totalN = sum(N, na.rm = TRUE),
    totalincome = sum(income, na.rm = TRUE)
  )

# Get share of taxpayers in class and mean income in class, and 

incometaxstats_class <- incometaxstats_class %>%
  mutate(
    shareN = N / totalN, # share 
    bracketavg = income / N, # mean income in class
    percentile_lb = lag(cumsum(shareN), default = 0) # cumsum gives cumulative sum, we lag the value to obtain values for lower bound
  )

# Get average income for values >= lower bound 

incometaxstats_class <- incometaxstats_class %>%
  arrange(lower_bound) %>% # sort according to lower bound
  mutate(
    avg_income_above = map_dbl(row_number(), 
                               ~sum(income[.x:n()]) / sum(N[.x:n()])) # go through all rows with lower bound, for each row calculate the sum of incomes >= lower bound and divide by the sum of individuals with incomes >= lower bound 
  )

# Get b (ratio of average income >= bracket start value / bracket start value)

incometaxstats_class <- incometaxstats_class %>%
  mutate(
    b = avg_income_above/lower_bound
  )

# Get a (Pareto coefficient)

incometaxstats_class <- incometaxstats_class %>%
  mutate(
    a = b/(b-1)
  )

# Focus on average coefficient above specific percentile. Here again look at top 5% 

pareto_a_indirect <- incometaxstats_class %>%
  filter(percentile_lb > 0.95) %>%
  summarise(mean_a = mean(a, na.rm = TRUE)) %>%
  pull(mean_a)

# Add optimal tax rate to table, when based on Pareto coefficient from tabular data  

toptaxes <-  toptaxes %>%
  mutate(top_tax_rate_indirect = estimate_T_top(g, elasticity, pareto_a_indirect))

# Plot comparison between SOEP and tabulated data

gr_session02_06_toptaxes_g0 <- toptaxes %>%
  filter(g == 0) %>%
  select(elasticity, top_tax_rate_direct, top_tax_rate_indirect) %>%
  pivot_longer(cols = starts_with("top_tax_rate"), names_to = "estimation", values_to = "tax_rate") %>%
  mutate(estimation = recode(estimation,
                             "top_tax_rate_direct" = "SOEP (2019)",
                             "top_tax_rate_indirect" = "Tabulated Data (2021)")) %>%
  ggplot(aes(x = elasticity, y = tax_rate, color = estimation, linetype = estimation)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_linetype_manual(values = c("SOEP (2019)" = "dashed", "Tabulated Data (2021)" = "solid")) +
  labs(
    x = "Elasticity",
    y = "Optimal Top Tax Rate (g = 0)",
    color = "Estimation",
    linetype = "Estimation",
    title = "Optimal Top Tax Rate (g = 0)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, by = 0.2))

gr_session02_06_toptaxes_g0
ggsave(gr_session02_06_toptaxes_g0, file = "03-output/gr_session02_06_toptaxes_g0.pdf")

gr_session02_07_toptaxes_g0_tabonly <- toptaxes %>%
  filter(g == 0) %>%
  ggplot(aes(x = elasticity, 
             y = top_tax_rate_indirect,
             colour = "Optimal Top Rate")) +   # legend entry for blue line
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # Actual top tax rate
  geom_hline(aes(yintercept = 0.45,
                 colour = "Actual Top Rate"),
             linewidth = 1) +
  # Colour legend
  scale_colour_manual(
    name = "",
    values = c(
      "Optimal Top Rate" = "#0072B2",
      "Actual Top Rate"  = "black"
    )
  ) +
  labs(
    x = "Elasticity",
    y = "Optimal Top Tax Rate (g = 0)",
    title = "Optimal Top Tax Rate (Tabulated Data, g = 0)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, by = 0.2))

gr_session02_07_toptaxes_g0_tabonly
ggsave(gr_session02_07_toptaxes_g0_tabonly, file = "03-output/gr_session02_07_toptaxes_g0_tabonly.pdf")

# Robustness: scale down by consumption tax 

t_c = 0.19 

toptaxes <- toptaxes %>% 
  mutate(
    top_tax_rate_indirect_corr_cons = top_tax_rate_indirect*(1-t_c)
  )

gr_session02_08_toptaxes_g0_tabonly_corr <- ggplot(data = toptaxes %>% filter(g == 0)) +
  geom_line(aes(x = elasticity, y = top_tax_rate_indirect,
                colour = "Optimal")) +
  geom_point(aes(x = elasticity, y = top_tax_rate_indirect,
                colour = "Optimal")) +
  geom_line(aes(x = elasticity, y = top_tax_rate_indirect_corr_cons,
                colour = "Optimal, corrected for VAT")) +
  geom_point(aes(x = elasticity, y = top_tax_rate_indirect_corr_cons,
                colour = "Optimal, corrected for VAT")) +
  geom_hline(aes(yintercept = 0.45,
                 colour = "Actual")) +
  labs(
    x = "Elasticity",
    y = "Top Tax Rate",
    title = "Top Tax Rate (Tabulated Data, g = 0)",
    colour = NULL,
    linetype = NULL
  ) +
  scale_colour_manual(
    values = c(
      "Optimal" = "#0072B2",
      "Optimal, corrected for VAT" = "#006400",
      "Actual" = "black"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, by = 0.2))


gr_session02_08_toptaxes_g0_tabonly_corr

ggsave(gr_session02_08_toptaxes_g0_tabonly_corr, file = "03-output/gr_session02_08_toptaxes_g0_tabonly_corr.pdf")
