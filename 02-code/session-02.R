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

# to be continued next time 



