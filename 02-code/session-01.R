# Session 01: Introduction into Basic R Commands

# Preamble -------------------------------------------------------------------------

library(tidyverse) # collection of packages for data wrangling, plotting, etc.
library(haven) # import SAS/Stata files
library(labelled) # handle labeled data 

# Generate / Load Data -------------------------------------------------------------------------

x <- 1:5 # example sequence of integers
testdata <- tibble(id = x, income = x*1000) # toy data set with id and income variable

# In this part, we load the training data from GSOEP (.dta files).The original 
# file can be found at https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.833185.de/practice_en.zip. 

# This training data only reflects the structure of the SOEP data set. All analyses are thus
# only for training purposes and do not reflect correct relationships between variables. 

# SOEP data 

soep <- read_dta(file="04-data/raw/practice_en/practice_dataset_eng.dta")

# Data set inspection  -------------------------------------------------------------------------

head(soep) # take a look at the structure of the data set (first 10 rows)
nrow(soep) # number of rows/observations 
ncol(soep) # number of variables

# Basic data set up --------------------------------------------------------

# Rename variable names to English names 

var_label(soep$alter) # variable label (description)
lapply(soep, var_label) # get variable label for all variables (lapply applies the function var_label to all columns of soep)

names(soep) # check variable names 

soep <- soep %>%       # rename all variables to English names
  rename(
    age = alter,
    num_pers = anz_pers,
    num_child = anz_kind,
    educ = bildung,
    empstat = erwerb, 
    industry = branche,
    health_subj = gesund_org,
    lifesat = lebensz_org,
    income_main_y = einkommenj1,
    income_main_m = einkommenm1,
    income_sec_y = einkommenj2,
    income_sec_m = einkommenm2
  )

names(soep) # check if all names are in English

# Checkout class of variable 

class(soep$age) # class function applied to one variable. Age is numeric.
class(soep$sex) # class function applied to one variable. Sex is numeric (double precision), but also contains labels for values
head(soep$sex)

# Arange data (sort rows)

soep <- soep %>% 
  arrange(id,syear)

# Add new variables --------------------------------------------------------

# Add a new variable containing the information whether person is in working age (see definition by OECD here) 

soep <- soep %>%                                      # overwrite soep tibble with new variables
  mutate(                                             # create or modify existing variables
    working_age = factor(                             # create working_age as a factor (categorical variable)
      if_else(                                        # if_else returns 1 if age between 16 and 64, otherwise 2
        age >= 16 & age <= 64, 1, 2),
      levels = c(1, 2),                               # set factor levels: 1 = working age, 2 = not working age
      labels = c("[1] Working age (16–64)", "[2] Not working age") # set factor labels for each level
    )
  ) %>%
  set_variable_labels(                                 # assign variable label for working_age
    working_age = "Working-age indicator (16–64 years)"
  )

# Add a variable for total income 

soep <- soep %>%
  mutate(
    income_tot_y = income_main_y + income_sec_y
  ) %>%
  set_variable_labels(
    income_tot_y = "Sum of annual main and second job gross income."
  )

# Average earnings (total income) for each individual across years

soep <- soep %>%
  group_by(id) %>%                                            # group data by id 
  mutate(
    income_tot_y_mean = mean(income_tot_y, na.rm = TRUE)
  ) %>%
  ungroup()  %>%                                              # ungroup data 
  set_variable_labels(
    income_tot_y_mean = "Average total earnings across years."
  )

save(soep,file="04-data/processed/soep.RData")

# Summary statistics --------------------------------------------------------

# Average and median age 

soep %>% summarise(
  mean_age = mean(age, na.rm = TRUE),
  median_age = median(age, na.rm = TRUE)
)

# Average income by year 

soep %>% 
  group_by(syear) %>%
  summarise(
    mean_income = mean(income_tot_y, na.rm = TRUE),
    median_income = median(income_tot_y, na.rm = TRUE)
  )

# Average income by year and sex

soep %>% 
  group_by(syear,sex) %>%
  summarise(
    mean_income = mean(income_tot_y, na.rm = TRUE),
    median_income = median(income_tot_y, na.rm = TRUE)
  )

# Number of male/female observations

soep %>% count(sex) # absolute count

soep %>%            # relative share
  group_by(sex) %>%
  summarise(n_sex = n()) %>%
  mutate(share = n_sex / sum(n_sex))

# Mean income of female only 

soep %>%
  filter(sex == 1) %>%
  summarise(mean_income = mean(income_tot_y, na.rm = TRUE))

# Plot some graphs -------------------------------------------------------------------------

# Plot the relationship between income and age for the year 2019

ggplot(soep %>% filter(syear == 2019),       # data: only 2019
       aes(x = age, y = income_tot_y)) +     # x: age, y: income
  geom_point(alpha = 0.4, size = 0.01) +     # plot points; semi-transparent, tiny
  geom_smooth(method = "loess", formula = 'y ~ x') + # add loess fit line
  scale_x_continuous(breaks = seq(0, 100, by = 5)) + # x-axis breaks every 5 years
  scale_y_continuous(breaks = seq(0, 200000, by = 50000), # y-axis breaks every 50k
                     labels = scales::comma) +         # comma labels for y-axis
  labs(
    x = "Age",                                # x axis label
    y = "Total income",                       # y axis label
    title = "Age and Total Income (2019)"     # plot title
  ) +
  theme_minimal()                             # minimal theme

# Plot average and median incomes over the years 

# Generate collapsed data with mean and median earnings for each year. 

mean_income <- soep %>% 
  group_by(syear) %>%
  summarise(
    mean_income = mean(income_tot_y, na.rm = TRUE),
    median_income = median(income_tot_y, na.rm = TRUE)
  )

# Generate collapsed data with mean and median earnings for each year. 

ggplot(mean_income)+
  geom_line(aes(x=syear,y = mean_income),color="black") +
  geom_line(aes(x=syear,y=median_income),color="grey")+
  scale_x_continuous(breaks = seq(2014, 2020, by = 1)) +
  scale_y_continuous(breaks = seq(0, 30000, by = 5000),
                     labels = scales::comma, limits = c(0,30000)) +
  labs(x = "Year", y = "Average income (total)", title="Average Earnings over Time") +
  theme_minimal()

# Plot the distribution of incomes in 2019 of all full-time employed individuals with positive earnings

graph_distr <- ggplot(soep %>% filter(syear == 2019 & empstat==1 & income_tot_y>=0),
       aes(x = income_tot_y)) +
  geom_histogram(
    fill = "steelblue", 
    color = "white",
    binwidth = 5000
  ) +
  labs(
    x = "Total Income (annual)",
    y = "Count",
    title = "Histogram of Total Income (2019)"
  )+
  scale_x_continuous(breaks = seq(0, 200000, by = 50000),
                     labels = scales::comma, limits = c(0,200000)) +
  theme_minimal()

graph_distr

ggsave(graph_distr, file="03-output/gr_session01_01_histogram.pdf")
  
# Reshape data to long -------------------------------------------------------------------------

soep_long <- soep %>%
  select(id, syear,
         income_main_y, income_sec_y,
         income_main_m, income_sec_m) %>%
  pivot_longer(
    cols = starts_with("income_"),
    names_to = c("income_type", "period"),
    names_pattern = "income_(.*)_(.)",   # extracts main/sec and y/m
    values_to = "income"
  )

# Some summary statistics by income type and year 

soep_long %>%
  group_by(syear, income_type) %>%
  summarise(mean_income = mean(income, na.rm = TRUE))

# Store data 

save(soep_long,file="04-data/processed/soep_long.RData")

# Set API Key for the connection of R to the model 

Sys.setenv("OPENAI_API_KEY" = "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")

# Additional Exercises for Home -------------------------------------------------------------------------

# 1.) Compute the (relative) gender pay gap in 2019 in annual earnings among 
#      a.) all individuals
#      b.) all employed individuals 
#      c.) all full-time employed individuals

# 2.) Plot the share of self-employed over years.

# 3.) Plot the distribution of the years of education.

# 4.) Reshape the long data set back to wide.

# Some notes on LLM usage in R --------------------------------------------

# Below are some exemplary LLM applications that can be accessed through R. Most of them require 
# paid services.

# Example: API Key for OPENAI for the connection of R to the model 

Sys.setenv("OPENAI_API_KEY" = "XXX")

## Chattr -------------------------------------------------------------------------

# Simple chatting interface (cannot see code / environment)
# Requires OPEN AI API KEY, need to buy credits  

library(chattr)

chattr("How can I make a scatter plot in R?")

## Copilot -----------------------------------------------------------------

# Copilot code completion (can only see open scripts, also those open, but not the environment)
# Needs to be installed via Tools-Global Options-Enable Copilot 
# Requires github account 

# Type something and then use code completion (keyboard shortcut can be specified via Tools-Modify keyboard shortcuts)

## Gander package ----------------------------------------------------------

# Code commenting / debugging / completion
# Can mark code and then be instructed. 
# Can see all code and environment structure, not the data itself

# Requires OPEN AI API KEY, need to buy credits  

library(gander)
options(.gander_chat = ellmer::chat_openai())
gander_peek()

## Claude Code -------------------------------------------------------------

# Claude code (via Terminal, has full access to data, code and folder structure)
# For installation instructions, see https://www.claude.com/product/claude-code
# Deep integration requires careful tracking of changes via git.