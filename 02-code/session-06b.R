# Session 06: Microsimulation II

# PART B: Here we look at singles with two children

# In session 06 we use the gettsim microsimulation model to study effective marginal tax rates in Germany. 
# The main execution of gettsim is presented in the jupyter notebook to this session. Here we only read 
# in the data for further processing and plotting of results.

# Preamble -------------------------------------------------------------------------

library(tidyverse)
library(pracma) # numerical math functions
library(readr) # read in csv files

# Load Data from GETTSIM -------------------------------------------------------------------------

data_gettsim_orig <- read_csv("./04-data/processed/df_merged_withchild.csv")

data_gettsim_orig <- data_gettsim_orig %>%
  filter(arbeitsstunden_w==40)

# Get tax and transfer componen

data_gettsim <- data_gettsim_orig %>%
  mutate(earnings_y = einnahmen__bruttolohn_m * 12,
         incometax_y = einkommensteuer_betrag_y_sn,
         transfer_basic_y = bürgergeld_betrag_y_bg,
         transfer_housing_y = wohngeld_betrag_y_wthh,
         transfer_child_kinderzuschlag_y = kinderzuschlag_betrag_y_bg,
         transfer_child_kindergeld_y = kindergeld_betrag_y,
         transfer_child_alimony_y = unterhaltsvorschuss_an_elternteil_auszuzahlender_betrag_y,
         sic_pension_y = sozialversicherung_rente_beitrag_betrag_versicherter_y,
         sic_health_y = sozialversicherung_kranken_beitrag_betrag_versicherter_y,
         sic_unemployment_y = sozialversicherung_arbeitslosen_beitrag_betrag_versicherter_y,
         sic_longtermcare_y = sozialversicherung_pflege_beitrag_betrag_versicherter_y) %>%
  select(p_id,
         earnings_y,
         incometax_y,
         transfer_basic_y,
         transfer_housing_y,
         transfer_child_kinderzuschlag_y,
         transfer_child_kindergeld_y,
         transfer_child_alimony_y,
         sic_health_y,
         sic_pension_y,
         sic_unemployment_y,
         sic_longtermcare_y)

# Get net earnings 

data_gettsim <- data_gettsim %>%
  mutate(net_earnings_y = earnings_y 
         - incometax_y - sic_pension_y - sic_health_y - sic_unemployment_y - sic_longtermcare_y
         + transfer_basic_y + transfer_housing_y 
         + transfer_child_kinderzuschlag_y + transfer_child_kindergeld_y + transfer_child_alimony_y)

# Net tax liability -------------------------------------------------------------------------

# Get net tax liability 

data_gettsim <- data_gettsim %>%
  mutate(net_tax_liability_y = incometax_y + sic_pension_y + sic_health_y + sic_unemployment_y + sic_longtermcare_y
         - transfer_basic_y - transfer_housing_y 
         - transfer_child_kinderzuschlag_y - transfer_child_kindergeld_y - transfer_child_alimony_y)

# Plot composition of net tax liability 

gettsim_composition <- data_gettsim %>%
  select(earnings_y, incometax_y, sic_pension_y, sic_health_y, sic_unemployment_y, sic_longtermcare_y,
         transfer_basic_y, transfer_housing_y, transfer_child_kinderzuschlag_y, transfer_child_kindergeld_y, 
         transfer_child_alimony_y, net_tax_liability_y) %>%
  mutate(
    transfer_basic_y = -transfer_basic_y,
    transfer_housing_y = -transfer_housing_y,
    transfer_child_kinderzuschlag_y = - transfer_child_kinderzuschlag_y,
    transfer_child_kindergeld_y = - transfer_child_kindergeld_y,
    transfer_child_alimony_y = - transfer_child_alimony_y
  ) %>%
  pivot_longer(cols = c(incometax_y, sic_pension_y, sic_health_y, sic_unemployment_y, sic_longtermcare_y,
                        transfer_basic_y, transfer_housing_y, transfer_child_kinderzuschlag_y, transfer_child_kindergeld_y,
                        transfer_child_alimony_y),
               names_to = "component", values_to = "amount") %>%
  mutate(component = recode(component,
                            "incometax_y" = "Income Tax",
                            "sic_pension_y" = "Pension SIC",
                            "sic_health_y" = "Health SIC",
                            "sic_unemployment_y" = "Unemployment SIC",
                            "sic_longtermcare_y" = "Long-term Care SIC",
                            "transfer_basic_y" = "Basic Transfer (Bürgergeld)",
                            "transfer_housing_y" = "Housing Transfer",
                            "transfer_child_kinderzuschlag_y" = "Child Transfer (Kinderzuschlag)",
                            "transfer_child_kindergeld_y" = "Child Transfer (Kindergeld)",
                            "transfer_child_alimony_y" = "Child Transfer (Unterhaltsvorschuss)"))

# Plot net tax liability by component 

ggplot(data = gettsim_composition, aes(x = earnings_y, y = amount, fill = component)) +
  geom_area(alpha = 0.7, position = "stack") +
  geom_line(aes(y = net_tax_liability_y, color = "Net Tax Liability"), linewidth = 1, stat = "summary", fun = mean) +
  scale_x_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks(),limits = c(0,250000)) +
  scale_y_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks()) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_manual(values = c("Net Tax Liability" = "black")) +
  labs(
    x = "Annual Gross Earnings (EUR)",
    y = "Component Amount (EUR)",
    title = "Single with two children, decomposition of net tax liability",
    fill = "",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(nrow = 4, byrow = FALSE),
    color = guide_legend(nrow = 4, byrow = FALSE)
  )


ggsave("03-output/gr_session06_07_gettsim_single_twochild_nettaxliability_decomposition_2025.pdf", width = 10, height = 6, dpi = 300)

# Marginal tax rate -------------------------------------------------------------------------

# Estimate marginal tax rates via numerical differentiation (here using gradient, could also use diff)

data_gettsim <- data_gettsim %>%
  arrange(earnings_y) %>%
  mutate(
    emtr = gradient(net_tax_liability_y, earnings_y),
    mtr_incometax = gradient(incometax_y, earnings_y),
    mtr_sic_pension = gradient(sic_pension_y, earnings_y),
    mtr_sic_health = gradient(sic_health_y, earnings_y),
    mtr_sic_unemployment = gradient(sic_unemployment_y, earnings_y),
    mtr_sic_longtermcare = gradient(sic_longtermcare_y, earnings_y),
    mtr_transfer_basic = -gradient(transfer_basic_y, earnings_y),
    mtr_transfer_housing = -gradient(transfer_housing_y, earnings_y),
    mtr_transfer_child_kinderzuschlag_y = -gradient(transfer_child_kinderzuschlag_y, earnings_y),
    mtr_transfer_child_kindergeld_y = -gradient(transfer_child_kindergeld_y, earnings_y),
    mtr_transfer_child_alimony_y = -gradient(transfer_child_alimony_y, earnings_y)
  )

# Correction regarding negative mtr
# At the notches, where the tax liability decreases discontinuously, we have an infinitely negative EMTR. 
# We exclude this here, because it is only relevant for one infinitely small income range 

data_gettsim <- data_gettsim %>%
  mutate(emtr = if_else(emtr < 0, NA_real_, emtr))

data_gettsim <- data_gettsim %>%
  arrange(earnings_y) %>%
  mutate(emtr = approx(x = earnings_y[!is.na(emtr)], 
                       y = emtr[!is.na(emtr)], 
                       xout = earnings_y, 
                       rule = 2)$y)

# Make the data long by the mtr component type 

gettsim_emtr <- data_gettsim %>%
  select(earnings_y, emtr, mtr_incometax, mtr_sic_pension, mtr_sic_health, mtr_sic_unemployment,
         mtr_sic_longtermcare, mtr_transfer_basic, mtr_transfer_housing, mtr_transfer_child_kinderzuschlag_y,
         mtr_transfer_child_kindergeld_y,mtr_transfer_child_alimony_y) %>%
  pivot_longer(
    cols = -earnings_y,
    names_to = "component",
    values_to = "rate"
  ) %>%
  mutate(
    component = recode(component,
                       "emtr" = "Net Tax Liability (EMTR)",
                       "mtr_incometax" = "Income Tax",
                       "mtr_sic_pension" = "Pension SIC",
                       "mtr_sic_health" = "Health SIC",
                       "mtr_sic_unemployment" = "Unemployment SIC",
                       "mtr_sic_longtermcare" = "Long-term Care SIC",
                       "mtr_transfer_basic" = "Basic Transfer (Bürgergeld)",
                       "mtr_transfer_housing" = "Housing Transfer",
                       "mtr_transfer_child_kinderzuschlag_y" = "Child Transfer (Kinderzuschlag)",
                       "mtr_transfer_child_kindergeld_y" = "Child Transfer (Kindergeld)",
                       "mtr_transfer_child_alimony_y" = "Child Transfer (Unterhaltsvorschuss)"))

cols_set3 <- RColorBrewer::brewer.pal(10, "Set3")
component_colors <- c(
  "Net Tax Liability (EMTR)" = "black",
  "Income Tax" = cols_set3[7] ,
  "Pension SIC" = cols_set3[9] ,
  "Health SIC" = cols_set3[5],
  "Unemployment SIC" = cols_set3[10] ,
  "Long-term Care SIC" = cols_set3[8] ,
  "Basic Transfer (Bürgergeld)" = cols_set3[1],
  "Housing Transfer" = cols_set3[6]  ,
  "Child Transfer (Kinderzuschlag)" =  cols_set3[3],
  "Child Transfer (Kindergeld)" =  cols_set3[2],
  "Child Transfer (Unterhaltsvorschuss)" = cols_set3[4] 
)

# Plot decomposition of EMTR 

ggplot(gettsim_emtr, aes(x = earnings_y, y = rate, color = component)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  scale_x_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks(),limits = c(0,250000)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = scales::pretty_breaks(), limits = c(-0.25, 1.25)) +
  scale_color_manual(values = component_colors) +
  labs(
    x = "Annual Gross Earnings (EUR)",
    y = "Rate",
    title = "Single with two children, EMTR and components by earnings",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_linetype_manual(values = c("EMTR" = "dashed", rep("solid", 8))) +
  geom_hline(yintercept = 0) +
  guides(color = guide_legend(nrow = 4, byrow = FALSE))

ggsave("03-output/gr_session06_08_gettsim_single_twochild_emtr_decomposition_2025.pdf", width = 10, height = 6, dpi = 300)

# Export data for reform analysis in session 07 -------------------------------------------------------------------------

data_gettsim_twochild <- data_gettsim
save(data_gettsim_twochild, file = "04-data/processed/data_gettsim_single_twochild_2025.RData")

# Clean up  -------------------------------------------------------------------------

rm(list = ls())