# Session 06: Microsimulation II

# In session 06 we use the gettsim microsimulation model to study effective marginal tax rates in Germany. 
# The main execution of gettsim is presented in the jupyter notebook to this session. Here we only read 
# in the data for further processing and plotting of results.

# Preamble -------------------------------------------------------------------------

library(tidyverse)
library(pracma) # numerical math functions
library(readr) # read in csv files

# Load Data from GETTSIM -------------------------------------------------------------------------

data_gettsim_orig <- read_csv("./04-data/processed/df_merged.csv")

# Get tax and transfer components

data_gettsim <- data_gettsim_orig %>%
  mutate(earnings_y = einnahmen__bruttolohn_m * 12,
         incometax_y = einkommensteuer_betrag_y_sn,
         transfer_basic_y = bürgergeld_betrag_y_bg,
         transfer_housing_y = wohngeld_betrag_y_wthh,
         transfer_child_y = kinderzuschlag_betrag_y_bg,
         sic_pension_y = sozialversicherung_rente_beitrag_betrag_versicherter_y,
         sic_health_y = sozialversicherung_kranken_beitrag_betrag_versicherter_y,
         sic_unemployment_y = sozialversicherung_arbeitslosen_beitrag_betrag_versicherter_y,
         sic_longtermcare_y = sozialversicherung_pflege_beitrag_betrag_versicherter_y) %>%
  select(p_id,
         earnings_y,
         incometax_y,
         transfer_basic_y,
         transfer_housing_y,
         transfer_child_y,
         sic_health_y,
         sic_pension_y,
         sic_unemployment_y,
         sic_longtermcare_y)

# Get net earnings 

data_gettsim <- data_gettsim %>%
  mutate(net_earnings_y = earnings_y 
         - incometax_y - sic_pension_y - sic_health_y - sic_unemployment_y - sic_longtermcare_y
         + transfer_basic_y + transfer_housing_y + transfer_child_y)

# Net tax liability -------------------------------------------------------------------------

# Get net tax liability 

data_gettsim <- data_gettsim %>%
  mutate(net_tax_liability_y = incometax_y + sic_pension_y + sic_health_y + sic_unemployment_y + sic_longtermcare_y
         - transfer_basic_y - transfer_housing_y - transfer_child_y)

# Plot composition of net tax liability 

gettsim_composition <- data_gettsim %>%
  select(earnings_y, incometax_y, sic_pension_y, sic_health_y, sic_unemployment_y, sic_longtermcare_y,
         transfer_basic_y, transfer_housing_y, transfer_child_y, net_tax_liability_y) %>%
  mutate(
    transfer_basic_y = -transfer_basic_y,
    transfer_housing_y = -transfer_housing_y,
    transfer_child_y = - transfer_child_y,
  ) %>%
  pivot_longer(cols = c(incometax_y, sic_pension_y, sic_health_y, sic_unemployment_y, sic_longtermcare_y,
                        transfer_basic_y, transfer_housing_y, transfer_child_y),
               names_to = "component", values_to = "amount") %>%
  mutate(component = recode(component,
                            "incometax_y" = "Income Tax",
                            "sic_pension_y" = "Pension SIC",
                            "sic_health_y" = "Health SIC",
                            "sic_unemployment_y" = "Unemployment SIC",
                            "sic_longtermcare_y" = "Long-term Care SIC",
                            "transfer_basic_y" = "Basic Transfer (Bürgergeld)",
                            "transfer_housing_y" = "Housing Transfer",
                            "transfer_child_y" = "Child Transfer"))

# Plot net tax liability by component 

ggplot(data=gettsim_composition,aes(x = earnings_y, y = amount, fill = component)) +
  geom_area(alpha = 0.7, position = "stack") +
  geom_line(aes(y = net_tax_liability_y, color = "Net Tax Liability"), linewidth  = 1, stat = "summary", fun = mean) +
  scale_x_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::label_dollar(), breaks = scales::pretty_breaks()) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_manual(values = c("Net Tax Liability" = "black")) +
  labs(
    x = "Annual Gross Earnings (EUR)",
    y = "Component Amount (EUR)",
    title = "Decomposition of Net Tax Liability",
    fill = "",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(nrow = 2, byrow = FALSE),
    color = guide_legend(nrow = 2, byrow = FALSE)
  )

ggsave("03-output/gr_session06_05_gettsim_nettaxliability_decomposition_2025.pdf", width = 10, height = 6, dpi = 300)

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
    mtr_transfer_child = -gradient(transfer_child_y, earnings_y)
  )

# Make the data long by the mtr component type 

gettsim_emtr <- data_gettsim %>%
  select(earnings_y, emtr, mtr_incometax, mtr_sic_pension, mtr_sic_health, mtr_sic_unemployment,
         mtr_sic_longtermcare, mtr_transfer_basic, mtr_transfer_housing, mtr_transfer_child) %>%
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
                       "mtr_transfer_child" = "Child Transfer"
    )
  )

cols_set2 <- RColorBrewer::brewer.pal(8, "Set2")
component_colors <- c(
  "Net Tax Liability (EMTR)" = "black",
  "Income Tax" = cols_set2[5],
  "Pension SIC" = cols_set2[7],
  "Health SIC" = cols_set2[3],
  "Unemployment SIC" = cols_set2[8],
  "Long-term Care SIC" = cols_set2[6],
  "Basic Transfer (Bürgergeld)" = cols_set2[1],
  "Housing Transfer" = cols_set2[4],
  "Child Transfer" = cols_set2[2]
)

# Plot decomposition of EMTR 

ggplot(gettsim_emtr, aes(x = earnings_y, y = rate, color = component)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  scale_x_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = scales::pretty_breaks(), limits = c(-0.25, 1.25)) +
  scale_color_manual(values = component_colors) +
  labs(
    x = "Annual Gross Earnings (EUR)",
    y = "Rate",
    title = "EMTR and Components by Earnings",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_linetype_manual(values = c("EMTR" = "dashed", rep("solid", 8))) +
  geom_hline(yintercept = 0)

ggsave("03-output/gr_session06_06_gettsim_emtr_decomposition_2025.pdf", width = 10, height = 6, dpi = 300)

