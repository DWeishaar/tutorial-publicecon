# Session 05: Microsimulation I

# Preamble -------------------------------------------------------------------------

library(tidyverse)
library(pracma) # numerical math functions
library(maps) # for maps
library(mapproj) # for plotting country map 

# Example Household 01 -------------------------------------------------------------------------

## Step 01: Specify tax system of interest ------------------------

# Tax system

taxsystem <- tibble(
  year = 2000
)

## Step 02: Specify household characteristics -------------------------------------------------------------------------

# Household characteristics

household_example <- tibble(
  state  = 18,
  mstat  = 2,
  page   = 45,
  pwages = 75000,
  sage   = 42,
  swages = 0,
  depx   = 2,
  age1   = 16,
  age2   = 13
)

## Step 03: Specify controls of TAXSIM ------------------------------------------------------

# See: https://taxsim.nber.org/taxsim35/low-level-remote.html
spec <- tibble(
  idtl = 2,  # level of detail in output (2 = detailed output)
  mtr  = 11  # which marginal tax rate to return (per TAXSIM documentation)
)

# Generate combined input data for TAXSIM

# Combine tax system information, household characteristics, and specifications
input_taxsim <- bind_cols(taxsystem, household_example, spec)

# Assign taxsimid (row number)
input_taxsim <- input_taxsim %>%
  mutate(taxsimid = row_number()) %>%
  select(taxsimid, everything())

## Step 04: Use TAXSIM to compute tax liability ---------------------------------------------

### Brief check whether SSH is enabled / installed ----------------------------------

ssh_path <- Sys.which("ssh")
if (ssh_path == "") {
  stop(
    "No 'ssh' executable found on your system PATH.\n",
    "Windows: install 'OpenSSH Client' (Settings -> Apps -> Optional features).\n",
    "macOS: if prompted, install Command Line Tools (xcode-select --install)."
  )
}

### Send via SSH to TAXSIM  ----------------------------------

taxsim <- function(input_taxsim) {

  # Generate temporary files (needed for sending to / receiving from NBER server)
  infile  <- tempfile(fileext = ".csv")
  outfile <- tempfile(fileext = ".csv")
  
  # Write the input_taxsim data into the temporary input file
  write.csv(input_taxsim, infile, row.names = FALSE, na = "")
  
  # Run TAXSIM via SSH (cross-platform robust version using system2)
  # - stdin  : send local CSV file to TAXSIM server
  # - stdout : save returned results to local CSV file
  # - stderr : capture error messages for debugging
  
  status <- system2(
    command = "ssh",
    args = c(
      "-T",                                  # no pseudo-terminal (non-interactive)
      "-o", "StrictHostKeyChecking=no",      # don't ask to trust the host
      "-o", "UserKnownHostsFile=/dev/null",  # don't store host keys
      "-p", "22",                            # default SSH port (try 443 or 80 if 22 is blocked)
      "taxsim35@taxsimssh.nber.org"          # TAXSIM SSH server (user@host)
    ),
    stdin  = infile,
    stdout = outfile
  )
  
  # If SSH returns a non-zero status, the call failed
  if (status != 0) {
    stop(
      "TAXSIM SSH call failed.\n",
      "Try changing the SSH port to 443 or 80 (some networks block port 22).\n",
      "Also check that your firewall/VPN allows outgoing SSH connections."
    )
  }
  
  # Read TAXSIM output
  
  output_taxsim <- read.csv(outfile)
  
  # Combine with input 
  
  output_taxsim <- left_join(
    input_taxsim,
    output_taxsim,
    by = c("taxsimid", "year", "state")
  )
  
  return(output_taxsim)
}

# Execute function 

output_taxsim_ex01 <- taxsim(input_taxsim)

## Step 05: Analyze results---------------------------------------------------

# Inspect results

print(output_taxsim_ex01 %>% select(fiitax,siitax,frate,srate))

# Multiple Households, vary pwages from 0 to 400000 -------------------------------------------------------------------------

income_seq <- tibble(pwages = seq(0, 400000, by = 100))

mult_households <- income_seq %>%
  mutate(
    state  = 18,
    mstat  = 2,
    page   = 45,
    sage   = 42,
    swages = 0,
    depx   = 2,
    age1   = 16,
    age2   = 13
  )

input_taxsim_mult <- bind_cols(taxsystem,mult_households,spec)  %>%
  mutate(taxsimid = row_number()) %>%
  select(taxsimid, everything())

output_taxsim_mult <- taxsim(input_taxsim_mult)

# Plot federal MTR and ATR 

output_taxsim_mult <- output_taxsim_mult %>% 
  mutate(
    twages = pwages + swages,
    atr_fiitax = if_else(twages == 0, NA_real_, fiitax/twages*100),
    atr_siitax = if_else(twages == 0, NA_real_, siitax/twages*100)
  )

p01 <- ggplot(output_taxsim_mult) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = twages, y = frate, color = "Marginal Tax Rate", linetype = "Marginal Tax Rate")) +
  geom_line(aes(x = twages, y = atr_fiitax, color = "Average Tax Rate", linetype = "Average Tax Rate")) +
  labs(
    x = "Total Wages",
    y = "MTR",
    color = "Legend",
    title = "Federal Marginal and Average Income Tax Rate (2000)",
    linetype = "Legend"
  ) +
  scale_color_manual(
    values = c("Marginal Tax Rate" = "darkorange", "Average Tax Rate" = "darkorange")
  ) +
  scale_linetype_manual(
    values = c("Marginal Tax Rate" = "dashed", "Average Tax Rate" = "solid")
  ) +
  scale_x_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1),limits = c(-40,50)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = NULL), linetype = guide_legend(title = NULL))
ggsave("03-output/gr_session05_01_frate.pdf", p01)

# Plot state MTR

p02 <- ggplot(output_taxsim_mult) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = twages, y = srate, color = "Marginal Tax Rate", linetype = "Marginal Tax Rate")) +
  geom_line(aes(x = twages, y = atr_siitax, color = "Average Tax Rate", linetype = "Average Tax Rate")) +
  labs(
    x = "Total Wages",
    y = "State Tax Rate",
    title = "State Marginal and Average Income Tax Rate (2000)",
    color = "Legend",
    linetype = "Legend"
  ) +
  scale_color_manual(values = c("Marginal Tax Rate" = "darkblue", "Average Tax Rate" = "darkblue")) +
  scale_linetype_manual(values = c("Marginal Tax Rate" = "dashed", "Average Tax Rate" = "solid")) +
  scale_x_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::label_percent(scale = 1),limits = c(-40,50)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = NULL), linetype = guide_legend(title = NULL))

ggsave("03-output/gr_session05_02_srate.pdf", p02)

## Decompose fiitax -------------------------------------------------------------------------

# Decompose fiitax into different components 
# Careful, in other years / for other households, more than these variables might be 
# relevant. 

output_taxsim_mult <- output_taxsim_mult %>%
  rename(
    fiitax_beforecredits = v28,
    child_tax_credit = v22,
    earned_income_credit = v25
  )

# Check whether all relevant compontents are included 

output_taxsim_mult %>%
  mutate(
    fiitax_check = fiitax_beforecredits  -
      (child_tax_credit + earned_income_credit),
    diff = fiitax - fiitax_check
  ) %>%
  summarise(
    max_abs_diff = max(abs(diff), na.rm = TRUE),
    mean_diff = mean(diff, na.rm = TRUE)
  )

# Turn tax credits negative for plotting 

output_taxsim_mult_gr <- output_taxsim_mult %>%
  mutate(
    neg_child_tax_credit = -child_tax_credit,
    neg_earned_income_credit = -earned_income_credit
  ) 

p_03 <- ggplot(data=output_taxsim_mult_gr, aes(x = twages)) +
  geom_area(aes(y = fiitax_beforecredits, fill = "Tax (before credits)"), alpha = 0.6) +
  geom_area(aes(y = neg_child_tax_credit, fill = "Child Tax Credit"), alpha = 0.7) +
  geom_area(aes(y = neg_earned_income_credit, fill = "EITC"), alpha = 0.7) +
  geom_line(aes(y = fiitax, color = "Net Income Tax"), size = 1) +
  labs(
    x = "Total Wages",
    y = "Tax Liability",
    title = "Federal Income Tax Liability Decomposition (2000)",
    fill = "",
    color = NULL
  ) +
  scale_x_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks(), limits =c(0,100000)) +
  scale_y_continuous(labels = scales::label_dollar(), breaks = scales::pretty_breaks(), limits =c(-10000,25000)) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_manual(values = c("Net Income Tax" = "black")) +
  theme_minimal() +
  theme(legend.position = "bottom")+
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(nrow = 2, byrow = TRUE)
  )


ggsave("03-output/gr_session05_03_fiitax_decomp.pdf", p_03, width = 7, height = 5)

## Decompose EMTR -------------------------------------------------------------------------

# Compute numerical marginal tax rates for each component by looking at dT/dY

output_taxsim_mult <- output_taxsim_mult %>%
  arrange(twages) %>%
  mutate(
    mtr_fiitax_beforecredits = c(NA, diff(fiitax_beforecredits)/diff(twages))*100,
    mtr_child_tax_credit = c(NA, diff(child_tax_credit)/diff(twages))*100,
    mtr_earned_income_credit = c(NA, diff(earned_income_credit)/diff(twages))*100,
    mtr_sum = mtr_fiitax_beforecredits - mtr_child_tax_credit - mtr_earned_income_credit,
    mtr_fiitax_numerical = c(NA, diff(fiitax)/diff(twages))*100
  )

# Plot decomposition of marginal tax rate

p_04 <- ggplot(output_taxsim_mult, aes(x = twages)) +
  geom_line(aes(y = mtr_fiitax_beforecredits, color = "Tax Before Credits"), linewidth = 1) +
  geom_line(aes(y = -mtr_child_tax_credit, color = "Child Tax Credit"), linewidth = 1) +
  geom_line(aes(y = -mtr_earned_income_credit, color = "EITC"), linewidth = 1) +
  geom_line(aes(y = mtr_fiitax_numerical, color = "Net Tax"), linewidth = 1, linetype = "dashed") +
  labs(
    x = "Total Wages",
    y = "Marginal Rate",
    title = "Marginal Tax Rate Decomposition (2000)",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "Net Tax" = "black",
      "Tax Before Credits" = "steelblue",
      "Child Tax Credit" = "darkgreen",
      "EITC" = "darkorange"
    ),
    breaks = c(
      "Net Tax",
      "Tax Before Credits",
      "Child Tax Credit",
      "EITC"
    )
  ) +
  scale_x_continuous(labels = scales::label_comma(), breaks = scales::pretty_breaks(), limits = c(0, 100000)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(-60, 60)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session05_04_mtr_decomp.pdf", p_04, width = 7, height = 5)

## Look at Phaseout of Child Tax Credit -------------------------------------------------------------------------

# Look at phaseout of child tax credit 

p_03_higher <- p_03 +
  scale_x_continuous(
    labels = scales::label_comma(),
    breaks = scales::pretty_breaks(),
    limits = c(100000, 150000)
  )+
  scale_y_continuous(
    labels = scales::label_comma(),
    breaks = scales::pretty_breaks(),
    limits = c(-10000, 50000)
  )  

ggsave("03-output/gr_session05_03b_fiitax_decomp_high.pdf", p_03_higher, width = 7, height = 5)

p_04_higher <- p_04 +
  scale_x_continuous(
    labels = scales::label_comma(),
    breaks = scales::pretty_breaks(),
    limits = c(100000, 150000)
  )
ggsave("03-output/gr_session05_04b_mtr_decomp_high.pdf", p_04_higher, width = 7, height = 5)

# Multiple Households, Vary state -----------------------------------------

# Vary state codes (1-51, see IRS state codes here: https://taxsim.nber.org/statesoi.html)

states <- tibble(state = 1:51)

# Generate household with different states 

mult_households_state <- states %>%
  mutate(
    mstat  = 2,
    page   = 45,
    pwages = 75000,
    sage   = 42,
    swages = 0,
    depx   = 2,
    age1   = 16,
    age2   = 13
  )

# Generate input data 

input_taxsim_state <- bind_cols(taxsystem, mult_households_state, spec) %>%
  mutate(taxsimid = row_number()) %>%
  select(taxsimid, everything())

output_taxsim_state <- taxsim(input_taxsim_state)

# Define Mapping of state codes 

state_codes_mapping <- tibble(
  state = 1:51,
  state_abb = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
)

# Use map_data (from ggplot2) to generate coordinates for map plotting

map_states <- map_data("state") %>%
  mutate(state_abb = case_when(
    region == "district of columbia" ~ "DC",
    TRUE ~ toupper(state.abb[match(region, tolower(state.name))])
  ))

map_states_plot <- output_taxsim_state %>%
  left_join(state_codes_mapping, by = "state") %>%
  right_join(map_states, by = "state_abb")


# Plot map 

p_05 <- ggplot(map_states_plot, aes(long, lat, group = group, fill = srate)) +
  geom_polygon(color = "black") +
  coord_map() +
  scale_fill_gradient(
    name   = "State MTR",
    low    = "white",
    high   = "steelblue",
    labels = scales::label_percent(scale = 1)
  )+
  labs(title = "State Marginal Tax Rate (MTR) for Example Household (2000)") +
  theme_void() +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session05_05_state_mtr_map.pdf", p_05, width = 7, height = 5)

# Maximum state mtr for example household 

output_taxsim_state %>%
  left_join(state_codes_mapping, by = "state") %>%
  filter(srate == max(srate, na.rm = TRUE)) %>%
  select(state, state_abb, srate)

# Minimum state mtr=0 for example household 

output_taxsim_state %>%
  filter(srate == 0) %>%
  left_join(state_codes_mapping, by = "state") %>%
  mutate(state_name = state.name[match(state_abb, state.abb)]) %>%
  select(state, state_abb, state_name, srate)

output_taxsim_state %>%
  filter(state == 20) %>%
  pull(srate)
