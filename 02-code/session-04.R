# Session 04: Inverse-Optimum Approach

# Preamble -------------------------------------------------------------------------

library(tidyverse)
library(pracma) # numerical math functions

# 01 Load optimal tax data from session 3 --------------------------------------------------

load("04-data/processed/data_opttax_long.RData")

# 02 Invert optimal tax rate formula -------------------------------------------------------------------------

# Limit income grid and keep only necessary variables 

data_opttax_long <- data_opttax_long %>%
  filter(income<750000) %>%
  select(elasticity,income,omega,mtr_HSV,pdf_pareto,cdf_pareto,tprime) 
  
# Generate omega' by numerical differentiation

data_opttax_long <- data_opttax_long %>%
  group_by(elasticity) %>%
  arrange(income) %>%
  mutate(
    diff_y = income - lag(income),
    diff_omega = omega - lag(omega),
    omega_prime = diff_omega/diff_y
  )

# Generate cummulative weights G() 

data_opttax_long <- data_opttax_long %>%
  mutate(
    G = 1 - (mtr_HSV/(1-mtr_HSV)) * (pdf_pareto/(1-cdf_pareto)) * (omega/omega_prime) * elasticity
  )

# Plot G() over income for one elasticity value 

p1 <- ggplot(data_opttax_long %>% filter(elasticity == 0.5), aes(x = income, y = G)) +
  geom_line(color="darkgreen") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(x = "Income", y = "G", title = "G over Income (Elasticity = 0.5)") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma, limits = c(0, 400000)) +
  scale_y_continuous(limits = c(-2, 2))

ggsave("03-output/gr_session04_01_G.pdf", p1)

# Plot optimal tax rate against HSV actual one 

p2 <- ggplot(data_opttax_long %>% filter(elasticity == 0.5)) +
  geom_line(aes(x = income, y = mtr_HSV, color = "MTR (Status quo)")) +
  geom_line(aes(x = income, y = tprime, color = "Revenue-Maximizing MTR (e=0.5)")) +
  scale_color_manual(values = c("MTR (Status quo)" = "black", "Revenue-Maximizing MTR (e=0.5)" = "darkgreen")) +
  labs(x = "Income", y = NULL, title = "MTR Schedule (Elasticity = 0.5)", color = "") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma, limits = c(0, 400000)) +
  scale_y_continuous(limits = c(0, 1))+
  theme(legend.position = "bottom")

ggsave("03-output/gr_session04_02_mtr_tprime.pdf", p2)

# 03 Obtain g() -------------------------------------------------------------------------

# Add G' (numerical differentiation)

data_opttax_long <- data_opttax_long %>%
  group_by(elasticity) %>%
  arrange(income) %>%
  mutate(
    diff_G = G - lag(G),
    G_prime = diff_G/diff_y
  )

# Compute g

data_opttax_long <- data_opttax_long %>% 
  mutate(
    g = -G_prime/(pdf_pareto)
  )

# Compute g

p3 <- ggplot(data_opttax_long %>% filter(elasticity == 0.5), aes(x = income, y = g)) +
  geom_line(color = "darkgreen") +
  labs(x = "Income", y = "g", title = "g over Income (Elasticity = 0.5)") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(labels = scales::comma, limits = c(0, 400000)) +
  scale_y_continuous(limits = c(-250, 250))

ggsave("03-output/gr_session04_03_g.pdf", p3)

# 04 Interpretation of g(y) -------------------------------------------------------------------------

p4 <- ggplot(data_opttax_long %>% filter(elasticity == 0.5, income >= 0, income <= 30000), aes(x = income, y = g)) +
  geom_line(color = "darkgreen") +
  labs(x = "Income", y = "g", title = "g over Income (Elasticity = 0.5), 0-30k") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(labels = scales::comma, limits = c(0, 30000)) +
  scale_y_continuous(limits = c(-3, 3))+
  geom_vline(xintercept = 10000, color = "gray", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 25000, color = "gray", linetype = "dashed", linewidth = 1)

ggsave("03-output/gr_session04_04_g_0_30k.pdf", p4)

# Extract g(y) for different income levels

g_10k <- data_opttax_long %>% 
  filter(elasticity == 0.5, abs(income - 10000) == min(abs(income - 10000))) %>% 
  pull(g)

g_25k <- data_opttax_long %>% 
  filter(elasticity == 0.5, abs(income - 25000) == min(abs(income - 25000))) %>% 
  pull(g)