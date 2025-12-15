# Preamble -------------------------------------------------------------------------

library(tidyverse)
library(lokern)

# 01 Kernel Density Toy Example --------------------------------------------------

# Write a function for the Gaussian kernel

kernelfunction <- function(x) {
  exp(-x^2 / 2) / sqrt(2 * pi)
}

# Generate data set with kernel 

data_kernel <- tibble(
  x = seq(-10, 10, length.out = 1000), # range of x variable
  k = kernelfunction(x)  # kernel function applied to x variable
)

# Plot data point (x=0) 

p0 <- ggplot() +
  geom_point(aes(x = 0, y = 0),
             inherit.aes = FALSE,
             shape = 21, size = 3, fill = "black") +
  labs(
    title = "Data Point",
    x = "x",
    y = "Kernel value"
  ) +
  ylim(0,0.4)+
  xlim(-10,10)+
  theme_minimal(base_size = 14)
ggsave(filename="03-output/gr_session03_01_kdensity_ex1.pdf",p0)

# Plot Gaussian Kernel around data point

p1 <- ggplot(data_kernel, aes(x = x, y = k)) +
  geom_line(linewidth = 1.1) +
  geom_point(data = tibble(x = 0, k = 0), aes(x = x, y = k),
             shape = 21, size = 3, fill = "black") +
  labs(
    title = "Kernel Function",
    x = "x",
    y = "Kernel value"
  ) +
  theme_minimal(base_size = 14)
ggsave(filename="03-output/gr_session03_02_kdensity_ex2.pdf",p1)

# Shift Kernel centered at a single data point

x1 <- 2      # single data point
h1  <- 1      # bandwidth for plotting shift/scale
h2  <- 2      # alternative bandwidth for plotting shift/scale

data_kernel <- data_kernel %>% mutate(
  k_shifted_h1 = (1 / h1) * kernelfunction((x - x1) / h1),
  k_shifted_h2 = (1 / h2) * kernelfunction((x - x1) / h2)
)

p2a <- ggplot(data_kernel, aes(x = x, y = k_shifted_h1)) +
  geom_line(linewidth = 1.1, color = "black") +
  geom_point(
    data = tibble(x = x1, y = 0),
    aes(x = x, y = y),
    shape = 21, size = 3, fill = "black"
  ) +
  labs(
    title = "Kernel centered at one data point, different bandwidth",
    x = "x",
    y = "Density contribution at x"
  ) +
  theme_minimal(base_size = 14)
ggsave(filename="03-output/gr_session03_03_kdensity_ex3a.pdf",p2a)

p2b <- p2a + 
geom_line(aes(y = k_shifted_h2), linewidth = 1.1, color = "darkgreen") 
ggsave(filename="03-output/gr_session03_04_kdensity_ex3b.pdf",p2b)

# Kernel Density Estimator (KDE) for two data points

x_points <- c(-1, 2)   # two data points
n <- length(x_points)
h_kde <- 1             # bandwidth

# Long format: kernel contributions from each point

data_kernel_long <- data_kernel %>%
  select(x) %>%
  crossing(i = factor(seq_along(x_points))) %>%
  mutate(
    x_i = x_points[as.integer(i)],
    k_i = (1 / h_kde) * kernelfunction((x - x_i) / h_kde)
  )

# Summed KDE: average of the two kernels
data_kde <- data_kernel_long |>
  group_by(x) |>
  summarise(f_hat = mean(k_i), .groups = "drop")

p3 <- ggplot() +
  # individual kernel contributions
  geom_line(
    data = data_kernel_long,
    aes(x = x, y = k_i, colour = i),
    linewidth = 0.9,
    a = 0.9
  ) +
  # KDE = sum / n
  geom_line(
    data = data_kde,
    aes(x = x, y = f_hat),
    linewidth = 1.3,
    colour = "black"
  ) +
  # show the two data points on the axis
  geom_point(
    data = tibble(x = x_points, y = 0),
    aes(x = x, y = y),
    inherit.aes = FALSE,
    shape = 21, size = 3, fill = "black"
  ) +
  labs(
    title = "Kernel Density Estimator (KDE) for two data points",
    x = "x",
    y = "Estimated density"
  ) +
  theme_minimal(base_size = 14) +
  guides(colour = "none")
ggsave(filename="03-output/gr_session03_05_kdensity_ex4.pdf",p3)
