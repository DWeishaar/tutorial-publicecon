# Preamble -------------------------------------------------------------------------

library(tidyverse)
library(pracma) # numerical math functions

# 01 Kernel Density Toy Example --------------------------------------------------

# Following slides, we program own small kernel density estimator

# Write a function for the Gaussian kernel

kernelfunction <- function(x) {
  exp(-x^2 / 2) / sqrt(2 * pi)
}

# Generate data set with kernel 

data_kernel <- tibble(
  x = seq(-10, 10, length.out = 1000), # range of x variable
  k = kernelfunction(x)  # kernel function applied to x variable
)

# Plot data point only (x=0) 

p1 <- ggplot() +
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
ggsave(filename="03-output/gr_session03_01_kdensity_ex1.pdf",p1)

# Plot Gaussian Kernel around data point

p2 <- ggplot(data_kernel, aes(x = x, y = k)) +
  geom_line(linewidth = 1.1) +
  geom_point(data = tibble(x = 0, k = 0), aes(x = x, y = k),
             shape = 21, size = 3, fill = "black") +
  labs(
    title = "Kernel Function",
    x = "x",
    y = "Kernel value"
  ) +
  theme_minimal(base_size = 14)
ggsave(filename="03-output/gr_session03_02_kdensity_ex2.pdf",p2)

# Shift Kernel centered at a single data point

x1 <- 2      # single data point
h1  <- 1      # bandwidth for plotting shift/scale
h2  <- 2      # alternative bandwidth for plotting shift/scale

data_kernel <- data_kernel %>% mutate(
  k_shifted_h1 = (1 / h1) * kernelfunction((x - x1) / h1),
  k_shifted_h2 = (1 / h2) * kernelfunction((x - x1) / h2)
)

p3 <- ggplot(data_kernel, aes(x = x, y = k_shifted_h1)) +
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
ggsave(filename="03-output/gr_session03_03_kdensity_ex3a.pdf",p3)

p4 <- p3 + 
geom_line(aes(y = k_shifted_h2), linewidth = 1.1, color = "darkgreen") 
ggsave(filename="03-output/gr_session03_04_kdensity_ex3b.pdf",p4)

# Kernel Density Estimator (KDE) for two data points

x_points <- c(-1, 2)   # two data points
n <- length(x_points)
h_kde <- 1             # bandwidth

# Long format, transform data set to long

data_kernel_long <- data_kernel %>%
  select(x) %>%
  crossing(i = factor(seq_along(x_points))) %>%
  mutate(
    x_i = x_points[as.integer(i)],
    k_i = (1 / h_kde) * kernelfunction((x - x_i) / h_kde)
  )

# Summed KDE: average of the two kernels

data_kde <- data_kernel_long %>%
  group_by(x) %>%
  summarise(f_hat = mean(k_i), .groups = "drop")

p5 <- ggplot() +
  geom_line(
    data = data_kernel_long,
    aes(x = x, y = k_i, colour = i),
    linewidth = 0.9
  ) +
  geom_line(
    data = data_kde,
    aes(x = x, y = f_hat),
    linewidth = 1.3,
    colour = "black"
  ) +
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
ggsave(filename = "03-output/gr_session03_05_kdensity_ex4.pdf", p5)

rm(kernelfunction,data_kde,data_kernel,data_kernel_long,h_kde,h1,h2,n,p1,p2,p3,p4,p5,x_points,x1)

# 02 Kernel Density of Income Distribution -------------------------------------------------------------------------

# We use density() for reliability and speed; hand-built KDE was for intuition only.

# Load data produced in session 01. 

load(file="04-data/processed/soep.RData")

# Focus on individuals with earnings larger than 0 

soep <- soep %>% filter(syear==2019 & income_tot_y>0)

# Plot histogram of incomes

p_6 <- ggplot(soep, aes(x = income_tot_y)) +
  geom_histogram(bins = 50, fill = "grey80", color = "white") +
  scale_x_continuous(labels = scales::comma, limits = c(0, 250000)) +
  labs(
    title = "Histogram of Income",
    x = "Income",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)
ggsave("03-output/gr_session03_06_income_hist.pdf", p_6)

# Generate income vector 

income <- soep$income_tot_y

## a.) Fixed bandwidth (Silverman) -------------------------------------------------------------------------

# Estimated density, use Silverman's rule of thumb (nrd0) for bandwidth selection

den_silverman <- density(income, bw = "nrd0", kernel = "gaussian",from=0,to=5000000,n=10001)

h0 <- den_silverman$bw # bandwidth used by kde 
income_grid <- den_silverman$x # income grid used for kde

## b.) Variable bandwidth -------------------------------------------------------------------------

# Use pilot density at observations, calculated with Silverman
# approx returns a list of points which linearly interpolate given data points.
# rule = 2 specifies that outside the interval, the closest value is used 

pdf <- approx(den_silverman$x, den_silverman$y, income, rule = 2)$y

# Geometric mean of observed densities 

mean_pdf <- exp(mean(log(pdf)))

# Bandwidth used at particular point = lambda * h0
# Lower density at point implies higher bandwidth (smoother)
# Higher density at point implies lower bandwidth (more precision possible)

lambda <- (mean_pdf / pdf)^0.5
bvar <- lambda*h0 

# Adaptive bandwidth
# Apply a function to values z of the income grid. 
# Dnorm is a Gaussian Kernel function (normal distribution)

pdf_adapt <- sapply(income_grid, function(z) {
  mean(dnorm((z - income) / (bvar)) / (bvar))
})

# Data set with income grid, pdf and cdf, 
# Make sure that pdf is normalized such that integ f(y)dy=1
# Trapz is numerical (trapezoidal) integration

density_estimate <- tibble(
  income = den_silverman$x,
  pdf = den_silverman$y/(trapz(den_silverman$x,den_silverman$y)),
  pdf_adapt = pdf_adapt/(trapz(income,pdf_adapt))
)

# CDF computed through numerical integration (cumulative trapezoidal integration)

density_estimate <- density_estimate %>%
  mutate(
    cdf = cumtrapz(income,pdf),
    cdf_adapt = cumtrapz(income,pdf_adapt)
  )

# Plot histogram and density 

p7 <- ggplot() +
  geom_histogram(aes(x = income, y = after_stat(density)), bins = 50, fill = "grey80", color = "white") +
  geom_line(data = density_estimate, aes(x = income, y = pdf, color = "Fixed KDE"), linewidth = 1.1) +
  geom_line(data = density_estimate, aes(x = income, y = pdf_adapt, color = "Adaptive KDE"), linewidth = 1.1) +
  scale_x_continuous(labels = scales::comma, limits = c(0, 250000)) +
  scale_color_manual(
    name = "",
    values = c("Fixed KDE" = "blue", "Adaptive KDE" = "darkgreen")
  ) +
  labs(
    title = "Income Histogram & Kernel Density Estimate",
    x = "Income",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
ggsave("03-output/gr_session03_07_income_hist_density.pdf", p7)

p8 <- ggplot(density_estimate, aes(x = income)) +
  geom_line(aes(y = cdf, color = "Fixed KDE"), linewidth = 1.1) +
  geom_line(aes(y = cdf_adapt, color = "Adaptive KDE"), linewidth = 1.1) +
  scale_x_continuous(labels = scales::comma, limits = c(0, 250000)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  scale_color_manual(
    name = "",
    values = c("Fixed KDE" = "blue", "Adaptive KDE" = "darkgreen")
  ) +
  labs(
    title = "Income CDF: Kernel Density Estimate",
    x = "Income",
    y = "Cumulative density"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
ggsave("03-output/gr_session03_08_income_cdf.pdf", p8)

# Inverse Hazard Rate
# Use adaptive cdf only

density_estimate <- density_estimate %>%
  mutate(
    inv_hazard = (1-cdf)/pdf,
    inv_hazard_adapt = (1 - cdf_adapt) / pdf_adapt
  )

p9 <- ggplot(density_estimate, aes(x = income)) +
  geom_line(aes(y = inv_hazard, color = "Fixed KDE"), linewidth = 1.1) +
  geom_line(aes(y = inv_hazard_adapt, color = "Adaptive KDE"), linewidth = 1.1) +
  scale_x_continuous(labels = scales::comma, limits = c(0, 250000)) +
  scale_y_continuous(labels = scales::comma,limits=c(0,100000)) +
  scale_color_manual(
    name = "",
    values = c("Fixed KDE" = "blue", "Adaptive KDE" = "darkgreen")
  ) +
  labs(
    title = "Inverse Hazard Rate: Kernel Density Estimate",
    x = "Income",
    y = "Inverse hazard rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
ggsave("03-output/gr_session03_09_income_inv_hazard.pdf", p9)

# 03 Add Pareto Tail Function  -------------------------------------------------------------------------

# Here we append a Pareto tail to our non-parametric estimate of the income distribution

## a.) Define Function to add pareto tail -------------------------------------------------------------------------

# Function takes five input values (income, pdf, cdf, the Pareto parameter for the top,the income
# value where the distribution starts to transform towards Pareto, and the income level at which
# the distribution is fully Pareto)

pareto_add <- function(income,
                       pdf,
                       cdf,
                       a_final,
                       inc_start,
                       inc_const) {
  
  # Index (position) of inc_start and inc_const
  
  i_start <- which(income > inc_start)[1]
  i_const <- which(income > inc_const)[1]
  
  # Length of income vector 
  
  n <- length(income)
  
  # Get Pareto parameter at start from inverse hazard rate 
  
  a_start <- (pdf[i_start] / (1 - cdf[i_start])) * income[i_start]
  
  # Get a sequence of Pareto parameters from start of transformation to full pareto income level
  
  a_vec <- seq(a_start, a_final, length.out = i_const - i_start + 1)
  
  # Initialize at i_start using original pdf(i_start)
  
  a <- a_vec[1]
  
  # Use the information on the pdf, income start and a to get ymin
  
  y_min <- (((pdf[i_start]*income[i_start])/a)^(1/a))*income[i_start]
  
  # Based on ymin, get new pdf vector 
  
  pdf[i_start] <- a * (y_min/income[i_start])^a * (1/income[i_start])
  
  # Now go through the whole transition range of the Pareto parameter
  
  for (k in 2:length(a_vec)) {
    
    i <- i_start + k - 1
    
    # Get pdf using income at current point but previous ymin information
    
    pdf[i] <- a * (y_min/income[i])^a * (1/income[i])
    
    # Switch to new a and get new y_min so new Pareto matches pdf at this point
    
    a <- a_vec[k]
    y_min <- (((pdf[i]*income[i])/a)^(1/a))*income[i]
    
    # Get new pdf at this point, overwrite it
    
    pdf[i] <- a * (y_min/income[i])^a * (1/income[i]) 
  }
  
  # Upper tail with constant a_final and last y_min
  
  a <- a_final
  
  for (i in i_const:n) {
    pdf[i] <- a * (y_min/income[i])^a * (1/income[i]) 
  }
  
  # Adding Pareto tail has transformed the pdf such that it might not longer 
  # be that integ f(y) dy = 1. Therefore need to renormalize 
  
  # Integrate pdf over income and re-normalize pdf 
  
  integral <- trapz(income, pdf)
  pdf <- pdf/integral
  cdf <- cumtrapz(income,pdf)
  
  # Outputs returned by function
  
  list(
    pdf = pdf,
    cdf = cdf,
    a_start = a_start,
    a_vec = a_vec
  )
  
}

## b.) Add Pareto Tail to KDensity -------------------------------------------------------------------------

### Specify Parameters  -------------------------------------------------------------------------

# Pareto parameter (constant a at the top)

a_final <- 1.67 # based on our analysis in session-02.R

# Income levels between which we move from kernel density to Pareto distribution
# E.g. between top 3 percent and top 1 percent (from our tabulated information)

inc_start <- 125000  
inc_const <- 250000 

# NOTE: Adding Pareto tail is sensitive to grid resolution and transition range
# Small changes in inc_start / inc_const can affect results

### Apply Pareto tail via function  -------------------------------------------------------------------------

# We use results from adaptive kde

pareto_out <- pareto_add(
  income = density_estimate$income,
  pdf    = density_estimate$pdf_adapt,
  cdf    = density_estimate$cdf_adapt,
  a_final = a_final,
  inc_start     = inc_start,
  inc_const     = inc_const
)

# Put output of function into new data set 

data_opttax <- density_estimate %>%
  mutate(
    pdf_pareto = pareto_out$pdf,
    cdf_pareto = pareto_out$cdf,
    inv_hazard_pareto = (1-cdf_pareto)/pdf_pareto
  )

## c.) Plot Density and inverse hazard rate  -------------------------------------------------------------------------

p10 <- ggplot() +
  geom_histogram(aes(x = income, y = after_stat(density)), bins = 50, fill = "grey80", color = "white", inherit.aes = FALSE) +
  geom_line(data = data_opttax, aes(x = income, y = pdf, color = "Adaptive KDE"), linewidth = 1.1, linetype ="solid") +
  geom_line(data = data_opttax, aes(x = income, y = pdf_pareto, color = "Adaptive KDE + Pareto tail"), linewidth = 1.2, linetype="dashed",show.legend = TRUE) +
  geom_vline(aes(xintercept = inc_start), color = "grey40", show.legend = FALSE, linetype="dashed") +
  geom_vline(aes(xintercept = inc_const), color = "grey40", show.legend = FALSE, linetype="dashed") +
  scale_x_continuous(labels = scales::comma, limits = c(0, 250000)) +
  scale_color_manual(
    name = "",
    values = c("Adaptive KDE" = "blue", "Adaptive KDE + Pareto tail" = "black")
  ) +
  labs(
    title = "Kernel Density with Pareto Tail",
    x = "Income",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session03_10_income_pareto_tail.pdf", p10)

p11 <- ggplot() +
  geom_line(data=data_opttax,aes(x=income,y=inv_hazard_pareto, color = "Adaptive KDE + Pareto tail"), linewidth = 1.1, linetype = "dashed")+
  geom_line(data=data_opttax,aes(x=income,y = inv_hazard_adapt, color = "Adaptive KDE"), linewidth = 1.1) +
  scale_y_continuous(labels = scales::comma,limits=c(0,200000)) +
  labs(
    title = "Inverse Hazard Rate: Kernel Density Estimate",
    x = "Income",
    y = "Inverse hazard rate"
  ) +
  geom_vline(aes(xintercept = inc_start), color = "grey40", show.legend = FALSE, linetype="dashed") +
  geom_vline(aes(xintercept = inc_const), color = "grey40", show.legend = FALSE, linetype="dashed") +
  scale_x_continuous(labels = scales::comma, limits = c(0, 250000)) +
  scale_color_manual(
    name = "",
    values = c("Adaptive KDE" = "blue", "Adaptive KDE + Pareto tail" = "black")
  ) +
  labs(
    title = "Inverse Hazard Rate with Pareto Tail",
    x = "Income",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave("03-output/gr_session03_11_income_inv_hazard.pdf", p11)

# 04 Shape of optimal tax rates -------------------------------------------------------------------------

## a.) Status quo tax system -------------------------------------------------------------------------

### Define function based on tax code -------------------------------------------------------------------------

# We go to the tax code of 2019, e.g. through buzer (https://www.buzer.de/gesetz/4499/al68661-0.htm) and 
# write a function replicating the tax code of 2019.

# Note: We work here with statutory tax rates only. In Session 5/6 we will make this more realistic.

# One option for function that calculates the tax liability and MTR for a vector input 

tax2019 <- function(x) {
  
  # 1. Below 9,169
  
  tax <- numeric(length(x)) # set up a vector that is of similar length like the input (income vector)
  tax[x < 9169] <- 0 # For all elements of the tax vector for which elements of input vector smaller than 9169, set elements of tax to zero.
  
  mtr <- numeric(length(x))
  mtr[x < 9169] <- 0
  
  # 2. From 9,169 to 14,254
  
  y <- (x[x >= 9169 & x <= 14254] - 9168) / 10000
  tax[x >= 9169 & x <= 14254] <- (980.14 * y + 1400) * y
  mtr[x >= 9169 & x <= 14254] <- (2 * 980.14 * y + 1400) / 10000

  # 3. From 14,255 to 55,960
  
  z <- (x[x >= 14255 & x <= 55960] - 14254) / 10000
  tax[x >= 14255 & x <= 55960] <- (216.16 * z + 2397) * z + 965.58
  mtr[x >= 14255 & x <= 55960] <- (2 * 216.16 * z + 2397) / 10000
  
  # 4. From 55,961 to 265,326
  
  tax[x >= 55961 & x <= 265326] <- 0.42 * x[x >= 55961 & x <= 265326] - 8780.90
  mtr[x >= 55961 & x <= 265326] <- 0.42
  
  # 5. From 265,327
  
  tax[x > 265326] <- 0.45 * x[x > 265326] - 16740.68
  mtr[x > 265326] <- 0.45
  
  # Return output 
  
  list(
    tax = tax,
    mtr = mtr
  )
  
}

tax <- tax2019(data_opttax$income)
taxliab <- tax$tax
mtr <- tax$mtr

### Apply function to data  -------------------------------------------------------------------------

data_opttax <- data_opttax %>%
  mutate(
    taxorig=taxliab,
    mtrorig=mtr,
    net_income=income-taxorig
)

### Approximate tax liability by HSV  -------------------------------------------------------------------------

# The actual status quo tax liability has kinks, i.e. FOC does not define local optimum. 
# For simplicity, we approximate the tax liability by a tax function with constant rate of 
# progressivity following Heathcote, Storesletten and Violante (HSV).

# For the regression, to estimate HSV in reduced-form, focus on positive incomes only 

data_opttax_reg <- data_opttax %>%
  filter(
    net_income>0
  )

reg <- lm(log(net_income) ~ log(income), data = data_opttax_reg)
summary(reg)

beta  <- coef(reg)["log(income)"]     # = 1 - tau
alpha <- coef(reg)["(Intercept)"]     # = log(lambda)

# Regression coefficients identify HSV parameters 

tau    <- 1 - beta
lambda <- exp(alpha)

# Now use parameters and estimate HSV tax function

data_opttax <- data_opttax %>%
  mutate(
    tax_HSV =  income - lambda * income^(1 - tau),
    mtr_HSV = 1 - lambda * (1 - tau) * income^(-tau)
  )

### Plot tax liability and mtr -------------------------------------------------------------------------

# Plot statutory tax liability

p12 <- ggplot(data_opttax, aes(x = income)) +
  geom_line(aes(y = taxorig, color = "Statutory liability"), linewidth = 1.1) +
  geom_line(aes(y = tax_HSV, color = "HSV fit"), linewidth = 1.1, linetype = "dashed") +
  scale_x_continuous(labels = scales::comma, limits = c(0, 400000)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 150000)) +
  scale_color_manual(
    name = "",
    values = c("Statutory liability" = "black", "HSV fit" = "firebrick"),
    breaks = c("Statutory liability", "HSV fit")
  ) +
  labs(
    title = "Statutory Tax Liability (2019)",
    x = "Income",
    y = "Tax liability"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
ggsave("03-output/gr_session03_12_tax_liability.pdf", p12)

# Plot statutory marginal tax rate 

p13a <- ggplot(data_opttax, aes(x = income)) +
  geom_line(aes(y = mtrorig, color = "Statutory MTR"), linewidth = 1.1) +
  scale_x_continuous(labels = scales::comma, limits = c(0, 400000)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) +
  scale_color_manual(
    name = "",
    values = c("Statutory MTR" = "black", "HSV fit" = "firebrick"),
    breaks = c("Statutory MTR", "HSV fit")
  ) +
  labs(
    title = "Statutory Marginal Tax Rate (2019)",
    x = "Income",
    y = "Marginal tax rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
ggsave("03-output/gr_session03_13a_mtr.pdf", p13a)

# Plot statutory marginal tax rate with HSV approx.

p13b <- ggplot(data_opttax, aes(x = income)) +
  geom_line(aes(y = mtrorig, color = "Statutory MTR"), linewidth = 1.1) +
  geom_line(aes(y = mtr_HSV, color = "HSV fit"), linewidth = 1.1, linetype = "dashed") +
  scale_x_continuous(labels = scales::comma, limits = c(0, 400000)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.6)) +
  scale_color_manual(
    name = "",
    values = c("Statutory MTR" = "black", "HSV fit" = "firebrick"),
    breaks = c("Statutory MTR", "HSV fit")
  ) +
  labs(
    title = "Statutory Marginal Tax Rate (2019)",
    x = "Income",
    y = "Marginal tax rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
ggsave("03-output/gr_session03_13b_mtr.pdf", p13b)


## b.) Optimal tax system  -------------------------------------------------------------------------

# Write function that gives skill/ability based on inverted FOC

omega <- function(income,mtr_statusquo,elasticity) {
  
  # Invert FOC 
  
  omega = ((income^(1/elasticity))/(1-mtr_statusquo))^(1/(1+(1/elasticity)))
  
  # Return skill/ability level 
  
  omega
}

# Write function that gives optimal tax system as function of 
# income vector, status quo mtr, and elasticity

opt_tax <- function(income,cdf,pdf,mtr_statusquo,elasticity) {
  
  # Get skill level by inverted first order condition
  
  omega = omega(income,mtr_statusquo,elasticity)
  
  # Numerical difference in skill level 
  
  d_omega <- omega-lag(omega)
  d_income <- income-lag(income)
  
  # Tprime/(1-Tprime)
  
  tprime_ratio = ((1-cdf)/pdf) * ((d_omega/d_income) / (omega)) * (1/elasticity)
  
  # tprime 
  
  tprime = tprime_ratio/(1+tprime_ratio)
  
  # Return tprime
  
  tprime
  
}


# Apply tax function to multiple elasticity values

data_opttax <- data_opttax %>%
  mutate(
    omega_e33 = omega(income, mtr_HSV, 0.33),
    omega_e50  = omega(income, mtr_HSV, 0.5),
    omega_e75 = omega(income, mtr_HSV, 0.75),
    omega_e100   = omega(income, mtr_HSV, 1),
    tprime_e33 = opt_tax(income, cdf_pareto, pdf_pareto, mtr_HSV, 0.33),
    tprime_e50  = opt_tax(income, cdf_pareto, pdf_pareto, mtr_HSV, 0.5),
    tprime_e75 = opt_tax(income, cdf_pareto, pdf_pareto, mtr_HSV, 0.75),
    tprime_e100   = opt_tax(income, cdf_pareto, pdf_pareto, mtr_HSV, 1),
  )

# Reshape data to long to have different rows for different 
# elasticity values 

data_opttax_long <- data_opttax %>%
  pivot_longer(
    cols = starts_with("tprime_e"),
    names_to = "elasticity",
    names_prefix = "tprime_e",
    values_to = "tprime"
  ) %>%
  mutate(
    elasticity = as.numeric(str_replace(elasticity, "_", "."))/100
  ) %>%
  mutate(
    omega = case_when(
      elasticity == 0.33 ~ omega_e33,
      elasticity == 0.5  ~ omega_e50,
      elasticity == 0.75 ~ omega_e75,
      elasticity == 1    ~ omega_e100
    )
  ) %>%
  select(-omega_e33, -omega_e50, -omega_e75, -omega_e100)

p14 <- ggplot() +
  geom_line(data = data_opttax_long, aes(x = income, y = tprime, color = factor(elasticity)), linewidth = 1) +
  geom_line(data = data_opttax, aes(x = income, y = mtrorig, color = "Status quo"), linewidth = 1.1, linetype = "dashed") +
  scale_x_continuous(labels = scales::comma, limits = c(0, 400000)) +
  scale_color_manual(
    name = "",
    values = c(
      setNames(viridisLite::viridis(length(unique(data_opttax_long$elasticity))), as.character(unique(data_opttax_long$elasticity))),
      "Status quo" = "black"
    ),
    breaks = c(as.character(sort(unique(data_opttax_long$elasticity))), "Status quo"),
    labels = c(scales::number_format(accuracy = 0.01)(sort(unique(data_opttax_long$elasticity))), "Status quo")
  ) +
  labs(
    title = "Optimal MTR by Elasticity",
    x = "Income",
    y = "Optimal MTR"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")+
  scale_y_continuous(limits = c(0,1))
ggsave("03-output/gr_session03_14_optimal_mtr_multi.pdf", p14)

# 05 Export data for next session -------------------------------------------------------------------------

save(data_opttax_long,file="04-data/processed/data_opttax_long.RData")