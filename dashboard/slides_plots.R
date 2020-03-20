# Generate slides plots
# preamble ####
library(tidyverse)
library(ggplot2)
ggplot2::theme_set(theme_classic())
library(patchwork)

# initial params
mu0 <- 0
mu1 <- 1
sigma2 <- 2
n <- 10
t1e <- 0.05

# create params
sd0 <- sqrt(sigma2 / n)

xmin0 <- mu0 - 3.5 * sd0
xmin1 <- mu1 - 3.5 * sd0
xmin <- min(xmin0, xmin1)

xmax0 <- mu0 + 3.5 * sd0
xmax1 <- mu1 + 3.5 * sd0
xmax <- max(xmax0, xmax1)

ymax0 <- dnorm(mu0, mean = mu0, sd = sd0)
ymax1 <- dnorm(mu1, mean = mu1, sd = sd0)
ymax <- max(ymax0, ymax1)

cv0 <- qnorm(1-t1e, mean = mu0, sd = sd0)

# H0 plot ####
h0 <- ggplot(tibble(x = c(xmin, xmax)), aes(x)) +
  stat_function(fun = dnorm, 
                args = list(mean = mu0, sd = sd0)) + #adds normal plot
  geom_segment(aes(x = cv0, y = 0, 
                   xend = cv0, yend = 1.15*ymax0),
               linetype="dashed",
               size=1) + # adds line on critical value
  stat_function(fun = dnorm, 
                args = list(mean = mu0, sd = sd0),
                xlim = c(xmin, cv0),
                geom = "area",
                fill = "#55C667FF") + # fill up specificity
  stat_function(fun = dnorm, 
                args = list(mean = mu0, sd = sd0),
                xlim = c(cv0, xmax),
                geom = "area",
                fill = "#D64B40FF") + # fill up type I error +
  labs(x = "",
       y = "") +
  xlim(c(xmin0, xmax0)) +
  theme(axis.text = element_text(size = 14))
ggsave("../img/1. significance.png", plot = h0)


# H1 plot ####
h1 <- ggplot(tibble(x = c(xmin, xmax)), aes(x)) +
  stat_function(fun = dnorm, 
                args = list(mean = mu1, sd = sd0)) + #adds normal plot
  geom_segment(aes(x = cv0, y = 0, 
                   xend = cv0, yend = 1.15*ymax1),
               linetype="dashed",
               size=1) + # adds line on critical value
  stat_function(fun = dnorm, 
                args = list(mean = mu1, sd = sd0),
                xlim = c(xmin, cv0),
                geom = "area",
                fill = "#FCA007FF") + # fill up  type II error
  stat_function(fun = dnorm, 
                args = list(mean = mu1, sd = sd0),
                xlim = c(cv0, xmax),
                geom = "area",
                fill = "#39558CFF",
                alpha = 0.75) + # fill up power +
  labs(x = "",
       y = "") +
  xlim(c(xmin1, xmax1)) +
  theme(axis.text = element_text(size = 14))
ggsave("../img/2. power.png", plot = h1)



# Together ####
# initial params
mu0 <- 0
mu1 <- 1
sigma2 <- 2
n <- 15
t1e <- 0.05

# create params
sd0 <- sqrt(sigma2 / n)

xmin0 <- mu0 - 3.5 * sd0
xmin1 <- mu1 - 3.5 * sd0
xmin <- min(xmin0, xmin1)

xmax0 <- mu0 + 3.5 * sd0
xmax1 <- mu1 + 3.5 * sd0
xmax <- max(xmax0, xmax1)

ymax0 <- dnorm(mu0, mean = mu0, sd = sd0)
ymax1 <- dnorm(mu1, mean = mu1, sd = sd0)
ymax <- max(ymax0, ymax1)

cv0 <- qnorm(1-t1e, mean = mu0, sd = sd0)

# together
htogether <- ggplot(tibble(x = c(xmin, xmax)), aes(x)) +
  stat_function(fun = dnorm, 
                args = list(mean = mu0, sd = sd0)) + #adds normal plot
  stat_function(fun = dnorm, 
                args = list(mean = mu1, sd = sd0)) + #adds normal plot
  geom_segment(aes(x = cv0, y = 0, 
                   xend = cv0, yend = 1.15*ymax0),
               linetype="dashed",
               size=1) + # adds line on critical value
  stat_function(fun = dnorm, 
                args = list(mean = mu0, sd = sd0),
                xlim = c(xmin, cv0),
                geom = "area",
                fill = "#55C667FF",
                alpha = 0.5) + # fill up specificity
  stat_function(fun = dnorm, 
                args = list(mean = mu0, sd = sd0),
                xlim = c(cv0, xmax),
                geom = "area",
                fill = "#D64B40FF",
                alpha = 0.75) + # fill up type I error
  stat_function(fun = dnorm, 
                args = list(mean = mu1, sd = sd0),
                xlim = c(xmin, cv0),
                geom = "area",
                fill = "#FCA007FF",
                alpha = 0.5) + # fill up  type II error
  stat_function(fun = dnorm, 
                args = list(mean = mu1, sd = sd0),
                xlim = c(cv0, xmax),
                geom = "area",
                fill = "#39558CFF",
                alpha = 0.5,
                alpha = 0.75) +
  labs(x = "",
       y = "") +
  xlim(c(xmin, xmax)) +
  theme(axis.text = element_text(size = 14))

ggsave("../img/3. together.png", plot = htogether,
       height = 5.1, width = 12)



# Power algorithm ####
# initial params
mu0 <- 0
mu1 <- 0.7
sigma2 <- 1
t1e <- 0.05

xmin <- -0.5
xmax <- 1.5
ymax <- 1.5

for(n in 10:13){
  
  # create params
  sd0 <- sqrt(sigma2 / n)
  cv0 <- qnorm(1-t1e, mean = mu0, sd = sd0)
  
  h1 <- ggplot(tibble(x = c(xmin, xmax)), aes(x)) +
    stat_function(fun = dnorm, 
                  args = list(mean = mu1, sd = sd0)) + #adds normal plot
    geom_segment(aes(x = cv0, y = 0, 
                     xend = cv0, yend = 1.15*ymax),
                 linetype="dashed",
                 size=1) + # adds line on critical value
    stat_function(fun = dnorm, 
                  args = list(mean = mu1, sd = sd0),
                  xlim = c(xmin, cv0),
                  geom = "area",
                  fill = "#FCA007FF") + # fill up  type II error
    stat_function(fun = dnorm, 
                  args = list(mean = mu1, sd = sd0),
                  xlim = c(cv0, xmax),
                  geom = "area",
                  fill = "#39558CFF",
                  alpha = 0.75) + # fill up power +
    labs(x = "",
         y = "") +
    xlim(c(xmin, xmax)) +
    theme(axis.text = element_text(size = 14))
  
  filename <- paste0("../img/power_alg/", n-9, ".png")
  ggsave(filename = filename, plot = h1)
}
