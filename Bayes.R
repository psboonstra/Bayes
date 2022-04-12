## ----setup, include=FALSE-----------------------------------------------------------
library(tidyverse); 
library(broom);
knitr::opts_chunk$set(echo = T, warning = F, message = F);
knitr::knit_hooks$set(mysize = function(before, options, envir) {
  if (before) 
    return(options$size);
})
options(digits = 4);
figure_scaler = 5/8;#1/2 for ioslides; ~1/3 for word, pdf
text_scaler = 1;#1 for ioslides; 2/3 for word, pdf
fig.x = 16 * figure_scaler;
fig.y = 9 * figure_scaler;
cex_scale = 1.75 * figure_scaler;
theme_set(theme_bw())



## ---- echo = T----------------------------------------------------------------------
pbinom(q = 13, size = 20, prob = 0.5, lower = F);
source("invert_pvalues.R")
invert_pbinom_leftsided(q = 13, size = 20, conf_level = 0.95)$conf_int


## -----------------------------------------------------------------------------------
pnbinom(q = 13, size = 6, prob = 0.5, lower = F);
invert_pnbinom_leftsided(q = 13, size = 6, conf_level = 0.95)$conf_int


## -----------------------------------------------------------------------------------
# a=b=0.5
qbeta(0.05, 14.5, 6.5)
# a=b=4
qbeta(0.05, 18, 10)


## -----------------------------------------------------------------------------------
require(rstanarm)
response_data <- 
  data.frame(y = c(rep(1, 14), rep(0, 6)), 
             x = rep(1, 20))

bayes_prop_cauchy <- 
  stan_glm(data = response_data, 
           # drop the the intercept from logistic regression
           # since x is already constant term
           formula = y ~ -1 + x, 
           prior = cauchy(scale = 2.5),
           family = "binomial",
           refresh = 1000)
bayes_prop_cauchy_betas <- rstan::extract(bayes_prop_cauchy$stanfit)[["beta"]][,1]
quantile(1 / (1 + exp(-bayes_prop_cauchy_betas)), p = c(0.05, 0.5))
ggplot(data = data.frame(gamma = 1 / (1 + exp(-bayes_prop_cauchy_betas)))) + 
  geom_histogram(aes(x = gamma), 
                 bins = 50) + 
  geom_vline(aes(xintercept = quantile(gamma, 0.05))) + 
  scale_x_continuous(name = expression(gamma), 
                     breaks = seq(0, 1, by = 0.1)) + 
  scale_y_continuous(name = NULL) + 
  theme(text = element_text(size = 24))


## -----------------------------------------------------------------------------------

bayes_prop_laplace <- 
  stan_glm(data = response_data, 
           # drop the the intercept from logistic regression
           # since x is already constant term
           formula = y ~ -1 + x, 
           prior = laplace(scale = 2.5),
           family = "binomial",
           refresh = 1000)
bayes_prop_laplace_betas <- rstan::extract(bayes_prop_laplace$stanfit)[["beta"]][,1]
quantile(1 / (1 + exp(-bayes_prop_laplace_betas)), p = c(0.05, 0.5))
ggplot(data = data.frame(gamma = 1 / (1 + exp(-bayes_prop_laplace_betas)))) + 
  geom_histogram(aes(x = gamma), 
                 bins = 50) + 
  geom_vline(aes(xintercept = quantile(gamma, 0.05))) + 
  scale_x_continuous(name = expression(gamma), 
                     breaks = seq(0, 1, by = 0.1)) + 
  scale_y_continuous(name = NULL) + 
  theme(text = element_text(size = 24))


## -----------------------------------------------------------------------------------

bayes_prop_laplace <- 
  stan_glm(data = response_data, 
           # drop the the intercept from logistic regression
           # since x is already constant term
           formula = y ~ -1 + x, 
           prior = laplace(scale = 0.05),
           family = "binomial",
           refresh = 1000)
bayes_prop_laplace_betas <- rstan::extract(bayes_prop_laplace$stanfit)[["beta"]][,1]
quantile(1 / (1 + exp(-bayes_prop_laplace_betas)), p = c(0.05, 0.5))
ggplot(data = data.frame(gamma = 1 / (1 + exp(-bayes_prop_laplace_betas)))) + 
  geom_histogram(aes(x = gamma), 
                 bins = 50) + 
  geom_vline(aes(xintercept = quantile(gamma, 0.05))) + 
  scale_x_continuous(name = expression(gamma), 
                     breaks = seq(0.02, 1, by = 0.04)) + 
  scale_y_continuous(name = NULL) + 
  theme(text = element_text(size = 24))


## -----------------------------------------------------------------------------------
(14/20) - qnorm(0.95) * sqrt((14/20) * (6/20) / 20)


## -----------------------------------------------------------------------------------
# Exact when binomial sampling scheme
invert_pbinom_leftsided(q = 13, size = 20, conf_level = 0.95)$conf_int


## -----------------------------------------------------------------------------------
# Exact when negative binomial sampling scheme
invert_pnbinom_leftsided(q = 13, size = 6, conf_level = 0.95)$conf_int


## -----------------------------------------------------------------------------------
# Bayes: a=b=0.5
qbeta(0.05, 14.5, 6.5)
# Bayes: a=b=4
qbeta(0.05, 18, 10)

