# for the ANSATZE app


library(dplyr)
library(ggformula)

az_omega_slider <- sliderInput("p1", "angular frequency 𝜔", min=0, max=2, step=0.01, value=1)
az_omega2_slider <- sliderInput("p2", "angular frequency 𝜔", min=0.3, max=2, step=0.01, value=1)
az_k_slider <- sliderInput("p1", "exponential parameter k", min=-1, max=1.2, step=0.01, value=0.25)
az_n_slider <- sliderInput("p1", "Power-law exponent n", min=-2, max=2, step=0.1, value=0.5)
az_center_slider <- sliderInput("p1", "Sigmoid center", min = 0, max = 10, step=0.1, value=5)
az_width_slider <- sliderInput("p2", "Sigmoid width", min = 0.1, max=5, step=0.1, value=2)

# set up the list of candidate ansatze
ansatz_candidates <- tibble::tribble(
  ~ fun, ~ control1, ~ control2,
  function(t, 𝜔, p2) sin(𝜔 * t), az_omega_slider, NA, # replace p1 and p2 with the actual names
  function(t, k, p2) exp(k * t), az_k_slider, NA,
  function(t, k, 𝜔) exp(k * t) * sin(𝜔 * t), az_k_slider, az_omega2_slider,
  function(t, center, width) sigmoid(t, center, width), az_center_slider, az_width_slider,
  function(t, n, p2) (t^n), az_n_slider, NA,
  function(t, n, 𝜔) (t^n) * sin(𝜔 * t), az_n_slider, az_omega2_slider,
) %>% mutate(display = unlist(sapply(fun, function(x) deparse(body(x)))),
             n = row_number())
ansatz_choices <- as.list(ansatz_candidates$n)
names(ansatz_choices) <- ansatz_candidates$display



sigmoid <- function(t, center, width) {
  pnorm(t, mean=center, sd = width)
}

#==============================


# For DE_Euler

dee_function_set <- tibble::tribble(
  ~ fun, ~ min, ~ max,
  function(x) -0.5*x, -10, 10,
  function(x) +0.5*x, -10, 10,
  function(x) -0.4*(x - 5), -2, 10,
  function(x) 2*x*(1-x/8), -2, 10,

) %>% mutate(display = unlist(sapply(fun, function(x) deparse(body(x)))),
             n = row_number())

dee_fun_choices <- as.list(dee_function_set$n)
names(dee_fun_choices) <- paste("∂𝗍 x =", dee_function_set$display)

# Shiny module for abcd selection widget
source("abcd-selector.R")
