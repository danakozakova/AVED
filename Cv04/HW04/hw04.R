# Homework: function, control structures, and iteration
# =====================================================

# Goal: create functions and use iteration

# clear workspace
rm(list = ls())

# load necessary libraries
library(tibble)
library(purrr)

# load the data needed
load("hw04.RData")

# 1. create function alpha(pstar, p)
alpha <- function(pstar, p) {
  # kladny numericky skalar pstar
  # neprazdny vektor kladnych numerickych cisel p spocita
  if ((!is.numeric(pstar)) # numericky pstar
      || (length(pstar) != 1) # skalar pstar
      || (pstar <= 0) # kladny pstar
      || (length(p) < 1) # neprazdny p
      || (!is.numeric(p)) # numericky p
      || (min(p) <= 0)) # kladne cisla v p
    stop("Bad input")
  100 * sqrt(sum((p - pstar)^2 / length(p))) / pstar
}
save(alpha, file = "results.RData")

# 2. create function alpha_df(df, id)
alpha_df <- function(df, id) {
  filtered <- df$id == id
  alpha(df$pstar[filtered][1], df$p[filtered])
}
save(alpha, alpha_df, file = "results.RData")

# 3. create vector ids
ids <- experiment$id |> unique()
save(alpha, alpha_df, ids, file = "results.RData")

# 4. create vector alphas
alphas <- map_dbl(ids, ~alpha_df(experiment, .)) 
save(alpha, alpha_df, ids, alphas, file = "results.RData")

# 5. create tibble outcomes
outcomes <- tibble(ids, alphas)
names(outcomes) <- c('id', 'alpha')
save(alpha, alpha_df, ids, alphas, outcomes, file = "results.RData")
