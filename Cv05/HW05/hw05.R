# Homework: function, control structures, and iteration
# =====================================================

# Goal: read and write data from / to files

# clear workspace
rm(list = ls())

# 1. load necessary libraries
library(readr)

# 2. load the data
mpg <- read_delim("hw05.csv", 
                  delim = ":", 
                  escape_double = FALSE, 
                  col_types = cols(year = col_integer(),
                                   cyl = col_integer(),
                                   class = col_factor(levels = c("compact", "midsize", "suv", "2seater", "minivan", "pickup", "subcompact"))),
                  locale = locale(decimal_mark = ",", grouping_mark = "."), 
                  na = "--", trim_ws = TRUE)

# 3. calculate the correlation
mpg_cty_cor <- cor(mpg$displ, mpg$cty, use = "complete.obs")

# 4. save the data into results.RData
save(mpg, mpg_cty_cor, file = "results.RData")
