# Homework: strings manipulation
# =====================================================

# Goal: work with strings

# clear workspace
rm(list = ls())

# 1. load necessary libraries
library(stringr)
library(tibble)

# 2. load the data ("hw06.txt"; encoding UTF8) into people
people <- readLines('hw06.txt')
save(people, file = "results.RData")

# 3. create df1
mat <- str_split_fixed(people, pattern = '\\|', n = 3) 
colnames(mat) <- c('name', 'height', 'weight')
df1 <- as_tibble(mat)

save(people, df1, file = "results.RData")

# 4. clean names in df2
df2 <- df1
df2$name <- df2$name |>
  str_trim() |> 
  str_remove_all("\"")

save(people, df1, df2, file = "results.RData")

# 5. change height and weight to numeric in df3
df3 <- df2
df3$height <- df3$height|> 
  str_extract("\\d+[.,]?\\d*") |> 
  str_replace(",", ".") |> 
  as.double()
df3$weight <- df3$weight |> 
  str_extract("\\d+[.,]?\\d*") |> 
  str_replace(",", ".") |> 
  as.double()

save(people, df1, df2, df3, file = "results.RData")

# 6. add bmi to df4
df4 <- df3
df4$bmi <- (df4$weight/((df4$height/100)^2)) |> round(1)

save(people, df1, df2, df3, df4, file = "results.RData")

# 7. add message to df4
df4$message <- str_c(df4$name, ' has BMI ', df4$bmi, '.')

save(people, df1, df2, df3, df4, file = "results.RData")
