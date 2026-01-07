# Homework: EDA (1)
# =====================================================

# Goal: Plot it!

library(tidyverse)

# clear workspace
rm(list = ls())
plot_data <- read_csv("hw10.csv")

# Your solution follows...

plot_data |> glimpse()
plot_data |> head()
plot_data |> summary()


plot_data <- plot_data %>%
  mutate(
    Sex = factor(Sex, levels = c(1, 2), labels = c("Men", "Women")),
    Year = factor(Year),
    People_sign = ifelse(Sex == "Men", -People, People)
  )


head(plot_data)

plot_data %>%
  ggplot(aes(x = Age, y = People_sign, fill = Sex)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Year) +
  labs(y = 'Number of people') +
  theme_bw()


# Save results
ggsave("results.pdf", width = 240, height = 160, units = "mm")
