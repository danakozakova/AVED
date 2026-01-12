# vzorove riesenie testu
library(tidyverse)

lits <- readr::read_rds("zk_lits3.rds")

lits %>% colnames()

# 1. chybajuce hodnoty
# kod pre chybajucu hodnotu v premennej general_trust nahradit NA
lits %>%
  mutate(
    general_trust = ifelse(general_trust = -99, NA, general_trust)
  )

# lepsie
lits %>%
  mutate(
    general_trust = as.integer(general_trust),
    general_trust = ifelse(general_trust = -99, NA, general_trust)
  )

# alebo
lits %>%
  mutate(
    general_trust = na_if(general_trust, -99)
  )

# chybajuce premenne v stlpci food (NA) nahradit priemerom pre danu krajinu

lits %>%
  group_by(country) %>%
  mutate(
    food = ifelse(is.na(food), mean(food, na.rm = TRUE), food)
  ) %>%
  ungroup()

# cele riesenie
lits <- lits %>%
  mutate(
    general_trust = na_if(general_trust, -99)
  )
  group_by(country) %>%
  mutate(
    food = ifelse(is.na(food), mean(food, na.rm = TRUE), food)
  ) %>%
  ungroup()

# 2. Exploratory data statistics
# nova tabulka, ktora bude pre vsetky premenne okrem ID a country
# dummy premenne
lits$house %>% unique()
  
lits %>%
  mutate(
    house_owner = house == "owner",
    house_renter = house == "renter",
    house_other = house == "other"
  ) %>%
  select(-c(ID, house, country)) # alebo select(-ID:-house)


lits %>%
  mutate(
    house_owner = house == "owner",
    house_renter = house == "renter",
    house_other = house == "other"
  ) %>%
  select(-ID:-house) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    qdf = IQR(value, na.rm = TRUE)
  )

# alebo, no treba vyriesit NA
map_dbl(lits, mean)
map_dbl(lits, sd)
map_dbl(lits, IQR)

# 3. pre 4 nahodne zeme zobrazit vztah premennych education a general_trust
# nový tibble
selected <- lits$country %>% unique() %>% sample(4)
lits %>% 
  filter(country %in% selected)

# alebo
lits %>%
  distinct(country) %>%
  slice_sample(n = 4) %>% # nahodne vyberiem 4 riadky
  semi_join(lits, .) # zachovaj mi iba riadky z vybranych country

lits %>% 
  filter(country %in% selected) %>%
  ggplot(
    aes(x = education, y = general_trust)
  ) + 
  geom_point() # geom_point / scatter plot nefunguje

lits %>% 
  filter(country %in% selected) %>%
  ggplot(
    aes(x = education, y = general_trust)
  ) + 
  geom_jitter(
    alpha = 0.2,
  )

lits %>% 
  filter(country %in% selected) %>%
  ggplot(
    aes(x = education, y = general_trust, color = country)
  ) + 
  geom_jitter(
    alpha = 0.2,
  )

# facetovani
lits %>% 
  filter(country %in% selected) %>%
  ggplot(
    aes(x = education, y = general_trust, color = country)
  ) + 
  geom_jitter(
    alpha = 0.2,
  ) +
  facet_wrap(~country)

# moznosti: dvojrozmerny histogram alebo velkost bodu podla pocti hodnot v tej kombinacii 5x7
lits %>% 
  filter(country %in% selected) %>%
  group_by(education, general_trust, country) %>%
  summarise(
    obs = n()
  ) %>%
  ggplot(
    aes(x = education, y = general_trust, size = obs, color = country)
  ) + 
  geom_point() +
  facet_wrap(~country)

lits %>% 
  filter(country %in% selected) %>%
  group_by(education, general_trust, country) %>%
  summarise(
    obs = n()
  ) %>%
  ggplot(
    aes(x = education, y = general_trust, size = obs)
  ) + 
  geom_point() +
  facet_wrap(~country)

lits %>% 
  filter(country %in% selected) %>%
  group_by(education, general_trust, country) %>%
  summarise(
    obs = n()
  ) %>%
  ggplot(
    aes(x = education, y = general_trust, fill = obs)
  ) + 
  geom_tile() +
  facet_wrap(~country)

lits %>% 
  filter(country %in% selected) %>%
  group_by(education, general_trust, country) %>%
  summarise(
    obs = n()
  ) %>%
  ggplot(
    aes(x = education, y = general_trust, fill = obs)
  ) + 
  geom_tile() +
  scale_fill_distiller(
    direction = 1
  ) +
  facet_wrap(~country)

# hodnoty food vo vlastnej mene
lits %>%
  distinct(country) %>%
  slice_sample(n = 4) %>%
  semi_join(lits, .) %>%
  group_by(country) %>%
  mutate(
    food = scale(food) 
  ) %>%
  ggplot(
    aes(x = food, fill = country)
    ) +
  geom_density(
    alpha = 0.4
  ) 

lits %>%
  distinct(country) %>%
  slice_sample(n = 4) %>%
  semi_join(lits, .) %>%
  group_by(country) %>%
  mutate(
    food = scale(food) 
  ) %>%
  ggplot(
    aes(y = food, x = country)
  ) +
  boxplot()

# Ukol 4 odhadnete dva modely

library(fixest)
lits %>%
  mutate(
    house = house %>% as.factor() %>% relevel(ref = "renter")
    # alebo house = factor(house, levels = c("renter", "owner", "other"))
  )

lits %>%
  mutate(
    house = house %>% as.factor() %>% relevel(ref = "renter")
  ) %>%
  feols(
    vote_local_el ~ net_access + sw0(house),
    data = .
  )

# vhodnejsie este etable
lits %>%
  mutate(
    house = house %>% as.factor() %>% relevel(ref = "renter")
  ) %>%
  feols( # namiesto feols sa da pouzit aj lm()
    vote_local_el ~ net_access + sw0(house), # sw0 hovori, ze raz to odhadni bez toho, raz s tym
    data = .
  ) %>% 
  etable()
