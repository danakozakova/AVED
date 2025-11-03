# Chapter 1: Data Visualization - First Steps 
# install.packages("dplyr")
library(dplyr)

penguins |> glimpse()

# install.packages("ggplot2")
library(ggplot2)

# Prazdny graf
ggplot(data = penguins) # empty graph

# Definovane x a y
ggplot( # ako su premenne namapovane na vizualne premenne (aesthetic)
  data = penguins,
  mapping = aes(x = flipper_len, y = body_mass) # zatial len prazdny canvas
)

# Bodovy graf
ggplot(
  data = penguins,
  mapping = aes(x = flipper_len, y = body_mass)
) +
  geom_point() # zobrazit namapovane premenne - definovane cez geom

# Bodovy graf s kategoriami
ggplot(
  data = penguins,
  mapping = aes(x = flipper_len, y = body_mass, color = species) # definovane kategorie
) +
  geom_point() 

# Pridaná vrstva s linearnym trendom
ggplot(
  data = penguins,
  mapping = aes(x = flipper_len, y = body_mass, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm") # pridana vrstva s lin trendom

# Kategorie, ale trend pre vsetky spolu 
ggplot(
  data = penguins,
  mapping = aes(x = flipper_len, y = body_mass)
) +
  geom_point(mapping = aes(color = species)) + # species je prehodeny z mapping sem
  geom_smooth(method = "lm") 

## maly update
ggplot(
  data = penguins,
  mapping = aes(x = flipper_len, y = body_mass)
) +
  geom_point(mapping = aes(color = species, shape = species)) + # species je aj pre farbu, aj pre tvar
  geom_smooth(method = "lm") 

# Pridat labely
ggplot(
  data = penguins,
  mapping = aes(x = flipper_len, y = body_mass)
) +
  geom_point(mapping = aes(color = species, shape = species)) + 
  geom_smooth(method = "lm") +
  labs( # labely pre vsetko mozne
    title = "Body mass and fliupper length",
    subtitle = "Dimensions for categories",
    x  = "Flipper length",
    y = "Body mass",
    color = "Species", shape = "Species"
  )

# Farebna skala pre farboslepych
# install.packages("ggthemes")
library(ggthemes)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_len, y = body_mass)
) +
  geom_point(mapping = aes(color = species, shape = species)) + 
  geom_smooth(method = "lm") +
  labs( # labely pre vsetko mozne
    title = "Body mass and fliupper length",
    subtitle = "Dimensions for categories",
    x  = "Flipper length",
    y = "Body mass",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

## page 40, pred Excercises
