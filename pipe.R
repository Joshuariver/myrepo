# %$% : upping your pipe game
# https://www.r-bloggers.com/upping-your-pipe-game/

rm(list=ls())

# Load magrittr
library(magrittr)
library(tidyverse)

# Create data
pets <- data.frame(friend = c("Mark", "Mark", "Kyle", "Kyle", "Miranda", "Kayla", "Kayla", "Kayla", "Adriana", "Adriana", "Alex", "Randy", "Nancy"),
                     pet = c("cat", "cat", "cat", "cat", "cat", "dog", "cat", "lizard", "cat", "cat", "dog", "dog", "woodpecker"),
                     main_pet_color = c("brown", "brown", "multi", "multi", "brown", "brown", "brown", "orange", "black", "white", "multi", "white", "multi"))

# Look at the data
pets


# Make a frequency table of pet colors
table(pets$main_pet_color)

# Make a frequency table of pet colors
pets %>% table(main_pet_color)


# Arrange the data frame by pet color
pets %>% arrange(main_pet_color)

# Make a frequency table of pet types
pets %$% table(main_pet_color)

# This returns a vector
pets$main_pet_color
