#Goal: lbs/acre calculations, 600 ft^2 total, 325 ft^2 in production
# 1 ft^2 = 2.29568411 x 10^-5 acres
install.packages('DT')

#load packages
library(dplyr)
library(ggplot2)
library(DT)

#load data
garden.data.species <- read.csv("~/Documents/Garden Data Analysis/Garden Data Full.csv") %>%
  mutate(weight_lbs = weight_oz * 0.0625)
crop.species <- read.csv("~/Documents/Garden Data Analysis/crop_key_species.csv")
garden.data.full <- inner_join(garden.data.species, crop.species, by = NULL,
                              copy = FALSE, suffix = c(".garden.data.species",
                                                   ".crop.species"))

#conversions
a <- 600 * 2.29568411 * 10^-5
b <- 325 * 2.29568411 * 10^-5

#filter by year and species
#2011
crop.2011 <- garden.data.full %>%
  filter(year == 2011)
species.2011 <- crop.2011$species
weight_lbs_2011 <- crop.2011$weight_lbs
family.2011 <- crop.2011$family
species_lbs_2011 <- data.frame(species.2011, family.2011, weight_llbs_2011)

total.crop.2011 <- species_lbs_2011 %>%
  group_by(family.2011) %>%
  summarize(total_lbs = sum(weight_lbs_2011)) %>%
  mutate(total_lbs_acre = total_lbs / a) %>%
  mutate(product_lbs_acre = total_lbs / b)

#2012
crop.2012 <- garden.data.full %>%
  filter(year == 2012)
species.2012 <- crop.2012$species
weight_lbs_2012 <- crop.2012$weight_lbs
family.2012 <- crop.2012$family
species_lbs_2012 <- data.frame(species.2012, family.2012, weight_lbs_2012)

total.crop.2012 <- species_llbs_2012 %>%
  group_by(family.2012) %>%
  summarize(total_lbs = sum(weight_lbs_2012)) %>%
  mutate(total_lbs_acre = total_lbs / a) %>%
  mutate(product_lbs_acre = total_lbs / b)

#2013
crop.2013 <- garden.data.full %>%
  filter(year == 2013)
species.2013 <- crop.2013$species
family.2013 <- crop.2013$family
weight_lbs_2013 <- crop.2013$weight_lbs
species_lbs_2013 <- data.frame(species.2013, family.2013, weight_lbs_2013)

total.crop.2013 <- species_lbs_2013 %>%
  group_by(family.2013) %>%
  summarize(total_lbs = sum(weight_lbs_2013)) %>%
  mutate(total_lbs_acre = total_lbs / a) %>%
  mutate(total_lbs_acre = total_lbs / b)

acre_over_time <- garden.data.full %>%
  mutate(lbs_acre = weight_lbs * a) %>%
  mutate(product_acre = weight_lbs * b)
str(acre_over_time)
acre_over_time$year <- as.character.Date(acre_over_time$year)

ggplot(acre_over_time, aes(x = year, y = lbs_acre)) + geom_violin() +
  scale_y_log10()
ggplot(acre_over_time, aes(x = year, y = product_acre, fill = family)) +
  geom_col()

str(acre_all_year)
