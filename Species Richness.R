#Goal: Calculate species richness by year and calculate the mean
#start packages
library(dplyr)
library(ggplot2)

#Access data
garden.data.raw <- read.csv("~/Documents/Garden Data Analysis/Garden Data Full.csv") %>%
  mutate(weight_llbs = weight_oz * 0.0625)
crop.families <- read.csv("~/Documents/Garden Data Analysis/crop_key_species.csv")

#Join two data sets
garden.data.families <- full_join(garden.data.raw, crop.families,
                                  by = NULL, copy = FALSE,
                                  suffix = c(".garden.data.raw",
                                                           ".crop.families"))
#Change count to numeric
garden.data.families$count <- as.numeric(garden.data.families$count)
str(garden.data.families)


#Count the number of crops per year
crops.2011 <- garden.data.families %>%
  group_by(year) %>%
  filter(year == 2011)

factor.2011 <- factor(crops.2011$crop)
factor.2011
summary(factor.2011)
#For 2011, there were 44 crop varieties planted

crops.2012 <- garden.data.families %>%
  group_by(year) %>%
  filter(year == 2012)

factor.2012 <- factor(crops.2012$species)
summary(factor.2012)
#For 2012, there were 45 crop varieties planted

crops.2013 <- garden.data.families %>%
  group_by(year) %>%
  filter(year == 2013)

factor.2013 <- factor(crops.2013$species)
summary(factor.2013)
#For 2013, there were 39 crop varieties planted

#Create data frame
year <- c("2011", "2012", "2013")
richness <- c("44", "44", "39")

richness.by.year <- data.frame(year,richness)

#Plot species richness by year
ggplot(richness.by.year, aes(x = year, y = richness)) + geom_col()

