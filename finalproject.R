# Final Project
# Jensen Brown & Cameron Zinn

# Required libraries
library(rgdal)
library(ggplot2)
library(readxl)
library(tidyverse)
library(rgeos)
library(dplyr)
library(sf)
library(sp)
library(spdep)
library(foreign)

# Reading spreadsheets, merging spreadsheets, and filtering spreadsheet
class_data <- st_read("ENVS 4826 Project Data.csv",
                      options = c("X_POSSIBLE_NAMES=x", 
                                  "Y_POSSIBLE_NAMES=y"),
                      crs = 4326)
tree_data <-  read_xlsx("SMUtreedatabase2021.xlsx")
project_data <- merge(class_data, tree_data, by = "uid_4826")
campus_data <- filter(project_data, location == "SMU campus")
campus_data <-  drop_na(campus_data, crown_condition)
crown_condition_int <- recode(campus_data$crown_condition, 'G' = 'G', 'F' = 'FP', 'P' = 'FP', default = NA_character_)
campus_data$crown_condition_int <- crown_condition_int
print(crown_condition_int)

# Reading shapefiles
campus_buildings <- st_read(dsn = 'C:/Users/jense/Desktop/ENVS 4826 - Data Science in the Environment/FinalProjectBrownZinn-master/CampusMap_Background_SHP/CampusBuildings.shp')
campus_buildings_df <- readOGR(dsn = 'C:/Users/jense/Desktop/ENVS 4826 - Data Science in the Environment/FinalProjectBrownZinn-master/CampusMap_Background_SHP', layer = "CampusBuildings")
campus_buildings_df <- fortify(campus_buildings_df)
campus_data_fixed <- st_write(campus_data, "campus_data_fixed.shp")

# Plotting Crown Condition Map
ggplot() +
  geom_polygon(data = campus_buildings_df, aes(x = long, y = lat, group = group), colour = "Blue", fill = "Light Blue") +
  geom_point(data = campus_data, aes(x = UTMX, y = UTMY, col = crown_condition_int), size = 1) +
  scale_colour_manual(values = c('Red', 'Green'), labels = c('Fair/Poor', 'Good')) +
  labs(title = "Crown Condition of Trees Around SMU Campus", x = "UTMX", y = "UTMY", colour = "Crown Condition") +
  theme_bw()

# Plotting Trunk Damage Map
ggplot() +
  geom_polygon(data = campus_buildings_df, aes(x = long, y = lat, group = group), colour = "Blue", fill = "Light Blue") +
  geom_point(data = campus_data, aes(x = UTMX, y = UTMY, col = trunk_damage), size = 1) +
  scale_colour_manual(values = c('Black', 'Green', 'Red'), labels = c('NA', 'Absent', 'Present')) +
  labs(title = "Presence of Trunk Damage on Trees Around SMU Campus", x = "UTMX", y = "UTMY", colour = "Trunk Damage") +
  theme_bw()

# Matching coordinate systems
campus_data_fixed <- st_read("C:/Users/jense/Desktop/ENVS 4826 - Data Science in the Environment/FinalProjectBrownZinn-master/campus_data_fixed.shp")
st_crs(campus_data_fixed)
st_crs(campus_buildings)
campus_data_utm <- st_transform(campus_data_fixed, st_crs(campus_buildings))
st_crs(campus_data_utm)
campus_data_utm = subset(campus_data_utm, select = -c(x,y,))
campus_data_utm = subset(campus_data_utm, select = -c(lat,long))

names(campus_buildings)[names(campus_buildings) == 'old.var.name'] <- 'new.var.name'

min(st_distance(campus_buildings, campus_data_utm[1,]))
campus_data_dist <- sapply(1:nrow(campus_buildings), function(x) min(st_distance(campus_data_utm, campus_buildings[x, ])))

campus_data_dist <- drop_na(campus_data_dist, dbh_cm)
class(campus_data_dist$dbh_cm)
campus_data_dist$dbh_cm <- as.numeric(campus_data_dist$dbh_cm)
glm_dbh <- glm(dbh_cm ~ NEAR_DIST, data = campus_data_dist)
summary(glm_dbh)
plot(glm_dbh)
logitof(campus_data_dist$dbh_cm, fitted(glm_dbh))

lm_crown <- lm(crown_condition_int ~ campus_data_dist, data = campus_data_utm)
lm_trunk <- lm(trunk_damage ~ campus_data_dist, data = campus_data_utm)
