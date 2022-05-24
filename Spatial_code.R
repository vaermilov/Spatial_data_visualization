#### Spatial data visualization ---------------------------------------

# This code is plotting the number of Wellbell subscribers on the
# map of Russia. We import data from Amplitude CSV file, then use
# partial join to match regions to the right spatial data. There
# are 2 different ways of visualization: using sf library and using
# raster library. Please note that 3 & 4 are alternative options for
# 5 & 6. Option 2 is slower but the map includes region borders.

## 1. Read data -------------------------------------------------------

df1 <- as.data.frame(read.csv("./Region.csv",
                skip = 4,
                quote = "",
                col.names = c("City", "Users", "Share")))
library(readxl)
df2 <- as.data.frame((read_excel("./Spatial.xlsx")))

## 2. Preparing data for plotting -------------------------------------

# To match data from Amplitude to spatial data (latitude and longitude
# of each region) we use partial match join (library fuzzyjoin). This
# help us to overcome different Region names in different data sources.
# At the beginning we need to tidy up data to get as well.

library(stringr)
df1$City <- str_replace_all(df1$City, "[\t]", "")
df1$City <- str_replace_all(df1$City, "\"", "")
df1$City <- word(df1$City, 1)
df1$Users <- str_replace_all(df1$Users, "\"", "")
df1$Share <- str_replace_all(df1$Share, "\"", "")
df1$Users <- as.numeric(df1$Users)
library(fuzzyjoin)
library(dplyr)
df3 <- df2 %>%
  fuzzy_inner_join(df1, by = c("City" = "City"), match_fun = str_detect)
df3$Latitude <- as.numeric(df3$Latitude)
df3$Longitude <- as.numeric(df3$Longitude)
df <- df3 %>%
  select(City.x, Users, Latitude, Longitude) %>%
  group_by(City.x, Latitude, Longitude) %>%
  summarise(Users = sum(Users))

## 3. Option 1: Get country map using sf library ----------------------
# If you want to use "Option 2" visualization - do not execute this code

library(sf)
library(rnaturalearth)
worldmap <- ne_countries(scale = "medium",
                         type = "map_units",
                         returnclass = "sf")
russia <- worldmap[worldmap$name == "Russia",]

## 4. Plotting "Option 1" using ggplot2 -------------------------------
# If you want to use "Option 2" visualization - do not execute this code

library(ggplot2)
ggplot(
  data = df) +
  ggtitle("Subscribers by region") +
  coord_fixed(xlim = c(30, 128), ylim = c(40, 85)) +
  geom_sf(data = russia) +
  coord_sf(xlim = c(30, 150),
           ylim = c(40, 85),
           lims_method = "box") +
  geom_point(
    aes(
    x = Longitude,
    y = Latitude,
    size = Users,
    colour = Users)) +
  scale_size_continuous(range = c(1,14),
                        guide = "none") +
  scale_color_viridis_b(alpha = 0.8,
                        limits = c(0, 1400),
                        begin = 0,
                        end = 1,
                        space = "Lab",
                        breaks = c(200, 400, 600, 800,
                                   1000, 1200, 1400)) +
  xlab("") +
  ylab("") +
  theme(
    aspect.ratio = 4/5,
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(25, "mm"),
    plot.margin = unit(c(5,5,5,5),"mm"),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

## 5. Option 2: Get country map using raster library ------------------
# If you want to use "Option 1" visualization - do not execute this code

library(raster)
Russia <- getData("GADM", country = "RU", level = 0)
Russia1 <- getData("GADM", country = "RU", level = 1)

## 4. Plotting "Option 2" using ggplot2 -------------------------------
# If you want to use "Option 1" visualization - do not execute this code

library(ggplot2)
ggplot(
  data = df) +
  ggtitle("Subscribers by region") +
  coord_fixed(xlim = c(30, 128), ylim = c(40, 85)) +
  geom_polygon(data = Russia, 
               aes(long, lat, group = group), 
               fill = "whitesmoke") +
  geom_path(data = Russia1, 
            aes(long, lat, group = group), 
            color = "grey", 
            size = 0.1) +
  geom_point(
    aes(
      x = Longitude,
      y = Latitude,
      size = Users,
      colour = Users)) +
  scale_size_continuous(range = c(1,14),
                        guide = "none") +
  scale_color_viridis_b(alpha = 0.8,
                        limits = c(0, 1400),
                        begin = 0,
                        end = 1,
                        space = "Lab",
                        breaks = c(200, 400, 600, 800,
                                   1000, 1200, 1400)) +
  xlab("") +
  ylab("") +
  theme(
    aspect.ratio = 4/5,
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key.width = unit(25, "mm"),
    plot.margin = unit(c(5,5,5,5),"mm"),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )