# Afghanistan
library(pacman)
p_load(tidyverse)
p_load(lubridate)
p_load(maps)
p_load(ggmap)
p_load(maptools)
# install.packages("rgeos", type = "source")
# install.packages("rgdal", type = "source")
p_load(sf)
p_load(RColorBrewer)
p_load(rgdal)
p_load(scales)


# read in the data for afghanistan
afg = read_csv('gedevents-2021-08-21.csv')
afg = afg %>% arrange(year)

# read in the date for pakistan
pak = read_csv('pakistan.csv') %>% arrange(year)

# look at total deaths by year and make a column graph
afg %>% group_by(year) %>% summarize(deaths = best_est %>% sum()) %>%
  ggplot() + aes(year, deaths) + geom_col()

# filter the DF for only Taliban as the adversarial side
talib = afg %>% filter(side_b == "Taleban") %>% 
  # calculate the length of the battles
  mutate("battle_length" = 
           (date_end %>% 
              strptime(format = "%m/%d/%Y", tz = "UTC") %>% as_date() - 
              date_start %>% 
              strptime(format = "%m/%d/%Y", tz = "UTC") %>% as_date()))

# look at the longest battles
talib %>% select(battle_length) %>% top_n(20) %>% arrange(battle_length %>% desc())

# look at the distribution of Talib deaths
talib %>% ggplot() + aes(deaths_b) %>% geom_histogram(color = 'black'
                                                      , bins = 100)
# distribution of government deaths
talib %>% ggplot() + aes(deaths_a) %>% geom_histogram(color = 'black'
                                                      , bins = 100)

# look at the distribution of Battle length
talib %>% ggplot() + aes(battle_length) %>% geom_histogram(bins = 100, color = "black")

# column graph of government deaths by year
talib %>% group_by(year) %>% summarise(deaths = deaths_a %>% sum()) %>%
  ggplot() + aes(year, deaths) + geom_col(color = "black") 

# column graph of total deaths by year
talib %>% mutate(year = strptime(year, format = '%Y') %>% as_date()) %>%
  group_by(year) %>% 
  summarise(deaths = best_est %>% sum()) %>%
  ggplot() + aes(year, deaths) + geom_col(color = "black") +
  scale_x_date(breaks = "2 year",date_labels = "%Y") + theme_minimal()

# Total Talib Deaths after the US invasion
talib %>% filter(year >= 2001) %>% 
  summarise(deaths = deaths_b %>% sum())

# column graph of Talib deaths by year
talib  %>%  mutate(year = strptime(year, format = '%Y') %>% as_date()) %>%
  group_by(year) %>% 
  summarise(deaths = deaths_b %>% sum()) %>%
  ggplot() + aes(year, deaths) + geom_col(color = "black") +
  scale_x_date(breaks = "2 year",date_labels = "%Y") + theme_minimal()

#===================
# PAKISTAN 
#===================
us_pak = pak %>% filter(side_a %in% c("Government of Afghanistan"
                                      , "Government of United States of America"))

us_pak %>% group_by(year, side_b) %>% summarise(deaths = deaths_b %>% sum()) %>% 
  arrange(deaths %>% desc()) %>%
  ggplot() + aes(year, deaths, fill = side_b) + geom_col()

#===============================================================================
# Map w/ Shape File
#===============================================================================
#ogrListLayers('gadm36_AFG_1.shp')
#afghanistan = readOGR('afg_shp_files/gadm36_AFG_1.shp')
afghanistan <- st_read('./afg_shp_files/gadm36_AFG_shp/gadm36_AFG_1.shp')

# map of Afghanistan
ggplot(data = afghanistan) +
  geom_sf() +
  ggtitle("Map of Afghanistan") +
  theme_minimal()

# create the data frame w/ fortify
# map = afghanistan %>% fortify(region = "NAME_1")

# check the province names between Spatial Object and DF
cbind(afghanistan$NAME_1 %>% sort()
      , afg$adm_1 %>% unique() %>% na.omit() %>% sort())

# pull out " Province" from afg 
afg$adm_1 = gsub(pattern = " province", "", afg$adm_1)

# check the province names between Spatial Object and DF
cbind(afghanistan$NAME_1 %>% sort()
      , afg$adm_1 %>% 
        unique() %>% 
        na.omit() %>% 
        sort())

# create a deaths df
deaths = afg %>% 
  group_by(adm_1) %>% 
  summarise(deaths = deaths_b %>% sum()) %>%
  filter(adm_1 != "NA")

# rename Panjsher
deaths <- deaths %>%
  mutate(adm_1 = if_else(adm_1 == "Panjsher", "Panjshir", adm_1)) %>% 
  rename(NAME_1 = adm_1)

# left join deaths to afghanistan
afghanistan <- afghanistan %>% left_join(deaths, by = 'NAME_1')

# create the 
ggplot(data = afghanistan) +
  geom_sf(aes(fill = deaths)) 
  scale_fill_gradientn(colors = c("white", "orange", "red"),
                       values = c(0, 0.5, 1), # can be adjusted based on distribution  
                       limits = c(min(afghanistan$deaths, na.rm = TRUE), 
                                  max(afghanistan$deaths, na.rm = TRUE)),
                       name = "Death Count") +
  labs(title = "Choropleth Map of Afghanistan") +
  theme_minimal()

#===============================================================================
# Pakistan
#===============================================================================
# get the map from the shape file
pakistan = readOGR('pak_adm_ocha_pco_gaul_20181218_SHP/pak_admbnda_adm1_ocha_pco_gaul_20181218.shp')


pakistan %>% plot(fill)
points(us_pak$longitude, us_pak$latitude
       , cex = sqrt(us_pak$best_est/pi)
       , col = 'red')

#===============================================================================
# Experiment
#===============================================================================
# afghanistan data frame
af_df = afghanistan  %>% fortify(region = "NAME_1")
# pakistan data frame
pak_df = pakistan %>% fortify(region = "ADM1_EN") %>% 
  rename("adm_1" = id)

# check the province names between Spatial Object and DF
cbind(pak$adm_1 %>% unique() %>% sort()
      , pakistan$ADM1_EN %>% unique() %>% na.omit() %>% sort())

# change names 
pak$adm_1 = pak$adm_1 %>% recode(
   "Federally Administered Tribal Areas" = "Khyber Pakhtunkhwa"
  , "Khyber Pakhtunkwa Province" = "Khyber Pakhtunkhwa"
  ,  "Balochistan Province" = "Balochistan"
  ,  "Islamabad Capital Territory" = "Federal Capital Territory"
  ,  "Punjab Province" = "Punjab" 
  ,  "Sindh Province" = "Sindh" 
)
# create a df with only the five provinces in the map 
pak_5 = pak %>% filter(adm_1 %in% c("Balochistan", "Federal Capital Territory"
                                    , "Khyber Pakhtunkhwa", "Punjab"
                                    , "Sindh")) %>% 
  rename("id" = adm_1)
# summarize deaths
deaths_pak = pak_5 %>% group_by(adm_1) %>% summarise(deaths = deaths_b %>% sum())

# merge
pak_map = inner_join(pak_df, deaths_pak, by = 'adm_1')


#*****************************************************************************#
ggplot(pak_map) + aes(long, lat, group = group) +
  
  geom_polygon(aes(fill = deaths, group = group), color = 'black') +
  
  geom_polygon(aes(long, lat, fill = deaths ,group=group)
               , color = 'black'
               , data = afg_map) +
  
  scale_fill_gradient(low = 'white', high = 'red') + theme_minimal()

#*****************************************************************************#
# THE MAP MY NIBBA
#*****************************************************************************#
ggplot(pak_map, aes(long, lat, group = group)) + geom_path() +
  
  geom_point(data = pak_5, aes(longitude, latitude, group = adm_1
                               , size = sqrt(best_est/pi))
             , color = '#006600', alpha = .2) +
  
  geom_path(aes(long, lat, group=group)
               , color = 'black'
               , data = afg_map) + 
  
  geom_point(data = afg, aes(longitude, latitude
                             , group = adm_1, size = sqrt(best_est/pi))
             , color = '#d32011', alpha = .2) +
  
  theme_nothing()
#*****************************************************************************#
# Roads Shape File
#*****************************************************************************#
roads = readOGR('afg roads/hotosm_afg_roads_lines.shp')
hwys = roads[roads$highway == c('primary', 'secondary', 'tertiary'),]
hwys %>% plot()

par(mar = c(0,0,0,0))
afghanistan %>% plot()
hwys %>% plot(add=T)
points(talib$longitude, talib$latitude
       , col = rgb(1,0,0,alpha = .4)
       , cex = rescale(radius, c(0,12))
       , pch = 21)

# plot the map
ggplot(afg_map) + aes(long, lat) +  
  
  geom_polygon(aes(group = group)
               , color = 'black'
               , fill = 'white') + 
  
  geom_point(data = talib, aes(x = longitude
                 , y = latitude
                 , alpha = .2
                 , color = '#660000'
                 , size = rescale(radius, c(0, 12)))
             , shape = 21) +
  
  theme_nothing()


 


