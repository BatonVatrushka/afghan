# extra code

#===================
# MAPS
#===================
# get the map for afghanistan
afghan = raster::getData(name = "GADM", country = "AFG", level = 1)

# plot the map
map = afghan %>% map(namefield = "NAME_1") %>% fortify() 
# rename the region column to match the df
map = map %>% rename("adm_1" = region)


# check the province names between Spatial Object and DF
cbind(afghan$NAME_1 %>% sort()
      , afg$adm_1 %>% unique() %>% na.omit() %>% sort())

# pull out " Province"
afg$adm_1 = gsub(pattern = " province", "", afg$adm_1)

# check the province names between Spatial Object and DF
cbind(afghan$NAME_1 %>% sort()
      , afg$adm_1 %>% unique() %>% na.omit() %>% sort())

deaths = afg %>% group_by(adm_1) %>% summarise(deaths = deaths_b %>% sum()) %>%
  filter(adm_1 != "NA")

# merge
my.map = merge(x = map, y = deaths, by = "adm_1")
my.map = my.map %>% select(-subregion) %>% mutate(adm_1 = amd_1 %>% tolower())

ggplot() +
  geom_polygon(data = my.map,
               aes(x = long, y = lat, group = adm_1, fill = deaths), 
               color = 'black', size = 0.25) +
  coord_map()

# palette of 30 colors
my_colors = brewer.pal(9, "Reds")
my_colors = colorRampPalette(my_colors)(34) 

# province_deaths = cut(my.map$deaths, 34)
# my_colors = my_colors[province_deaths %>% as.numeric()]

