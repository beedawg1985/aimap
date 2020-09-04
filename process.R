require(stringr)
require(dplyr)
require(ggmap)
require(sf)
require(leaflet)
source('private.R')
setwd('/Users/user/Documents/GitHub/aimap')
fileName <- '2019_locations.txt'
locs <- readChar(fileName, file.info(fileName)$size)
locs.s <- as.data.frame(t(str_split(locs,'\n',simplify = T))) %>% 
  filter(V1 != '')
locs.df <- as.data.frame(str_split(locs.s$V1,'  ',simplify = T))
names(locs.df) <- c('num','text')
write.csv(locs.df,file='locations2019.csv')
locs2020 <- read.csv('locations2020_locreformatted.csv',head=T,sep=',')
locs2020$loc <- str_split_fixed(locs2020$text,'[.]',2)[,1]


register_google(key = gkey)
df <- geocode(locs2020$loc, 
              output = "latlona", 
              source = "google")
locs2020geo <- cbind(locs2020,df)
write.csv(locs2020geo,file='locs2020geo.csv')
# modify csv manually, add missing locations etc.
locs2020mod <- read.csv('locs2020geo_mod.csv') %>% 
  mutate_all(as.character) %>% 
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat))
locs2020noPoint <- locs2020mod %>% filter(is.na(lat))
# load continents

continents <- st_read('https://gist.githubusercontent.com/hrbrmstr/91ea5cc9474286c72838/raw/59421ff9b268ff0929b051ddafafbeb94a4c1910/continents.json')
locs2020sf <- locs2020mod[complete.cases(locs2020mod),] %>%
  st_as_sf(coords=c('lon','lat'), crs=4326) %>% 
  st_join(continents) %>% 
  arrange(CONTINENT) %>% 
  mutate(map_num = 1:nrow(.))

# get eu bounding box
euBbox <- continents %>% filter(CONTINENT == 'Europe') %>% 
  st_bbox() %>% st_as_sfc() %>% 
  st_transform(3857) %>% 
  st_cast('LINESTRING') %>% 
  st_line_sample(200) %>% 
  st_cast('POINT') %>% 
  st_transform(4326) %>% 
  st_as_sf() %>% 
  mutate(new_id = seq(1000,(999+nrow(.))))
  # st_sf() %>% 
  # st_reverse() # reverse so d3 reads correctly!

# preview
leaflet() %>% addTiles() %>% 
  addMarkers(data=euBbox)
  # addMarkers(data=st_transform(locs2020sf,4326),
  #            label=~loc)

# export to json
st_write(euBbox, 
         dsn='eu_bbox_points200.geojson',
         delete_dsn = T)

## vertices of bbox need to be manually reversed !!

st_write(locs2020sf %>% filter(CONTINENT != 'Europe'), 
         dsn='locs2020_rw.geojson',
         delete_dsn = T)
st_write(locs2020sf %>% filter(CONTINENT == 'Europe'), 
         dsn='locs2020_eu.geojson',
         delete_dsn = T)

