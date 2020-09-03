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
locs2020sf <- locs2020mod[complete.cases(locs2020mod),] %>%
  st_as_sf(coords=c('lon','lat'), crs=4326) %>% 
  mutate(map_num = 1:nrow(.))
# preview
leaflet() %>% addTiles() %>% 
  addMarkers(data=st_transform(locs2020sf,4326),
             label=~loc)
# export to json?

st_write(locs2020sf, dsn='locs2020.geojson')

