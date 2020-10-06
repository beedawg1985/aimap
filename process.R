require(ggplot2)
require(rnaturalearth)
require(rnaturalearthdata)
require(ggrepel)
require(stringr)
require(dplyr)
require(ggmap)
require(sf)
require(leaflet)
setwd('/Users/user/Documents/GitHub/aimap')
source('private.R')
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

p <- st_read('locs2020_w_airegions.shp')

world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_transform(54009)
grats <- st_read('ne_10m_graticules_all/ne_10m_graticules_5.shp') %>% 
  st_transform(54009)
gb <- p %>% filter(AI_Region == 'Britain and Ireland') %>% 
  st_transform(54009)

bbox <- st_bbox(st_buffer(gb,100000))

ggplot() + 
  geom_sf(data = world, aes(fill = region_wb)) + 
  geom_sf(data=gb) + 
  geom_sf_label(data=gb,aes(label = num)) +
  coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE) + 
  theme(legend.position = 'none')

gb.df <- st_coordinates(gb) %>% as.data.frame() %>% 
  mutate(num = gb$num)

gbplot <- ggplot() + 
  geom_sf(data = world, aes(fill = region_wb)) + 
  geom_text_repel(data = gb.df, aes(x = X, y=Y,label=num),
                  segment.colour = 'grey') + 
  geom_point(data=gb.df, aes(x = X, y=Y)) + 
  coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE) + 
  theme(legend.position = 'none')

remotes::install_github('slowkow/ggrepel')

a <- ggplot_build(gbplot)
a$plot$layers

a$layout$train_position

ggplot() + 
  coord_sf(data=world$geometry)
require(devtools)
require(sf)
Sys.setenv(R_REMOTES_STANDALONE="true")
remotes::install_github("yutannihilation/ggsflabel")

continents <- st_read('https://gist.githubusercontent.com/hrbrmstr/91ea5cc9474286c72838/raw/59421ff9b268ff0929b051ddafafbeb94a4c1910/continents.json')
locs2020sf <- locs2020mod[complete.cases(locs2020mod),] %>%
  st_as_sf(coords=c('lon','lat'), crs=4326) %>% 
  st_join(continents) %>% 
  arrange(CONTINENT) %>% 
  mutate(map_num = 1:nrow(.))

# get eu bounding box
euBbox <- st_read('w_eu.gpkg') %>% 
  st_cast('MULTILINESTRING') %>% 
  st_cast('LINESTRING') %>% 
  st_transform(3857) %>% 
  st_line_sample(200) %>% 
  st_cast('POINT') %>% 
  st_transform(4326) %>% 
  st_as_sf() %>% 
  mutate(new_id = seq(1000,(999+nrow(.))))
  # st_sf() %>% 
  # st_reverse() # reverse so d3 reads correctly!


# preview
leaflet() %>% addTiles() %>% 
  addMarkers(data=euBbox) %>% 
  addPolygons(data=int)
  # addMarkers(data=st_transform(locs2020sf,4326),
  #            label=~loc)

# export to json
st_write(euBbox, 
         dsn='weu.geojson',
         delete_dsn = T)


euBboxPol <- st_read('w_eu.gpkg')
locs2020sf_weu <- st_intersection(locs2020sf, euBboxPol)

st_write(locs2020sf_weu, 
         dsn='locs2020_weu.geojson',
         delete_dsn = T)
st_write(locs2020sf %>% filter(map_num %!in% locs2020sf_weu$map_num), 
         dsn='locs2020_rw.geojson',
         delete_dsn = T)

land50m <- st_read('countries-50m.json',
                   layer='land',crs=4326) %>% 
  lwgeom::st_make_valid()
int <- st_intersection(land50m, euBboxPol)
st_write(int, 
         dsn='countries-50m_int.geojson',
         delete_dsn = T)
