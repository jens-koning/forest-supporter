### Grid-cell map (0.5 degrees) of world
install.packages("sf")
install.packages("raster")
install.packages("tmap")
install.packages("tmaptools")
install.packages("rgdal")
install.packages("OpenStreeetMap")
install.packages("ggplot2")

#trials 2

#Making smaller data set
library(sf)
library(ggrepel)
library(raster)
library(rgdal)
library(tidyverse)
library(rnaturalearth)
library(stars)



Dataviz_full <- select(full, c("x", "y", "year", "FID", "forest")) #0.5 degree data
Dataviz_1982 <- Dataviz_full %>%
  filter(year == "1982")
Dataviz_1992 <- Dataviz_full %>%
  filter(year == "1992")
Dataviz_2002 <- Dataviz_full %>%
  filter(year == "2002")
Dataviz_2012 <- Dataviz_full %>%
  filter(year == "2012")


view(Dataviz_1982)

Dataviz_1982_t <- Dataviz_1982

## ggplot
ggplot(Dataviz_1982) +
  geom_tile(aes(x = x, y = y, fill = forest))

  
## covert to sf
Dataviz_1982_sf <- sf::st_as_sf(Dataviz_1982, coords = c("x", "y")) 

## set crs (coordinate reference system)
Dataviz_1982_sf_1 <- select(Dataviz_1982_sf, c("forest", "geometry"))

st_crs(Dataviz_1982_sf_1) <- 4326


plot(Dataviz_1982_sf["forest"])



## make raster of 1982 data
rast1 <- raster::rasterize(Dataviz_1982_sf_1, y = raster(nrow = 360, ncol = 720), "forest", crs = sf::st_crs(Dataviz_1982_sf_1)) #+
  #raster::extent(c(-180,180,-75,75))

#simple plot
plot(rast1$layer)
plot(sf::st_geometry(world1, alpha = 0.1), add = T)



install.packages("cshapes")
library(cshapes)
world1 <- cshapes::cshp(as.Date("2019-01-01"))

world %>%
  st_transform(world, crs = 54030)

plot(world)
plot(sf::st_geometry(world), add = T, main = "Global Forest Cover (1982)")


## experiment w/st_make_grid                         
Dataviz_1982_sf_rs<- st_make_grid(Dataviz_1982_sf, cellsize = 0.5)



#### FROM WEBPAGE
robinson <- CRS("+proj=robin +over")
countries <- ne_countries(scale = 50, returnclass = c("sf"))
# create a bounding box for the robinson projection
# we'll use this as "trim" to remove jagged edges at
# end of the map (due to the curved nature of the
# robinson projection)
bb <- sf::st_union(sf::st_make_grid(
  st_bbox(c(xmin = -180,
            xmax = 180,
            ymax = 90,
            ymin = -90), crs = st_crs(4326)), n = 100))
bb_robinson <- st_transform(bb, as.character(robinson))
# transform the coastline to robinson
countries_robinson <- st_transform(countries, robinson)

p1 <- ggplot()+
  geom_raster(
    data = Dataviz_1982_t,
    aes(
      x=x,
      y=y,
      fill=forest,
    ),
 interpolate = TRUE
  ) +
  scale_fill_identity() +
  geom_sf(data=countries_robinson,
          colour='grey25',
          linetype='solid',
          fill= NA,
          size=0.3) +
  geom_sf(data=bb_robinson,
          colour='white',
          linetype='solid',
          fill = NA,
          size=0.7) +
labs(
  title = "Forest Cover",
  subtitle = "1982",
  caption = "Data: Hansen (2018), Sanford (2021) and Song et al. (2018)
")

## Jonas kode


## covert to sf
Dataviz_1982_sf <- sf::st_as_sf(Dataviz_1982, coords = c("x", "y")) 
Dataviz_1992_sf <- sf::st_as_sf(Dataviz_1992, coords = c("x", "y")) 
Dataviz_2002_sf <- sf::st_as_sf(Dataviz_2002, coords = c("x", "y")) 
Dataviz_2012_sf <- sf::st_as_sf(Dataviz_2012, coords = c("x", "y")) 


#make smaller sf
Dataviz_1982_sf_1 <- select(Dataviz_1982_sf, c("forest", "geometry"))
## set crs (coordinate reference system)
st_crs(Dataviz_1982_sf_1) <- 4326
st_crs(Dataviz_1982_sf) <- 4326
st_crs(Dataviz_1992_sf) <- 4326
st_crs(Dataviz_2002_sf) <- 4326
st_crs(Dataviz_2012_sf) <- 4326
## make raster of 1982 data
rast <- raster::rasterize(Dataviz_1982_sf, y = raster(nrow = 360, ncol = 720), "forest", crs = sf::st_crs(Dataviz_1982_sf)) #+
  #raster::extent(c(-180,180,-75,75))
rast1 <- raster::rasterize(Dataviz_1992_sf, y = raster(nrow = 360, ncol = 720), "forest", crs = sf::st_crs(Dataviz_1982_sf))

rast2 <- raster::rasterize(Dataviz_2002_sf, y = raster(nrow = 360, ncol = 720), "forest", crs = sf::st_crs(Dataviz_1982_sf))

rast3 <- raster::rasterize(Dataviz_2012_sf, y = raster(nrow = 360, ncol = 720), "forest", crs = sf::st_crs(Dataviz_1982_sf))

#make world
world <- cshapes::cshp(as.Date("2012-01-01"))
world <- st_transform(world, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

rast <- projectRaster(rast, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
rast <- st_as_stars(rast)
rast1 <- projectRaster(rast1, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
rast1 <- st_as_stars(rast1)
rast2 <- projectRaster(rast2, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
rast2 <- st_as_stars(rast2)
rast3 <- projectRaster(rast3, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
rast3 <- st_as_stars(rast3)


install.packages("viridis")
library(viridis)


ggplot() +
  geom_stars(data = rast, alpha =0.7) +
  geom_sf(data = world, fill = NA) +
  scale_fill_gradient(low = "lightbrown", high = "darkgreen") +
  coord_sf() +
  labs(
    title = "Global Forest Cover (1982)",
    caption = "Data: Hansen (2018) and Song et al. (2018)
") +
  geom_sf(data=bb_robinson,
           colour='white',
           linetype='solid',
           fill = NA,
           size=0.7)

stars <- geom_stars(data = rast, alpha =1)
  



robinson <- CRS("+proj=robin +over")
countries <- ne_countries(scale = 50, returnclass = c("sf"))
# create a bounding box for the robinson projection
# we'll use this as "trim" to remove jagged edges at
# end of the map (due to the curved nature of the
# robinson projection)
bb <- sf::st_union(sf::st_make_grid(
  st_bbox(c(xmin = -180,
            xmax = 180,
            ymax = 90,
            ymin = -90), crs = st_crs(4326)), n = 100))
bb_robinson <- st_transform(bb, as.character(robinson))
# transform the coastline to robinson
countries_robinson <- st_transform(countries, robinson)

p <- ggplot() +
  geom_sf(data=bb_robinson,
          colour='grey25',
          linetype='solid',
          fill = NA,
          size=0.7)  +
  geom_sf(data=countries_robinson,
          colour='grey15', # TRY WITH NA
          linetype='solid',
          fill= NA,
          size=0.3) +
  geom_stars(data = rast, alpha =0.85) +
  scale_fill_viridis_c(name = "Forest in cell (%)", option="viridis", na.value = "grey92") +
  coord_sf() +
  labs(
    title = "Forest Cover (1982)",
    caption = "Robinson projection, 0.5° grid-cells. Data: Hansen (2018) & Song et al. (2018)
") +
theme(axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(), 
      axis.title = element_blank())

##WHITE BACKGROUND COLOUR & NO TITLE
#1982
ggplot() +
  geom_sf(data=bb_robinson,
          colour='grey15',
          linetype='solid',
          fill = NA,
          size=0.7)  +
  geom_sf(data=countries_robinson,
          colour='grey15', # TRY WITH NA
          linetype='solid',
          fill= NA,
          size=0.3) +
  geom_stars(data = rast, alpha =0.85) +
  scale_fill_viridis_c(name = "Forest in cell (%)", option="viridis", na.value = "white") +
  coord_sf() +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank()) 

ggsave("image_1982.png", plot = last_plot(), dpi = 300)

#1992
ggplot() +
  geom_sf(data=bb_robinson,
          colour='grey15',
          linetype='solid',
          fill = NA,
          size=0.7)  +
  geom_sf(data=countries_robinson,
          colour='grey15', # TRY WITH NA
          linetype='solid',
          fill= NA,
          size=0.3) +
  geom_stars(data = rast, alpha =0.85) +
  scale_fill_viridis_c(name = "Forest in cell (%)", option="viridis", na.value = "white") +
  coord_sf() +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank()) 

ggsave("image_1992.png", plot = last_plot(), dpi = 300)

#2002
ggplot() +
  geom_sf(data=bb_robinson,
          colour='grey15',
          linetype='solid',
          fill = NA,
          size=0.7)  +
  geom_sf(data=countries_robinson,
          colour='grey15', # TRY WITH NA
          linetype='solid',
          fill= NA,
          size=0.3) +
  geom_stars(data = rast, alpha =0.85) +
  scale_fill_viridis_c(name = "Forest in cell (%)", option="viridis", na.value = "white") +
  coord_sf() +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank()) 

ggsave("image_2002.png", plot = last_plot(), dpi = 300)

#2012
ggplot() +
  geom_sf(data=bb_robinson,
          colour='grey15',
          linetype='solid',
          fill = NA,
          size=0.7)  +
  geom_sf(data=countries_robinson,
          colour='grey15', # TRY WITH NA
          linetype='solid',
          fill= NA,
          size=0.3) +
  geom_stars(data = rast, alpha =0.85) +
  scale_fill_viridis_c(name = "Forest in cell (%)", option="viridis", na.value = "white") +
  coord_sf() +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank()) 

ggsave("image_2012.png", plot = last_plot(), dpi = 300)


##GREY BACKGROUND COLOUR
#1982
ggplot() +
  geom_sf(data=bb_robinson,
          colour='grey25',
          linetype='solid',
          fill = NA,
          size=0.7)  +
  geom_sf(data=countries_robinson,
          colour='grey15', # TRY WITH NA
          linetype='solid',
          fill= NA,
          size=0.3) +
  geom_stars(data = rast, alpha =0.85) +
  scale_fill_viridis_c(name = "Forest (%)", option="viridis", na.value = "grey92") +
  coord_sf() +
  labs(
    title = "1982") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank())

ggsave("image_1982_grey.png", plot = last_plot(), dpi = 300)

#1992
ggplot() +
  geom_sf(data=bb_robinson,
          colour='grey25',
          linetype='solid',
          fill = NA,
          size=0.7)  +
  geom_sf(data=countries_robinson,
          colour='grey15', # TRY WITH NA
          linetype='solid',
          fill= NA,
          size=0.3) +
  geom_stars(data = rast1, alpha =0.85) +
  scale_fill_viridis_c(name = "Forest (%)", option="viridis", na.value = "grey92") +
  coord_sf() +
  labs(
    title = "1992") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank())

ggsave("image_1992_grey.png", plot = last_plot(), dpi = 300)

#2002
ggplot() +
  geom_sf(data=bb_robinson,
          colour='grey25',
          linetype='solid',
          fill = NA,
          size=0.7)  +
  geom_sf(data=countries_robinson,
          colour='grey15', # TRY WITH NA
          linetype='solid',
          fill= NA,
          size=0.3) +
  geom_stars(data = rast2, alpha =0.85) +
  scale_fill_viridis_c(name = "Forest (%)", option="viridis", na.value = "grey92") +
  coord_sf() +
  labs(
    title = "2002") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank())

ggsave("image_2002_grey.png", plot = last_plot(), dpi = 300)

#2012
ggplot() +
  geom_sf(data=bb_robinson,
          colour='grey25',
          linetype='solid',
          fill = NA,
          size=0.7)  +
  geom_sf(data=countries_robinson,
          colour='grey15', # TRY WITH NA
          linetype='solid',
          fill= NA,
          size=0.3) +
  geom_stars(data = rast3, alpha =0.85) +
  scale_fill_viridis_c(name = "Forest (%)", option="viridis", na.value = "grey92") +
  coord_sf() +
  labs(
    title = "2012") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank())

ggsave("image_2012_grey.png", plot = last_plot(), dpi = 300)

## Displaying democratic transistions as a table
countries_transition <- select(country_full_new, c("year", "country_name", "democracy_BX", "UN_ccode", "v2reginfo"))

#tests
test %>%
  group_by(country_name) %>%
  summarize(changes=sum(change,na.rm=TRUE)) %>% View()
test <- test %>%
 mutate(lag_v2reginfo = lag(v2reginfo,1))
test %>%
filter(change==1) %>% View()
test %>%
  filter(change==1) %>% xtable::xtable()
test1 <- test %>%
  filter(change==1) 

#Making transistion table
transisitons  <- select(test1, -c("un", "democracy_BX", "UN_ccode", "lag_v2reginfo")) 
transisitons <-  transisitons[-c(67),]
transisitons$year <- as.vector(transisitons$year)
transisitons <- arrange(transisitons, year, country_name, change, v2reginfo)
transisitons <- arrange(transisitons, (year))

transisitons %>% 
  xtable::xtable()

## Making business elites table
business_elites <- select(country_new, c("year", "country_name", "democracy_BX", "Busielite_mimp","UN_ccode", "v2reginfo", "forest.diff"))

business_elites <- business_elites %>%
  filter(Busielite_mimp == "1", .preserve = FALSE) %>%
  group_by(country_name, v2reginfo) %>%
  summarise(Busielite_mimp = sum(Busielite_mimp)) 


business_elites %>% 
  xtable::xtable()

# Making urban supporters table
urban_support <- select(country_new, c("year", "country_name", "democracy_BX", "Urbansup","UN_ccode", "v2reginfo", "forest.diff"))

urban_support <- urban_support %>%
  filter(Urbansup == "1", .preserve = FALSE) %>%
  group_by(country_name, v2reginfo) %>%
  summarise(Urbansup = sum(Urbansup)) 


urban_support %>% 
  xtable::xtable()

# Making rural supporters table
rural_support <- select(country_new, c("year", "country_name", "democracy_BX", "Ruralsup","UN_ccode", "v2reginfo", "forest.diff"))

rural_support <- rural_support %>%
  filter(Ruralsup == "1", .preserve = FALSE) %>%
  group_by(country_name, v2reginfo) %>%
  summarise(Ruralsup = sum(Ruralsup)) 

# Making agrarian elites table
agrarian_elites <- select(country_new, c("year", "country_name", "democracy_BX", "Agrelite_mimp","UN_ccode", "v2reginfo", "forest.diff"))

agrarian_elites <- agrarian_elites %>%
  filter(Agrelite_mimp == "1", .preserve = FALSE) %>%
  group_by(country_name, v2reginfo) %>%
  summarise(Agrelite_mimp = sum(Agrelite_mimp)) 

##calculate average forest loss rate for these countries and plug into table



##Vizualising frequncy of support coaltion members

country_viz$year <- as.Date(as.character(country_viz$year), format = "%Y")
str(country_viz)


ggplot(country_viz, mapping = aes(year, Busielite_mimp)) +
  geom_line() + 
  scale_x_date(date_labels = "%Y")
  xlab("")
  
###Summary statistics table
install.packages("vtable")
library(vtable)

sumtable(country_new, out='latex', vars= c("forest", "forest.l", "forest.diff", "PCGDP.change.l", "Pop.growth.l", "PCGDP.l", "democracy_BX", "election_DPI", "close90", "Ruralsup", "Urbansup", "Ruralloc", "Urbanloc", "Agrelite_mimp", "Busielite_mimp"))

sumtable(Full_new, out='latex', vars= c("forest", "forest.l", "forest.diff", "PCGDP.change.l", "Pop.growth.l", "PCGDP.l", "democracy_BX", "election_DPI", "close90", "Ruralsup", "Urbansup", "Ruralloc", "Urbanloc", "Agrelite_mimp", "Busielite_mimp"))


###Visualize supporters on the map

country_groups_viz <- select(country_new_df, c("un", "x", "y", "country_name", "year", "Ruralsup", "Urbansup", "Agrelite_mimp", "Busielite_mimp", "v2reginfo"))

Rural_groups_viz <- country_groups_viz %>%
  group_by(country_name, un) %>%
  summarise(Ruralsup = sum(Ruralsup)) 

Urban_groups_viz <- country_groups_viz %>%
  group_by(country_name, un) %>%
  summarise(Urbansup = sum(Urbansup)) 

Agrelite_mimp_viz <- country_groups_viz %>%
  group_by(country_name, un) %>%
  summarise(Agrelite_mimp = sum(Agrelite_mimp)) 

Busielite_mimp_viz <- country_groups_viz %>%
  group_by(country_name, un) %>%
  summarise(Busielite_mimp = sum(Busielite_mimp)) 


order_by(Rural_groups_viz, desc)
Rural_groups_viz <- Rural_groups_viz[order(-Ruralsup),]


###Calculate global average forest loss

mean(Full_new$forest.diff, na.rm = TRUE)
mean(country_new$forest.diff, na.rm = TRUE)


# Using polyarchy
m1_org_poly <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + v2x_polyarchy|FID + year|0|un + year, data=Full_new)

# Displaying distribution of forest change on democracy
ggplot(Full_new, aes(v2x_polyarchy, forest.diff, color = v2x_polyarchy)) +
  geom_point(shape = ".", alpha = 1/50) +
  scale_color_gradient(low = "red", high = "darkblue", name = "Polyarchy") +
  ylab("% Forest Change (t-1 to t+0)") +
  xlab("V-Dem Polyarchy Index (0 to 1)") +
  theme_bw()

ggsave("polyarchy_forestchange_50.png", plot = last_plot(), dpi = 300)

ggplot(Full_new, aes(v2x_polyarchy, forest, color = v2x_polyarchy)) +
  geom_point(shape = ".", alpha = 1/50) +
  scale_color_gradient(low = "red", high = "darkblue", name = "Polyarchy") +
  ylab("% Forest in cell (t+0)") +
  xlab("V-Dem Polyarchy Index (0 to 1)") +
  theme_bw()

ggsave("polyarchy_forest.png", plot = last_plot(), dpi = 300)

# Displaying forest change and GDP per capita

#PCGDP
ggplot(Full_new, aes(PCGDP.l, forest.diff, color = PCGDP.l)) +
  geom_point(shape = ".", alpha = 1/30) +
  scale_color_viridis_c(direction = 1, name = "$USD (x1000)") +
  ylab("% Forest Change (t-1 to t+0)") +
  xlab("Per capita Gross Domestic Product ($, fixed 2010)") +
  xlim(0, 90) +
  ylim(-35, 35) +
  theme_minimal()


ggsave("PCGDP_forest.png", plot = last_plot(), dpi = 200)


#Ag.pct.l
ggplot(Full_new, aes(Ag.pct.l, forest.diff, color = Ag.pct.l)) +
  geom_point(shape = ".", alpha = 1/25) +
  scale_color_viridis_c(direction = 1, name = "(%) in Agriculture") +
  ylab("% Forest Change (t-1 to t+0)") +
  xlab("Percentage of Population Employed in Agriculture") +
  theme_minimal() 



  

#scale_color_gradient(low = "red", high = "darkblue", name = "PCGDP") +
  

# Display distribution of predictors


#distributions of independent variables

install.packages('hrbrthemes')
library(hrbrthemes)

#urban supporters
ggurban_sup <- ggplot(urban_support, aes(Urbansup, country_name, color = Urbansup)) +
  geom_point()+
  theme_minimal() +
  theme(axis.text.y=element_blank(),  
        axis.ticks.y=element_blank())
ggurban_sup + scale_color_viridis_c(name = "Years") +
  ylab("Regimes With Urban Supporters") +
  xlab("Urban Supported Years")

ggsave("urban_sup.png", plot = last_plot(), dpi = 150)

#rural supporters
ggrural_sup <- ggplot(rural_support, aes(Ruralsup, country_name, color = Ruralsup)) +
  geom_point()+
  theme_minimal() +
  theme(axis.text.y=element_blank(),  
        axis.ticks.y=element_blank())
ggrural_sup + scale_color_viridis_c(name = "Years") +
  ylab("Regimes With Rural Supporters") +
  xlab("Rural Supported Years")

ggsave("rural_sup.png", plot = last_plot(), dpi = 150)

#business elites
ggbusi_sup <- ggplot(business_elites, aes(Busielite_mimp, country_name, color = Busielite_mimp)) +
  geom_point()+
  theme_minimal() +
  theme(axis.text.y=element_blank(),  
        axis.ticks.y=element_blank())
ggbusi_sup + scale_color_viridis_c(name = "Years") +
  ylab("Regimes Dominated by Business Elites") +
  xlab("Business Elite Years")

ggsave("busi_sup.png", plot = last_plot(), dpi = 150)

  
#agrarian elites
ggagr_sup <- ggplot(agrarian_elites, aes(Agrelite_mimp, country_name, color = Agrelite_mimp)) +
  geom_point()+
  theme_minimal() +
  theme(axis.text.y=element_blank(),  
        axis.ticks.y=element_blank())
ggagr_sup + scale_color_viridis_c(name = "Years") +
  ylab("Regimes Dominated by Agrarian Elites") +
  xlab("Agrarian Elite Years")

ggsave("agr_sup.png", plot = last_plot(), dpi = 150)


#barplots

#urban
urban_bp <- select(country_new, c("year", "Urbansup"))  
urban_bp <- urban_bp %>%
  group_by(year) %>%
  summarize(Urbansup = sum(Urbansup))
ggplot(urban_bp) +
  geom_bar(aes(year, Urbansup, fill = Urbansup), stat='identity') + 
  scale_fill_continuous(type = "viridis", name = "Years") +
  theme_minimal() +
  ylab("Urban Supporters in Coalition (Count)") +
  xlab("Year") 

ggsave("urban_time.png", plot = last_plot(), dpi = 200)


#business elites
busi <- select(country_new, c("year", "Busielite_mimp"))  
busi <- busi %>%
group_by(year) %>%
    summarize(Busielite_mimp = sum(Busielite_mimp))
ggplot(busi) +
  geom_bar(aes(year, Busielite_mimp, fill = Busielite_mimp), stat='identity') + 
  scale_fill_continuous(type = "viridis", name = "Years") +
  theme_minimal() +
  ylab("Business Elites Dominate Coalition (Count)") +
  xlab("Year") 

ggsave("busi_time.png", plot = last_plot(), dpi = 200)


#rural 
rural_bp <- select(country_new, c("year", "Ruralsup"))  
rural_bp <- rural_bp %>%
  group_by(year) %>%
  summarize(Ruralsup = sum(Ruralsup))
ggplot(rural_bp) +
  geom_bar(aes(year, Ruralsup, fill = Ruralsup), stat='identity') + 
  scale_fill_continuous(type = "viridis", name = "Years") +
  theme_minimal() +
  ylab("Rural Supporters in Coalition (Count)") +
  xlab("Year") 

ggsave("rural_time.png", plot = last_plot(), dpi = 200)


#agrarian elites 
agr_bp <- select(country_new, c("year", "Agrelite_mimp"))  
agr_bp <- agr_bp %>%
  group_by(year) %>%
  summarize(Agrelite_mimp = sum(Agrelite_mimp))
ggplot(agr_bp) +
  geom_bar(aes(year, Agrelite_mimp, fill = Agrelite_mimp), stat='identity') + 
  scale_fill_continuous(type = "viridis", name = "Years") +
  theme_minimal() +
  ylab("Agrarian Elites Dominate Coalition (Count)") +
  xlab("Year") 

ggsave("agr_time.png", plot = last_plot(), dpi = 200)




#barplot democracy
library(ggplot2)

dem_bp <- select(country_new, c("year", "democracy_BX"))  
dem_bp <- dem_bp %>%
  group_by(year) %>%
  summarize(democracy_BX = sum(democracy_BX))
ggplot(dem_bp) +
  geom_bar(aes(year, democracy_BX, fill = democracy_BX), stat='identity') + 
  scale_color_gradient(low = "red", high = "darkblue", name = "Democracy (BMR)") +
  theme_minimal() +
  ylab("Democracy (Count)") +
  xlab("Year") 

ggsave("agr_time.png", plot = last_plot(), dpi = 200)


#MARGINAL EFFECTS

install.packages("marginaleffects")






  

