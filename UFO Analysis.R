library(readr)
library(tidytext)
library(tidyverse)
library(dplyr)
library(sqldf)
library(tigris)
library(sf)
library(httr)
library(jsonlite)
library(lubridate)

#GET DATA
ufo_sightings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')

#GET US SIGHTINGS
us_ufo <- subset(ufo_sightings, ufo_sightings$country_code=='US')
us_ufo_split <- split(us_ufo, us_ufo$shape)

#GET SIGHTINGS WHERE SHAPE IS NOT LISTED
us_ufo_na <- subset(us_ufo, is.na(us_ufo$shape))
na_words <- us_ufo_na %>% unnest_tokens(word, summary)

#IDENTIFY SHAPE FOR MISSING VALUES USING WORDS IN THE SUMMARY
identified <- subset(na_words, na_words$word==names(us_ufo_split)[1])
identified$shape <- "changing"

for (i in 1:length(names(us_ufo_split))) {
  id <- subset(na_words, na_words$word==names(us_ufo_split)[i])
  id$shape <- names(us_ufo_split)[i]
  identified <- rbind(identified, id)
}

identified <- identified[,c(1,4,5,7)]

#ADD MISSING SHAPES
us_ufo_id <- sqldf("SELECT a.*, b.shape as shape_id
                     FROM us_ufo a LEFT JOIN identified b
                      ON a.reported_date_time=b.reported_date_time and a.city=b.city and a.state=b.state")
  
us_ufo_id$shape <- ifelse(is.na(us_ufo_id$shape)==TRUE, us_ufo_id$shape_id, us_ufo_id$shape)
us_ufo_id <- us_ufo_id[is.na(us_ufo_id$shape)==FALSE, c(1,4,5,7,9)]

#ADD LATITUDE AND LONGITUDE
ufo <- sqldf("SELECT a.*, b.latitude, b.longitude
               FROM us_ufo_id a, places b
                WHERE a.city=b.city and a.state=b.state") 
ufo <- ufo %>% st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% shift_geometry(position = "outside")

#GROUP SHAPES AND KEEP ONLY LARGE GROUPS
ufo$shape_grouped <- ifelse(ufo$shape %in% c('circle','sphere','oval','orb'), 'round', 
                            ifelse(ufo$shape %in% c('cigar','cylinder'), 'cylinder', 
                                   ifelse(ufo$shape %in% c('light','fireball'), 'light', 
                                          ifelse(ufo$shape %in% c('triangle','disk'), ufo$shape, NA))))

ufo <- subset(ufo, is.na(ufo$shape_grouped)==FALSE)
ufo <- ufo[order(ufo$shape_grouped, decreasing=TRUE),]
ufo$state <- toupper(ufo$state)
ufo$city <- toupper(ufo$city)

#GET SOME NUMBERS FOR THE GRAPHIC
min(ufo$reported_date_time)
max(ufo$reported_date_time)
year_count <- ufo %>% group_by(year(ufo$reported_date_time)) %>% count()
year_count[year_count$n==max(year_count$n),]
shape_count <- ufo %>% group_by(shape_grouped) %>% count()
shape_count <- shape_count[order(-shape_count$n),]

aa <- subset(ufo, ufo$state=='FL') #CHANGE STATE HERE
aa$yr <- year(aa$reported_date_time)
c <- aa %>% group_by(yr) %>% count()
c <- c[order(-c$n),]
max(aa$reported_date_time)

s <- ufo %>% group_by(shape_grouped) %>% count()
s <- s[order(-s$n),]

#GET US MAP SHAPEFILE
us_map <- states(cb=TRUE, resolution = "20m") %>% shift_geometry(position = "outside")
us_map <- us_map[-52,]

map_colors <- c(
  "#d7191c",
  "#fdae61",
  "lightyellow",
  "#48b01b",
  "darkgreen")

area51 <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(area51) <- c("longitude", "latitude")
area51$longitude <- -115.7930
area51$latitude <- 37.2431
area51 <- area51 %>% st_as_sf(coords=c("longitude", "latitude"), crs=4326)

ufo_count <- ufo %>% group_by(city, state, shape_grouped) %>% count()

mc <- data.frame(matrix(ncol=4, nrow=nrow(ufo_count)))
mc$X1 <- ufo_count$city
mc$X2 <- ufo_count$state
mc$X3 <- ufo_count$n
mc$X4 <- ufo_count$shape_grouped
colnames(mc) <- c("city","state","sightings","shape_grouped")

m2 <- merge(ufo, mc, by=c("city","state","shape_grouped"), all.x=FALSE)
m2u <- distinct(m2, city, state, shape_grouped, .keep_all = TRUE)
m2u$sightings <- m2u$sightings/10
m2u <- m2u[order(-m2u$sightings),]

#MAKE US MAP
ggplot() +
  geom_sf(data=us_map, fill=NA, color="black", size=0.1) +
  geom_sf(data=m2u, shape=16, size=m2u$sightings, aes(colour=shape_grouped)) +
  geom_sf(data=area51, shape=24, color="white", fill="blue", alpha=1, size=2) +
  geom_sf_label(data=area51, label="Area 51", color="blue", fill=NA, size=2, label.size=NA,
                nudge_y=60000, fontface="bold") +
  scale_color_manual(values=map_colors) +
  guides(color=guide_legend(override.aes=list(size=2)))  +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks=element_blank(),
    panel.background=element_rect(fill = "gray85"),
    panel.grid.major=element_line(color="gray85"),
    legend.text=element_text(face="bold", size=8),
    legend.title=element_text(face="bold", size=8))

####################################################################################################################
#GET STATES WITH MOST SIGHTINGS
state_counts <- ufo %>% group_by(state) %>% count()
state_counts <- state_counts[order(state_counts$n, decreasing=TRUE),]

state_counts2 <- ufo %>% group_by(state, shape_grouped) %>% count()
state_counts2 <- state_counts2[order(state_counts2$state, -state_counts2$n),]

#FUNCTION TO GET AND MAP STATE DATA
state_map <- function (stateABBR) {
  #SUBSET THE DATA FOR THE STATE
  m <- subset(ufo, ufo$state==stateABBR)
  m_count <- m %>% group_by(city, shape_grouped) %>% count()
  
  mc <- data.frame(matrix(ncol=3, nrow=nrow(m_count)))
  mc$X1 <- m_count$city
  mc$X2 <- m_count$n
  mc$X3 <- m_count$shape_grouped
  colnames(mc) <- c("city","sightings","shape_grouped")
  
  m2 <- merge(m, mc, by=c("city","shape_grouped"), all.x=FALSE)
  m2u <- distinct(m2, city, shape_grouped, .keep_all = TRUE)
  m2u$sightings <- m2u$sightings/2
  m2u <- m2u[order(-m2u$sightings),]
  
  #GET STATE MAP SHAPEFILE
  map_sp <- counties(stateABBR, cb=TRUE)
  #REMOVE COUNTY BORDERS
  map_sp <- map_sp %>% st_union()
  
  #GET AF BASE LOCATION DATA
  statename <- state.name[match(stateABBR, state.abb)]
  statename <- gsub(" ", "%20", statename) 
  
  af_api <- GET(paste0("https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/military-bases/records?where=state_terr%3D%27",
                       statename,
                       "%27%20and%20oper_stat%3D%27Active%27&limit=99&refine=component%3A%22AF%20Active%22"))
  
  af <- as.data.frame(fromJSON(rawToChar(af_api$content), simplifyDataFrame=TRUE))
  names(af) <- sub('^results.', '', names(af))
  
  af2 <- data.frame(matrix(ncol=3, nrow=nrow(af)))
  af2$X1 <- af$geo_point_2d$lon
  af2$X2 <- af$geo_point_2d$lat
  af2$X3 <- af$site_name
  af2 <- af2 %>% st_as_sf(coords=c("X1","X2"), crs=4326)
  
  #MAKE STATE MAP
  ggplot() +
    geom_sf(data=map_sp, fill=NA, color="black", size=0.1) +
    geom_sf(data=m2u, shape=16, size=m2u$sightings, aes(colour=shape_grouped)) +
    geom_sf(data=af2, shape=24, size=4, color="white", fill="blue", alpha=1) +
    scale_color_manual(values=map_colors) +
    guides(color=guide_legend(override.aes=list(size=10))) +
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.ticks=element_blank(),
      panel.background=element_rect(fill = "gray85"),
      panel.grid.major=element_line(color="gray85"),
      legend.text=element_text(face="bold", size=8),
      legend.title=element_text(face="bold", size=8))
}

#CA - 7988
state_map("CA")

#FL - 4198
state_map("FL")

#WA - 3496
state_map("WA")

#TX - 2859
#state_map("TX")

#NY - 2759
#state_map("NY")
