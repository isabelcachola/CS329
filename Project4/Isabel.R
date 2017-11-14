require(MASS)
require(ISLR)
require(tidyverse)
library(ggplot2)
library(dplyr)

project<- "https://data.world/wangweiyi722/f-17-eda-project-4"
df_orig <- read.csv("https://query.data.world/s/tZl4yOP0Ui6RbVBm482-KU67IDetEk", header=TRUE, stringsAsFactors=FALSE)
names(df_orig)
################################# Insght 1 ########################################
library(maps)
library(mapdata)

df_map = subset(df_orig, (df_orig$Title.1.Eligible == "Yes")|(df_orig$Title.1.Eligible == "No"))

### Texas ###
df_map_texas = subset(df_map, df_map$State == "Texas")

states <- map_data("state")
tx_df <- subset(states, region == "texas")
tx_base <- ggplot(data = tx_df, mapping = aes(x = long, y = lat)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "black")

df_map_texas_eligible <- dplyr:: filter(df_map_texas, df_map_texas$Title.1.Eligible=="Yes")
df_map_texas_not_eligible <- subset(df_map_texas, df_map_texas$Title.1.Eligible=="No")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

tx_base +
  geom_point(data = df_map_texas_eligible, mapping = aes(x=Longitude,y=Latitude,colour='Title I Eligible')) +
  geom_point(data = df_map_texas_not_eligible,mapping = aes(x=Longitude,y=Latitude,colour='Not Title I Eligible')) + 
  geom_point(mapping = aes(x=-97.743061,y=30.267153,colour = 'Major Cities'),size = 5,shape = 18)+
  geom_point(mapping = aes(x=-96.796988,y=32.776664,colour = 'Major Cities'),size = 5,shape=18) +
  geom_point(mapping = aes(x=-95.369803,y=29.760427,colour = 'Major Cities'),size = 5,shape=18) +
  geom_point(mapping = aes(x=-98.493628,y=29.424122,colour = 'Major Cities'),size = 5,shape=18) + 
  ditch_the_axes +
  scale_color_brewer(palette="PRGn")


## US ##

usa <- map_data("usa")
us_base <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") + 
  coord_fixed(1.3)

df_map_main_us <- filter(df_map, (df_map$State != "Bureau of Indian Education")&(df_map$State != "Northern Marianas")&(df_map$State != "Puerto Rico")&(df_map$State != "Alaska")&(df_map$State != "Hawaii"))

df_map_main_us$State <- tolower(df_map_main_us$State)

state_eligibilty_perc <-data.frame(state = unique(df_map_main_us$State), perc = rep(0,47))
for (i in 1:47){
  state <- state_eligibilty_perc[i,]$state
  num_el <- nrow(subset(df_map_main_us, (df_map_main_us$State==state)&(df_map_main_us$Title.1.Eligible=="Yes")))
  total <- nrow(subset(df_map_main_us, (df_map_main_us$State==state)))
  percent <- num_el/total
  state_eligibilty_perc[i,]$perc <- percent
}
state_eligibilty_perc <- mutate(state_eligibilty_perc, region = state)
el_perc <- inner_join(state_eligibilty_perc, states, by = "region")

ggplot(data = el_perc) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill=perc), color = "white") + 
  ggtitle("Percentage of Title I Eligibility") +
  theme_bw() +
  ditch_the_axes +
  scale_fill_gradientn(colours = rev(terrain.colors(7)),
                       breaks = c(.14, .28, .42, .56, .70, .84, 1))

el_perc[which.min(el_perc$perc),]$state
el_perc[which.max(el_perc$perc),]$state

