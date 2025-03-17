##### 1.Data ####
load("~/HATUA (Final)/Map.RData")
library(ggplot2)
library(viridis)
library(ggrepel)
library(ggspatial)
library(sf)
##### remember to check file path: getwd()


##### 2. Map plot #########
a <- ggplot() +
  geom_sf(data = world, fill = "grey90") +
  ## Three countries
  geom_sf(data = Geo0_sf, fill = "antiquewhite1") + # Remove boundary, lwd = 0
  ## Lake Victoria
  geom_sf(data = Lake_crs, fill = "lightskyblue2") +
  ## 25°E - 45°E, 15°S - 5°N
  coord_sf(xlim = c(25, 45), ylim = c(-13, 8), expand = FALSE) +
  ## Put nine points on map
  geom_point( data=Nine_Sites,  aes(x=Longitude, y=Latitude, color=per, size=n), shape=20) +
  ## Set colour from dark to light
  scale_color_viridis(discrete = FALSE, option = "D", # viridis
                      direction=-1, name=c("Prevalence of MDR"),
                      begin = 0, end = 1,
                      aesthetics = c("colour", "fill")) + ## Avoid yellow: end = 0.9
  ## Change the size of bubbles
  scale_size_binned(range = c(1,9),
                    name = "Sample size"
                    #,
                    #breaks = c(50,100,150,200,250,300),
                    #labels = c("50", "100", "150", "200","250","300"),
                    #limits = c(0, 300)
  ) +
  ## Map title
  ## ggtitle("The % of MDR", subtitle = "Nine sites in three East-African countries")  +
  ## No xlab/ylab - No Lat/Long Title
  xlab("") + ylab("") +
  ## Make map with dashed grey lines and with blue background
  theme(
    panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
    panel.background = element_rect(fill = "lightskyblue2"), # Only affects the main plot background
    legend.background = element_rect(fill = "white"), # Keeps legend background white
    legend.key = element_rect(fill = "grey90") # Keeps individual legend keys white
  ) +

  #theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",
  #                                      size = 0.5), panel.background = element_rect(fill = "lightskyblue2"),
        #axis.title.x=element_text(size=4),
        #axis.title.y=element_text(size=4)
  #      ) +
  ## Add country label
  geom_label(data = countries, aes(X, Y, label = NAME_0), size = 3.5, fontface = "bold", nudge_y = c(0,1,0.25)) +
  # another way:
  # geom_sf_text(data = countries, aes(X,Y,label=NAME_0), size = 3.5, check_overlap = TRUE, nudge_y = 1) +
  ## Add study site label
  geom_text_repel(data = Nine_Sites, aes(x = Longitude, y = Latitude, label = site2),
                  fontface = "bold", size = 3,
                  nudge_x = c(-0.5, 0.7, 0, 0.4, 0.7, -0.7, -0.7, 0, 0.5),
                  nudge_y = c(0,0,0.3,0,-0.5,0,0,0,0)) +
  ## Add Lake Victoria label
  geom_text_repel(data = Lake_Victoria, aes(X, Y, label = Name), size = 2.5, fontface = "bold", nudge_y = -0.25) +
  ## Add annotation scale and north arrow
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering)

###### 3. save MAP as png #######
png("map.png", width = 8, height = 6, units = "in", res = 300)
plot(a)
dev.off()


###### 4. save MAP as pdf #######
pdf("map.pdf", width = 8, height = 6)  # Set width and height to 8x6 inches
plot(a)
dev.off()
