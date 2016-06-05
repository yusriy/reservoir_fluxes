########################################################
# TITLE: To plot the Ross Barnett reservoir site map
# AUTHOR: Yusri Yusup, PhD
# DATE: 2016-06-05
# VERSION: 1.0
# NOTE: This script downloads and plots the map using
# ggmap and google map
########################################################

####### * Installing and loading required packages ###################################################

#install.packages("ggmap")
#install.packages("gridExtra")
#install.packages("maptools")
#install.packages("rgdal")
#install.packages("raster")

#loading libraries
library(ggmap)
library(mapproj)
library(ggplot2)
library(gridExtra)
library(maptools)
library(rgdal)
library(raster)
library(grid)

#load map tools
source('R/tools/tool_map_createscale.R') # Script to source functions of scales

##### * Zoomed in map of station #####
sitemap <- get_googlemap(center = c(lon = -90.03,lat = 32.43823), sensor=TRUE,
                         size = c(640,640), scale = 2, zoom = 12, maptype = "terrain")

map_plot <- ggmap(sitemap) +
  geom_point(aes_string(x = "-90.03",y = "32.43823"),size = 5,shape=16,colour="black")+
  geom_text(aes_string(x="-90.03",y="32.43823"),label="EC station",colour="black",size=7,
            hjust=0,vjust=-1.00) +
  xlab("") + ylab("") +
  theme(plot.title = element_text(lineheight=1, face="bold",size = 25, colour = "grey20"),
        axis.line=element_blank(),
        panel.border = element_rect(colour="grey20",fill=NA,size=0.5),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()) +
  scaleBar(lon = -90.1, lat = 32.35, distanceLon = 6, distanceLat = 0.5, distanceLegend = 1,
           dist.unit = "km", orientation = FALSE)
map_plot
#Plot site map
png(filename = "figs/reservoir_plot_zoom.png",height=8,width=8,
    bg = "white",units='in', res = 360, family = "")
map_plot
dev.off()
rm(sitemap,map_plot)





