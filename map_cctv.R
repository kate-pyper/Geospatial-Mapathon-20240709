#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# RStudio Workbench is strictly for use by Public Health Scotland staff and     
# authorised users only, and is governed by the Acceptable Usage Policy https://github.com/Public-Health-Scotland/R-Resources/blob/master/posit_workbench_acceptable_use_policy.md.
#
# This is a shared resource and is hosted on a pay-as-you-go cloud computing
# platform.  Your usage will incur direct financial cost to Public Health
# Scotland.  As such, please ensure
#
#   1. that this session is appropriately sized with the minimum number of CPUs
#      and memory required for the size and scale of your analysis;
#   2. the code you write in this script is optimal and only writes out the
#      data required, nothing more.
#   3. you close this session when not in use; idle sessions still cost PHS
#      money!
#
# For further guidance, please see https://github.com/Public-Health-Scotland/R-Resources/blob/master/posit_workbench_best_practice_with_r.md.
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(tidyverse)
library(sf)
library(leaflet)

cctv_data <- read_csv("/conf/LIST_analytics/West Hub/Geospatial Cross Team/Team Mapathon/Traffic data/cctv_aggregated_monthly.csv")

cctv_may23 <- cctv_data %>% 
  filter(month == "01/05/2023")

cctv_may24 <- cctv_data %>% 
  filter(month == "01/05/2024")

icons <- awesomeIcons(markerColor = "black",
                      iconColor = "white",
                      icon = "glyphicon-facetime-video")

leaflet() %>% 
  addTiles() %>% 
  addAwesomeMarkers(data = cctv_june24,
                  popup = paste(paste0("<b>", cctv_june24$camera_id)),
                  icon = icons)

pal <- colorNumeric("RdYlGn", c(cctv_may23$cars, cctv_may24$cars), reverse = TRUE)

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = cctv_may23,
                   radius = ~pedestrians/500,
                   color = ~pal(cars),
                   group = "May 2023") %>% 
  addCircleMarkers(data = cctv_may24,
                   radius = ~pedestrians/500,
                   color = ~pal(cars),
                   group = "May 2024") %>% 
  addLegend(pal = pal, position = "bottomleft", values = c(cctv_may23$cars, cctv_may24$cars)) %>% 
  addLayersControl(
  # Groups will show in order they are set here
  baseGroups = c("May 2023", "May 2024"),
  position = "topright",
  # set collapsed = FALSE so that controls always displayed
  options = layersControlOptions(collapsed = FALSE))

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = cctv_may23,
                   radius = ~cars/500,
                   color = ~pal(cars),
                   group = "May 2023") %>% 
  addCircleMarkers(data = cctv_may24,
                   radius = ~cars/500,
                   color = ~pal(cars),
                   group = "May 2024") %>% 
  addLegend(pal = pal, position = "bottomleft", values = c(cctv_may23$cars, cctv_may24$cars)) %>% 
  addLayersControl(
    # Groups will show in order they are set here
    baseGroups = c("May 2023", "May 2024"),
    position = "topright",
    # set collapsed = FALSE so that controls always displayed
    options = layersControlOptions(collapsed = FALSE))

### PART TWO

traffic_data <- read_csv("/conf/LIST_analytics/West Hub/Geospatial Cross Team/Team Mapathon/Traffic data/traffic_new.csv")

traffic_latest <- traffic_data %>% 
  filter(lastupdate == max(lastupdate))

traffic_oneyear <- traffic_data %>% 
  filter(month == "5" & year == "2023")

pal <- colorNumeric("RdYlGn", log(c(traffic_latest$flow_mean, traffic_oneyear$flow_mean)+1), reverse = TRUE)

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = traffic_latest,
                   group = "May 2024",
                   radius = ~flow_mean/5,
                   opacity = 0.8,
                   fillOpacity = 0.8,
                   color = ~pal(log(flow_mean+1))) %>% 
  addCircleMarkers(data = traffic_oneyear,
                   opacity = 0.8,
                   fillOpacity = 0.8,
                   group = "May 2023",
                   radius = ~flow_mean/5,
                   color = ~pal(log(flow_mean+1))) %>% 
  addLegend(pal = pal, position = "bottomleft", values = log(c(traffic_latest$flow_mean, traffic_oneyear$flow_mean)+1)) %>% 
  addLayersControl(
    # Groups will show in order they are set here
    baseGroups = c("May 2023", "May 2024"),
    position = "topright",
    # set collapsed = FALSE so that controls always displayed
    options = layersControlOptions(collapsed = FALSE))





library(ggplot2)
ggplot(traffic_latest, aes(x=flow_average)) + 
  geom_histogram()
