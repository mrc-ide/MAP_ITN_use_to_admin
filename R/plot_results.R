library(data.table)
library(tidyverse)
library(ggplot2)
library(viridis)
library(rgdal)
library(raster)
library(ggpubr)

MAP_raster <- raster("data/MAP_raster/ITN_use/ITN_2020_use_mean.tif")

ITN_adm0 <- read.csv("output/ITN_coverage/SSA_ITN_use_adm0.csv")
ITN_adm1 <- read.csv("output/ITN_coverage/SSA_ITN_use_adm1.csv")
ITN_adm2 <- read.csv("output/ITN_coverage/SSA_ITN_use_adm2.csv")

ITN_list <- list(ITN_adm0, ITN_adm1, ITN_adm2)

shp_adm0 <- readOGR("data/shp/SSA_shapefile_adm0.shp")
shp_adm1 <- readOGR("data/shp/SSA_shapefile_adm1.shp")
shp_adm2 <- readOGR("data/shp/SSA_shapefile_adm2.shp")

shp_list <- list(shp_adm0, shp_adm1, shp_adm2)

#Set up fortified shapes
shp_data <- sapply(1:3, function(x){
  
  this_shp <- shp_list[[x]]
  this_data <- subset(ITN_list[[x]], year == 2020 & type == "mean")
  this_shp <- this_shp[which(this_shp$GID_0 %in% unique(this_data$GID_0)), ]
  message(paste0("In order? ", all(this_shp$GID_0 == this_data$GID_0)))
  
  this_shp$ITN_cov <- this_data$ITN_coverage_pop_weighted
  shp_fort <- fortify(this_shp)
  this_shp$id <- as.character(row.names(this_shp))
  this_shp_data <- left_join(shp_fort, as.data.frame(this_shp), by = "id")
  
  this_shp_data
  
}, simplify = FALSE)

#MAP data setup
MAP_spdf <- as(MAP_raster, "SpatialPixelsDataFrame")
MAP_df <- as.data.frame(MAP_spdf)
colnames(MAP_df) <- c("value", "x", "y")

#MAP
MAP_plot <- ggplot() +
  geom_tile(data = MAP_df, aes(x = x, y = y, fill = value)) +
  geom_polygon(data = shp_data[[1]], aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  theme_void() +
  coord_map() +
  scale_fill_viridis(option = "B", limits = c(0, 100))

#adm0
adm0_plot <- ggplot(data = shp_data[[1]]) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = ITN_cov * 100)) +
  theme_void() +
  coord_map() +
  labs(fill = "ITN use (%)") +
  scale_fill_viridis(option = "B", limits = c(0, 100))

#adm1
adm1_plot <- ggplot(data = shp_data[[2]]) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = ITN_cov * 100)) +
  theme_void() +
  coord_map() +
  labs(fill = "ITN use (%)") +
  scale_fill_viridis(option = "B", limits = c(0, 100))

#adm2
adm2_plot <- ggplot(data = shp_data[[3]]) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = ITN_cov * 100)) +
  theme_void() +
  coord_map() +
  labs(fill = "ITN use (%)") +
  scale_fill_viridis(option = "B", limits = c(0, 100))

arranged_admin_plot <- ggarrange(MAP_plot, ggarrange(adm0_plot, adm1_plot, adm2_plot, ncol = 1, common.legend = T, legend = "right"), ncol = 2)

ggsave(filename = "figs/arranged_admin_plot_compare.png", arranged_admin_plot, height = 9, width = 10, units = "in")











