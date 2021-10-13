# ISO = run_these[189, ]$ISO
# admin_level = run_these[189, ]$admin_level
# year = run_these[189, ]$year
# type = run_these[189, ]$type
# data_type = run_these[189, ]$data_type




MAP_data_to_admin <- function(ISO, admin_level, year, type, data_type){
  
  #Load and subset shapefile
  SSA_shapefile <- readOGR(paste0("data/shp/SSA_shapefile_", admin_level, ".shp"), encoding = "UTF-8")
  country_shapefile <- SSA_shapefile[which(SSA_shapefile$GID_0 %in% if(any(grepl(";", ISO))) strsplit(ISO, ";")[[1]] else ISO), ]
  
  message("Shapefile loaded")
  
  #Set up and load MAP data
  data_list <- list.files(paste0("data/MAP_raster/", type, "/"), pattern = as.character(year), full.names = T)
  data <- data_list[grepl(data_type, data_list)]
  MAP_data_loaded <- raster(data)
  MAP_data_country <- rasterize(country_shapefile, crop(MAP_data_loaded, extent(country_shapefile)), mask = T)
  
  message("MAP data loaded")
  
  #Load population data and subset
  population_all <- raster(list.files("data/population/", pattern = as.character(year), full.names = T))
  population_country <- rasterize(country_shapefile, crop(population_all, extent(country_shapefile)), mask = T)
  #Aggregate population data to the same res as the MAP data
  population_country_agg <- aggregate(population_country, fact = res(MAP_data_country)/res(population_country), fun = sum, na.rm = T)
  #Have to go the long way round to create population raster with an identical extent/res and conserve all the population
  template <- raster(extent(population_country_agg), crs = crs(population_country_agg), resolution = res(MAP_data_loaded))
  pts <- as(population_country, "SpatialPoints")
  vals <- raster::extract(population_country, pts)
  pts2 <- SpatialPointsDataFrame(pts, data.frame(vals))
  population_country_agg_resampled <- rasterize(pts2, template, field = "vals", fun = sum, na.rm = T)
  
  message("Population data loaded and processed")
  
  #Now update MAP data country to the same extent
  #Have to go the long way round to create population raster with an identical extent/res and conserve all the population
  template <- raster(extent(population_country_agg_resampled), crs = crs(population_country_agg_resampled), resolution = res(population_country_agg_resampled))
  pts <- as(MAP_data_country, "SpatialPoints")
  vals <- raster::extract(MAP_data_country, pts)
  pts2 <- SpatialPointsDataFrame(pts, data.frame(vals))
  MAP_data_country_resampled <- rasterize(pts2, template, field = "vals", fun = sum, na.rm = T)
  
  message("MAP data processed")
  
  #Now we're ready to go - loop by admin unit to make it easier to debug/overall faster
  all_processed <- as.data.frame(rbindlist(sapply(1:nrow(country_shapefile), function(x){
    
    message(paste0(x, " of ", nrow(country_shapefile)))
    
    #Subset to admin and extract data for dataframe
    this_admin <- country_shapefile[x, ]
    these_cols <- as.data.frame(this_admin[, grepl("GID_0|NAME_|ID_", names(this_admin))])
    these_cols <- these_cols[-which(grepl("VARNAME|NL_NAME", colnames(these_cols)))]
    
    #Work out ITN coverage
    ITN_admin <- rasterize(this_admin, crop(MAP_data_country_resampled, extent(this_admin)), mask = T)
    pop_admin <- rasterize(this_admin, crop(population_country_agg_resampled, extent(this_admin)), mask = T)
    
    data.frame(these_cols,
               population = sum(pop_admin[], na.rm = T),
               ITN_coverage_mean = mean(ITN_admin[], na.rm = T),
               ITN_coverage_pop_weighted = sum((ITN_admin * pop_admin)[], na.rm = T)/sum(pop_admin[], na.rm = T))
    
  }, simplify = FALSE)))
  
  #Set up figure
  country_shapefile$ITN_coverage_pop_weighted <- all_processed$ITN_coverage_mean
  country_fort <- fortify(country_shapefile)
  country_shapefile$id <- row.names(country_shapefile)
  country_fort_data <- left_join(country_fort, as.data.frame(country_shapefile), by = "id")
  
  ITN_map <- ggplot(data = country_fort_data) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = ITN_coverage_pop_weighted * 100),
                 color = "black") +
    theme_void() +
    coord_map() +
    scale_fill_viridis_b(limits = c(0, 100)) +
    labs(fill = "ITN coverage (%)")
  
  #Set up save location
  save_dir <- paste0("output/ITN_coverage/", admin_level, "/", gsub(";", "", ISO), "/")
  if(!dir.exists(paste0(save_dir, "figs/"))) dir.create(paste0(save_dir, "figs/"), recursive = T)
  
  write.csv(x = all_processed,
            file = paste0(save_dir, paste(c(admin_level, year, type, data_type), collapse = "_"), ".csv"),
            row.names = FALSE)
  
  ggsave(filename = paste0(paste0(save_dir, "figs/"), paste(c(admin_level, year, type, data_type), collapse = "_"), ".png"), 
         plot = ITN_map, 
         width = 4,
         height = 4,
         dpi = 100,
         units = "in")
  
}
