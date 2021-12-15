# ISO = run_these[1, ]$ISO
# admin_level = run_these[1, ]$admin_level
# year = run_these[1, ]$year
# type = run_these[1, ]$type
# data_type = run_these[1, ]$data_type




MAP_data_to_admin <- function(ISO, admin_level, year, type, data_type){
  
  #Load and subset shapefile
  SSA_shapefile <- readOGR(paste0("data/shp/SSA_shapefile_", admin_level, ".shp"), encoding = "UTF-8")
  country_shapefile <- SSA_shapefile[which(SSA_shapefile$GID_0 %in% if(any(grepl(";", ISO))) strsplit(ISO, ";")[[1]] else ISO), ]
  
  message("Shapefile loaded")
  
  #Set up and load MAP data
  year_max <- ifelse(type == "ITN_use", 2020,
                     ifelse(type == "IRS_use", 2017, 2019))
  
  pattern_find <- ifelse(type == "relative_abundance", ".tif", 
                         ifelse(type == "ITN_use",
                                paste0("ITN_", year_max, "_use_", data_type),
                                paste0(as.character(pmin(year, year_max)), ".tif")))
  
  data <- list.files(paste0("data/MAP_raster/", type, "/"), pattern = pattern_find, full.names = T)
  
  MAP_data_loaded <- if(type == "relative_abundance") stack(sapply(1:3, function(a) raster(data, band = a), simplify = FALSE)) else raster(data)
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
  MAP_data_country_resampled <- if(type == "relative_abundance"){
    stack(sapply(1:3, function(a){
      cut_point <- pts2
      cut_point@data <- data.frame(cut_point@data[, a])
      names(cut_point@data) <- "vals"
      rasterize(cut_point, template, field = "vals", fun = sum, na.rm = T)
    }, simplify = FALSE))
  } else {
    rasterize(pts2, template, field = "vals", fun = sum, na.rm = T)
  }
   
  message("MAP data processed")
  
  #Now we're ready to go - loop by admin unit to make it easier to debug/overall faster
  all_processed <- as.data.frame(rbindlist(sapply(1:nrow(country_shapefile), function(x){
    
    message(paste0(x, " of ", nrow(country_shapefile)))
    
    #Subset to admin and extract data for dataframe
    this_admin <- country_shapefile[x, ]
    these_cols <- as.data.frame(this_admin[, grepl("GID_0|NAME_|ID_", names(this_admin))])
    if(any(grepl("VARNAME|NL_NAME", names(these_cols)))) these_cols <- these_cols[-which(grepl("VARNAME|NL_NAME", names(these_cols)))]
    
    #Work out ITN coverage
    ITN_admin <- rasterize(this_admin, crop(MAP_data_country_resampled, extent(this_admin)), mask = T)
    pop_admin <- rasterize(this_admin, crop(population_country_agg_resampled, extent(this_admin)), mask = T)
    
    if(type == "relative_abundance"){
      
      relative_abundance_admin <- rasterize(this_admin, crop(MAP_data_country_resampled, extent(this_admin)), mask = T)@data@values
      relative_abundance_admin_pop <- relative_abundance_admin * pop_admin[]
      
      relative_abundance_all <- colSums(relative_abundance_admin, na.rm = T)/sum(colSums(relative_abundance_admin, na.rm = T))
      relative_abundance_all_pop <- colSums(relative_abundance_admin_pop, na.rm = T)/sum(colSums(relative_abundance_admin_pop, na.rm = T))
      
      names(relative_abundance_all) <- paste0(c("arab", "fun", "gamb"), "_abundance")
      names(relative_abundance_all_pop) <- paste0(c("arab", "fun", "gamb"), "_pop_weighted_abundance")
      
      data.frame(these_cols,
                 population = sum(pop_admin[], na.rm = T),
                 t(relative_abundance_all),
                 t(relative_abundance_all_pop))
    } else {
      data.frame(these_cols,
                 population = sum(pop_admin[], na.rm = T),
                 coverage_mean = mean(ITN_admin[], na.rm = T),
                 coverage_pop_weighted = sum((ITN_admin * pop_admin)[], na.rm = T)/sum(pop_admin[], na.rm = T))
    }
    
  }, simplify = FALSE)))
  
  all_processed$year <- year
  
  #Set up save location
  save_dir <- paste0("output/", type, "/", admin_level, "/", gsub(";", "", ISO), "/")
  if(!dir.exists(paste0(save_dir, "figs/"))) dir.create(paste0(save_dir, "figs/"), recursive = T)
  
  write.csv(x = all_processed,
            file = paste0(save_dir, paste(c(admin_level, year, type, data_type), collapse = "_"), ".csv"),
            row.names = FALSE)
  
}
