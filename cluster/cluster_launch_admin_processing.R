# rm(list = ls(all = TRUE))

drat:::add("mrc-ide")

## information on my computer
malaria <- didehpc::path_mapping("Malaria", "U:", "//fi--didef3.dide.ic.ac.uk/Malaria", "U:")

config <- didehpc::didehpc_config(shares = malaria, use_rrq =  FALSE, 
                                  cluster = "fi--didemrchnb", cores = 1,
                                  template = "20Core",
                                  parallel = FALSE)

packages <- c("raster", "rgdal", "rgeos", "ggplot2", "tidyverse", "data.table")


sources = c("cluster/cluster_source_all.R")

src <- conan::conan_sources(NULL, repos = "https://mrc-ide.github.io/")
ctx <- context::context_save(path = "new_contexts", packages = packages, sources = sources,
                             package_sources = src)

obj = didehpc::queue_didehpc(ctx, config = config)

#Set up
all_ISO <- readOGR("data/shp/SSA_shapefile_adm0.shp")
UN_geo_region <- read.csv("data/shp/UN_geoscheme_regions") #This is just a way of chunking the runs
africa_intermediate_region <- unique(UN_geo_region$intermediate.region[which(UN_geo_region$region == "Africa")])

SSA_africa <- sapply(africa_intermediate_region[-which(africa_intermediate_region == "")], function(x) paste(UN_geo_region[which(UN_geo_region$intermediate.region == x),]$alpha.3, collapse = ";"))
Northern_africa <- paste(UN_geo_region[which(UN_geo_region$sub.region == "Northern Africa"), ]$alpha.3, collapse = ";")

run_these <- expand.grid(ISO = as.character(sapply(split(all_ISO$GID_0, ceiling(seq_along(all_ISO$GID_0)/2)), function(x) paste(x, collapse = ";"))),
                         admin_level = paste0("adm", 1:2),
                         year = 2020,
                         type = "relative_abundance",
                         data_type = "mean",#c("lower", "mean", "upper"),
                         stringsAsFactors = FALSE)

dim(run_these)

#Launch
grp <- obj$enqueue_bulk(run_these,
                        MAP_data_to_admin, do_call = TRUE, progress = TRUE)

#Now combine
all_datasets <- sapply(c("ITN_use", "IRS_use", "prevalence", "treatment", "relative_abundance"), function(a){
  sapply(c("adm1", "adm2"), function(x){
    these <- paste0("U:/Arran/MAP_ITN_use_to_admin/output/", a, "/", x, "/")
    all_ran <- list.files(these, full.names = T, ".csv", recursive = T)
    df <- rbindlist(sapply(all_ran, function(y){
      this <- read.csv(y)
      this$type <- gsub(".csv", "", last(strsplit(y, "_")[[1]]))
      this$year <- (as.numeric(strsplit(y, "_")[[1]]))[which(!is.na(as.numeric(strsplit(y, "_")[[1]])))]
      this
    }, simplify = FALSE), use.names = TRUE)
    colnames(df) <- gsub("ITN_", "", colnames(df))
    write.csv(df, file = paste0("output/", a, "/SSA_", a, "_", x, ".csv"), row.names = FALSE)
  }, simplify = FALSE)
})









