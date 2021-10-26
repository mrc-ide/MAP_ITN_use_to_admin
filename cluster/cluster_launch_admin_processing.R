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

run_these <- expand.grid(ISO = paste(all_ISO[-which(all_ISO$GID_0 %in% c("NAM", "SWZ", "LSO", "ZAF", "BWA")), ]$GID_0, collapse = ";"),
                         admin_level = paste0("adm", 0:2),
                         year = 2000:2020,
                         type = "ITN_use",
                         data_type = c("lower", "mean", "upper"),
                         stringsAsFactors = FALSE)

dim(run_these)

#Launch
grp <- obj$enqueue_bulk(run_these,
                        MAP_data_to_admin, do_call = TRUE, progress = TRUE)


#Now combine
all_datasets <- sapply(c("adm0", "adm1", "adm2"), function(x){
  these <- paste0("U:/Arran/MAP_ITN_use_to_admin/output/ITN_coverage/", x, "/AGOBDIBENBFACAFCIVCMRCODCOGCPVDJIERIETHGABGHAGINGMBGNBGNQKENLBRMDGMLIMOZMRTMUSMWINERNGARWASDNSENSLESOMSSDSTPSYCTCDTGOTZAUGAZMBZWE")
  all_ran <- list.files(these, full.names = T, ".csv")
  rbindlist(sapply(all_ran, function(y){
    this <- read.csv(y)
    this$type <- gsub(".csv", "", last(strsplit(y, "_")[[1]]))
    this$year <- strsplit(y, "_")[[1]][7]
    this
  }, simplify = FALSE))
}, simplify = FALSE)

write.csv(all_datasets[[1]], file = "output/ITN_coverage/SSA_ITN_use_adm0.csv", row.names = FALSE)
write.csv(all_datasets[[2]], file = "output/ITN_coverage/SSA_ITN_use_adm1.csv", row.names = FALSE)
write.csv(all_datasets[[3]], file = "output/ITN_coverage/SSA_ITN_use_adm2.csv", row.names = FALSE)











