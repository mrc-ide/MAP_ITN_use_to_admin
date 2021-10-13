# rm(list = ls(all = TRUE))

drat:::add("mrc-ide")

## information on my computer
malaria <- didehpc::path_mapping("Malaria", "U:", "//fi--didef3.dide.ic.ac.uk/Malaria", "U:")

config <- didehpc::didehpc_config(shares = malaria, use_rrq =  FALSE, 
                                  cluster = "fi--didemrchnb", cores = 1,
                                  parallel = FALSE, template = "24Core")

packages <- c("raster", "rgdal", "rgeos", "ggplot2", "tidyverse")


sources = c("cluster/cluster_source_all.R")

src <- conan::conan_sources(NULL, repos = "https://mrc-ide.github.io/")
ctx <- context::context_save(path = "new_contexts", packages = packages, sources = sources,
                             package_sources = src)

obj = didehpc::queue_didehpc(ctx, config = config)

#Set up
all_ISO <- readOGR("data/shp/SSA_shapefile_adm0.shp")

run_these <- expand.grid(ISO = all_ISO$GID_0,
                         admin_level = paste0("adm", 0:2),
                         year = 2000:2020,
                         type = "ITN_use",
                         data_type = c("lower", "mean", "upper"),
                         stringsAsFactors = FALSE)

#Launch
grp <- obj$enqueue_bulk(run_these,
                        MAP_data_to_admin, do_call = TRUE, progress = TRUE)

