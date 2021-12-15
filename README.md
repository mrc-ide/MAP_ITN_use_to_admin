# MAP_raster_to_admin
Repo for converting MAP ITN use (or other raster data) to admin data

All of the work is done by the function "MAP_data_to_admin" (R/functions) which loads the specified MAP raster data and calculates population weighted (and unweighted) values of the raster in question for each administrative unit
Code should be fairly self explanatory, to launch on the cluster the file is located in "cluster", though changes will need to be made in order to run on your specific log in.
