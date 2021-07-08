# script description #
# script:  variables - pca projection


# preparate r -------------------------------------------------------------
# memory
rm(list = ls())




# packages
library(factoextra)
library(FactoMineR)
library(raster)
library(RStoolbox)
library(tidyverse)
library(viridis)

# source
# setar diretório do "pca_function.R"
source("pca_function.R")

# directory
path <- "./data/rasters/chelsa/"
setwd(path)
dir()

# import ------------------------------------------------------------------
# list variables
tif <- dir(pattern = ".tif$")
tif

# tif presente
tif_pre <- tif %>%
  stringr::str_subset("present")
tif_pre

# tif future
tif_fut <- tif %>%
  stringr::str_subset("mpi")
tif_fut

# import rasters present
var_pre <- raster::stack(tif_pre)
var_pre



# import rasters future
var_fut <- raster::stack(tif_fut)
var_fut

# aogcms list
gcm <- stringr::str_split(tif_pre, "_", simplify = TRUE)[, 1] %>% unique


# pca ---------------------------------------------------------------------
# directory
dir.create("pca")
setwd("pca")
dir.create("pres")
dir.create("proj")

# for
for(i in gcm){

  # information
  print(i)

  # aogcm raster present
  var_pre_gcm <- var_pre[[grep(pattern = i, names(var_pre), value = TRUE)]]

  # aogcm raster future
  var_fut_gcm <- var_fut[[grep(pattern = i, names(var_fut), value = TRUE)]]

  for(j in c("mpi126", "mpi585")){

    # aogcm raster future
    var_fut_gcm_ssp <- var_fut_gcm[[grep(pattern = j, names(var_fut_gcm), value = TRUE)]]

    # function
    var_pca_proj(raster_pres = var_pre_gcm,
                 raster_proj = var_fut_gcm_ssp,
                 cum_sum = .95,
                 graphics = TRUE,
                 graphic_names = i,
                 graphic_var_pres_names = c(paste0("bio0", 1:9), paste0("bio", 10:28)),
                 prefix_pca_raster_pres = names(var_pre_gcm[[1]]) %>% sub("_bio01", "", .),
                 prefix_pca_raster_proj = names(var_fut_gcm_ssp[[1]]) %>% sub("_bio01", "", .),
                 path_output_pca_pres = "./pres",
                 path_output_pca_proj = "./proj")

  }

}


# end ---------------------------------------------------------------------
