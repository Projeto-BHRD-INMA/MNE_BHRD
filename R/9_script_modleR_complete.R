
#################################################
# ModleR                                        #
# Cleaning, setup sdm_data and Do many          #
# Species trees occurrences                     #
# Tutorial: https://github.com/Model-R/modleR   #
# Edited by Daphne Spier                        #
# Update: 03 oct 2021                           #
#################################################



# Packages -------------------------------------------------------

library(modleR)
library(dplyr)
library(raster)
library(progress)
library(modleR)
library(foreach)
library(rgeos)
library(doParallel)

getwd()


# 1. clear memory and load packages
# clear workspace and increase memory (Windows)
# rm(list = ls())
# gc()
# memory.limit(size = 1.75e13)

# For Linux
#install.packages("unix") 
library(unix)
rlimit_all()     # check memory
rlimit_as(1e12)  #increases to ~12GB


# LOAD DATA ----------------------------------------------------------------------------


# reading species data, only names
#target_species <- read.csv("./species/lista_species.csv",
#                           stringsAsFactors = FALSE) %>%
#                  pull(sp)


target_species <-c("Annona_crassiflora")

#target_species <-c("Annona_cacans", "Annona_crassiflora")



# reading occurrence table of all species
# in case that your occurrence table have more species than you want to project

occs <- read.csv2("./data/registros/spp_2022/6_recorte2_ModleR.csv", sep=",",
                      header=TRUE,  dec=".") %>%
        filter(spp %in% target_species)


occs <- occs [,2:4]
data_list <- split(occs, occs$spp)
names(data_list)                    #check names
species <- names(data_list)
species


# Selected environmental data (pca axes, Pearson correlation, vif, etc)

wc <- list.files(path="./data/raster/modelagem/pca/pres", 
                        ".*.tif$", full.names = TRUE) %>%
  stack()




# Setup Sdm_data ------------------------------------------------------------------------


# Let's also measure how much time the code below will take 
# Start the clock!
start.time <- Sys.time()

for (i in 1:length(data_list)) {
  sp <- species[i]
  occs <- data_list[[i]]
  setup_sdmdata(species_name = sp,
                models_dir = "./modelos/modelos_recorte2",
                occurrences = occs,
                predictors = wc,
                seed = 123,
                partition_type = "crossvalidation",
                cv_n = 1,
                cv_partitions = 10,
                buffer_type = "mean",
                write_buffer = TRUE,
                png_sdmdata = TRUE,
                n_back = 10000,
                clean_dupl = TRUE,
                clean_uni = TRUE,
                clean_nas = TRUE,
                geo_filt = TRUE,
                geo_filt_dist = 0.1,
                select_variables = FALSE
  )
}

#Stop the clock!
end.time <- Sys.time()
time.elapsed <- end.time - start.time
time.elapsed




# Do many --------------------------------------------------------------------------------


# Start the clock!
start.time <- Sys.time()


for (i in 1:length(data_list)) {
  sp <- species[i]
  do_many(species_name = sp,
          predictors = wc,
          models_dir = "./modelos/modelos_recorte2",
          project_model = TRUE,
          proj_data_folder = "./data/raster/modelagem/pca/proj",
          dismo_threshold = "spec_sens",
          png_partitions = TRUE,
          # bioclim = TRUE, #usar este
          #brt = TRUE,
          #glm = TRUE,
          maxnet = TRUE, #usar este
          # svmk = TRUE, #usar este
          #rf = TRUE,
          #equalize = TRUE,
          write_bin_cut = TRUE)
}

#Stop the clock!
end.time <- Sys.time()
time.elapsed <- end.time - start.time
time.elapsed




# Joining partitions: final_model() ------------------------------------------------------

# Start the clock!
start.time <- Sys.time()

for (i in 1:length(data_list)) {
  sp <- species[i]
  final_model(species_name = sp,
              algorithms = NULL,
              consensus_level = 0.5,
              models_dir = "./modelos/modelos_recorte2",
              mean_th_par = c("spec_sens"),
              which_models = c("raw_mean",
                               "bin_mean",
                               "bin_consensus"),
              proj_dir = "present",
              uncertainty = TRUE,
              png_final = TRUE,
              overwrite = TRUE)


  final_model(species_name = sp,
              algorithms = NULL,
              consensus_level = 0.5,
              models_dir = "./modelos/modelos_recorte2",
              mean_th_par = c("spec_sens"),
              which_models = c("raw_mean",
                               "bin_mean",
                               "bin_consensus"),
              proj_dir = "mpi_126",
              uncertainty = TRUE,
              png_final = TRUE,
              overwrite = TRUE)

  final_model(species_name = sp,
              algorithms = NULL,
              consensus_level = 0.5,
              models_dir = "./modelos/modelos_recorte2",
              mean_th_par = c("spec_sens"),
              which_models = c("raw_mean",
                               "bin_mean",
                               "bin_consensus"),
              proj_dir = "mpi_585",
              uncertainty = TRUE,
              png_final = TRUE,
              overwrite = TRUE)
}

#Stop the clock!
end.time <- Sys.time()
time.elapsed <- end.time - start.time
time.elapsed



# ENSEMBLE ------------------------------------------------------------------------------


# Start the clock!
start.time <- Sys.time()

 for (i in 1:length(data_list)) {
 sp <- species[i]
 occs <- data_list[[i]]
  ensemble_model(species_name = sp,
                 occurrences = occs,
                 models_dir = "./modelos/modelos_recorte2",
                 performance_metric = "TSSmax",
                 which_ensemble = c("weighted_average", "consensus", "best"),
                 which_final = c("raw_mean",
                                 "bin_mean",
                                 "bin_consensus"),
                 consensus_level = 0.5,
                 proj_dir = "present",
                 png_ensemble = TRUE,
                 uncertainty = TRUE,
                 overwrite = TRUE)


  ensemble_model(species_name = sp,
                 occurrences = occs,
                 models_dir = "./modelos/modelos_recorte2",
                 performance_metric = "TSSmax",
                 which_ensemble = c("weighted_average", "consensus", "best"),
                 which_final = c("raw_mean",
                                 "bin_mean",
                                 "bin_consensus"),
                 consensus_level = 0.5,
                 proj_dir = "mpi_126",
                 png_ensemble = TRUE,
                 uncertainty = TRUE,
                 overwrite = TRUE)

  ensemble_model(species_name = sp,
                 occurrences = occs,
                 models_dir = "./modelos/modelos_recorte2",
                 performance_metric = "TSSmax",
                 which_ensemble = c("weighted_average", "consensus", "best"),
                 which_final = c("raw_mean",
                                 "bin_mean",
                                 "bin_consensus"),
                 consensus_level = 0.5,
                 proj_dir = "mpi_585",
                 png_ensemble = TRUE,
                 uncertainty = TRUE,
                 overwrite = TRUE)
}


#Stop the clock!
end.time <- Sys.time()
time.elapsed <- end.time - start.time
time.elapsed
