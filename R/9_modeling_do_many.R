######################################################
# ModleR for several species and several algorithms  #
# Do_many                                    #
# Tree species BHRD                                  #
# Tutorial: https://github.com/Model-R/modleR        #
######################################################




# It is necessary to run the script #8 Cleaning for sdm_data function


######################### MODEL DO MANY ####################################

library(modleR)

#Fitting a model per partition: do_many()
args(do_many)
?do_many

#Let's also measure how much time the code below will take 
#Start the clock!
start.time <- Sys.time()

for (i in 1:length(data_list)) {
  sp <- species[i]
  do_many(species_name = sp,
          predictors = clim.stack,
          models_dir = "./modelos/modelos_gualaxo",
          png_partitions = TRUE,
          equalize = TRUE,
          bioclim = TRUE,
          brt = TRUE,
          domain = TRUE,
          glm = TRUE,
          maxnet = TRUE,
          mahal = TRUE,
          svmk = TRUE,
          rf = TRUE,
          write_bin_cut = TRUE
          #project_model = TRUE,
          #proj_data_folder = "./data/raster/proj"
          )
}

#Stop the clock!
end.time <- Sys.time()

time.elapsed <- end.time - start.time
time.elapsed

####################### Joining partitions: final_model() #####################

args(final_model)
?final_model

#Let's also measure how much time the code below will take 
#Start the clock!
start.time <- Sys.time()

for (i in 1:length(data_list)) {
  sp <- species[i]
  final_model(species_name = sp,
              models_dir = "./modelos/modelos_gualaxo",
              algorithms = NULL,
              mean_th_par = c("spec_sens"),
              consensus_level = 0.5,
              which_models = c("raw_mean",
                               "bin_mean",
                               "bin_consensus"),
              uncertainty = TRUE,
              png_final = TRUE,
              overwrite = TRUE
              )
  final_model(species_name = sp,
              models_dir = "./modelos/modelos_gualaxo",
              proj_dir = "future",
              algorithms = NULL,
              mean_th_par = c("spec_sens"),
              consensus_level = 0.5,
              which_models = c("raw_mean",
                               "raw_mean_cut",
                               "raw_mean_th",
                               "bin_mean",
                               "bin_consensus"),
              uncertainty = TRUE,
              png_final = TRUE,
              overwrite = TRUE
              )
}

#Stop the clock!
end.time <- Sys.time()

time.elapsed <- end.time - start.time
time.elapsed

############################# ENSEMBLE ######################################
args(ensemble_model)

#Para criar o concenso dos modelos finais de varios algoritmos
for (i in 1:length(data_list)) {
  sp <- species[i]
  occs <- data_list[[i]]
  ensemble_model(species_name = sp,
                 occurrences = occs,
                 which_final = "bin_consensus",
                 png_ensemble = TRUE,
                 models_dir = "./modelos/loop")
}


#Para criar um mapa dos ensembles
ensemble_files <- list.files(path="~/modleR_test/purrr/Acalypha_villosa/present/ensemble", 
                             ".*.tif$",
                             full.names = TRUE)
ens_mod <- raster::stack(ensemble_files)
names(ens_mod) <- c("average", "uncertainty")
raster::plot(ens_mod)
