######################################################
# ModleR for several species and several algorithms  #
# Step 8: Do_many                                    #
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
          bioclim = TRUE,
          mahal = TRUE,
          maxnet = FALSE,
          maxent = TRUE,
          rf = TRUE,
          svmk = TRUE,
          svme = FALSE,
          brt = TRUE,
          glm = FALSE,
          domain = FALSE,
          equalize = TRUE,
          write_bin_cut = TRUE)
}

#Stop the clock!
end.time <- Sys.time()

time.elapsed <- end.time - start.time

####################### Joining partitions: final_model() #####################

args(final_model)

for (i in 1:length(data_list)) {
  sp <- species[i]
  final_model(species_name = sp,
              consensus_level = 0.5,
              models_dir = "./modelos/loop",
              which_models = c("raw_mean",
                               "bin_mean",
                               "bin_consensus"),
              uncertainty = TRUE,
              overwrite = TRUE)
}



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
