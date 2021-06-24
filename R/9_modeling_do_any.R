###########################################
# MODLER                                  #
# Do Any                                  #
# Tree species                            #
#
###########################################

# It is necessary to run the script #8 Cleaning for sdm_data function
# List of species is also in script #8


######################### MODELAGEM DO_ANY ####################################

args(do_any)
?do_any

modelos <- "./modelos/modelos_gualaxo" # file to save models

sp <- species[15] #where number indicates the order or the species which we want to run

do_any(species_name = sp,
       models_dir = modelos,
       predictors = clim.stack,
       algorithm = "maxnet",
       png_partitions = TRUE,
       write_bin_cut = TRUE,
       equalize = TRUE)

#You can explore the list of files created at this phase, for example

partitions.folder <-list.files(modelos, recursive = T,
             pattern = "partitions",
             include.dirs = T, full.names = T)
partitions.folder
list.files(partitions.folder, recursive = T)

####################### Joining partitions: final_model() #####################

args(final_model)
?final_model

final_model(species_name = species[48],
            algorithms = NULL, #if null it will take all the in-disk algorithms
            models_dir = modelos,
            #select_partitions = TRUE,
            select_par = "TSS",
            select_par_val = 0,
            which_models = c("raw_mean", "bin_consensus"),
            consensus_level = 0.5,
            uncertainty = T,
            overwrite = T)

#We can explore these models from the files:

final.folder <- list.files(modelos,
                           recursive = T,
                           pattern = "final_models",
                           include.dirs = T,
                           full.names = T)
final.folder
final_mods <- list.files(final.folder, full.names = T,
                         pattern = "raw_mean.+tif$", recursive = T)
final_mods

