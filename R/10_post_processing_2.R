###
###  Title: POST-PROCESSING
###
###  Description: code for calculate area gain and loss of species geographic
###    ranges under different future scenarios, export png maps and raster 
###    files.
###
###  Created by: Alan Braz (@brazagm) & Danielle Moreira (@daniomoreira)
###
###  Observations:
###   1. this code is based and only works with ensemble_0.5_consensus models 
###      from ModleR
###   2. this code version includes only present, mpi_126 and mpi_585 as 
###      projections
###
###  Next steps:
###   1. include legend in exported png (maps)
###   2. find a better color palette for maps...
###

library(adehabitatHR)
library(raster)
library(rgeos)
library(rgdal)
library(tidyverse)

#setwd("/home/alan/Documentos/azure_MNE_BHRD/azure")

# 0. INPUTS AND OUTPUTS  ####
## 0.1. Input  ####
# get the output directory from ModleR
path <- "./modelos/gualaxo/modler" 

# get species name and projections from local folders
species <- list.files(path) # './modleR' must contain only species folders!
proj <- list.files(file.path(path, species[1])) # './modleR/<species>' must contain only projection folders!

# get occurrence records
records <- read.csv("./data/registros/spp_Gualaxo/6_Gualaxo_ModleR_out.csv")

# get shapefiles for the Doce riverbasin and world borders
riverbasin <- "./data/shape/bhrd_wgs84_dissolv.shp"
border <- "./data/shape/ne_10m_admin_0_countries.shp"

# get file path for final models and ensemble of each species
input <- list()
for(i in species){
  list <- list()
  
  for(j in proj){
    list <- append(list, list(file.path(path, i, j, "ensemble")))
  }
  
  names(list) <- proj
  input <- append(input, list(list))
}

# set list names by species
names(input) <- species


## 0.2. Output  ####
output <- "./modelos/gualaxo/posproces_th/distribution_area.csv"


# 1. ESTIMATE AREA GAIN AND LOSSES  ####
# ensemble models include only 0.5_consensus!
results <- data.frame(matrix(ncol = 5, nrow = 0))

# calculate range area for each species, in current and future scenarios
# note that this code version includes only c("present", "mpi_126", "mpi_585") as projections


## Para testar para uma ou algumas espécies
#species <- species[36:40] #ou
#i <- species[10]

for(i in species){
  
  # raster manipulation
  raster_present <- list.files(input[[i]]$present, pattern = "_th0.3_.tif", full.names = TRUE) %>%
    stack(.) # raster files for present
  raster_mpi_126 <- list.files(input[[i]]$mpi_126, pattern = "_th0.3_.tif", full.names = TRUE) %>%
    stack(.) # raster files for mpi_126
  raster_mpi_585 <- list.files(input[[i]]$mpi_585, pattern = "_th0.3_.tif", full.names = TRUE) %>%
    stack(.) # raster files for mpi_585
  
  
  ## 1.1. Calculate area gain/loss at the South America extent  ####
  # get sizes of all cells in raster (km2)
  present <- raster_present; present[present == 0] <- NA
  cell_size <- raster::area(present, na.rm = TRUE, weights = FALSE)
  present_area <- sum(values(cell_size), na.rm = TRUE)
  
  future_126 <- raster_mpi_126 ; future_126[future_126 == 0] <- NA
  cell_size <- raster::area(future_126, na.rm = TRUE, weights = FALSE)
  mpi_126_area <- sum(values(cell_size), na.rm = TRUE)
  
  future_585 <- raster_mpi_585 ; future_585[future_585 == 0] <- NA
  cell_size <- raster::area(future_585, na.rm = TRUE, weights = FALSE)
  mpi_585_area <- sum(values(cell_size), na.rm = TRUE)
  
  raster_sa <- stack(present, future_126, crop(future_585, extent(present)))
  names(raster_sa) <- c("present", "mpi_126", "mpi_585")
  
  # compute the results
  results <- rbind(results, c(i, "Am_Sul", present_area, mpi_126_area, mpi_585_area))
  
  
  ## 1.2. Calculate area gain/loss assuming no dispersal  ####
  occ <- subset(records, sp == i, select = c("lon", "lat"))
  
  # delimit geographic range as the Minimum Convex Polygon (MCP) with a 100 km buffer
  # create a MCP including all records
  msk <- adehabitatHR::mcp(SpatialPoints(occ, CRS("+proj=longlat +datum=WGS84 +no_defs")), percent = 100)
  msk <- spTransform(msk, CRS("+init=epsg:32724")) # project MCP shapefile
  msk <- raster::buffer(msk, width = 100*1000, dissolve = TRUE) # create a 100 km buffer
  msk <- spTransform(msk, CRS("+proj=longlat +datum=WGS84 +no_defs")) # back to WGS84 unprojected
  
  # get sizes of all cells in raster (km2)
  present <- crop(mask(raster_present, msk), extent(msk))
  present[present == 0] <- NA
  cell_size <- raster::area(present, na.rm = TRUE, weights = FALSE)
  present_area <- sum(values(cell_size), na.rm = TRUE)
  
  future_126 <- crop(mask(raster_mpi_126, msk), extent(msk))
  future_126[future_126 == 0] <- NA
  cell_size <- raster::area(future_126, na.rm = TRUE, weights = FALSE)
  mpi_126_area <- sum(values(cell_size), na.rm = TRUE)
  
  future_585 <- crop(mask(raster_mpi_585, msk), extent(msk))
  future_585[future_585 == 0] <- NA
  cell_size <- raster::area(future_585, na.rm = TRUE, weights = FALSE)
  mpi_585_area <- sum(values(cell_size), na.rm = TRUE)
  
  raster_no <- stack(present, future_126, crop(future_585, extent(present)))
  names(raster_no) <- c("present", "mpi_126", "mpi_585")
  
  # compute the results
  results <- rbind(results, c(i, "no_dispersal", present_area, mpi_126_area, mpi_585_area))
  
  
  ## 1.3. Calculate area gain/loss for the Doce Riverbasin  ####
  # import riverbasin shapefile
  doce <- shapefile(riverbasin)
  proj4string(doce) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # get sizes of all cells in raster (km2)
  present <- crop(mask(raster_present, doce), extent(doce))
  present[present == 0] <- NA
  cell_size <- raster::area(present, na.rm = TRUE, weights = FALSE)
  present_area <- sum(values(cell_size), na.rm = TRUE)
  
  future_126 <- crop(mask(raster_mpi_126, doce), extent(doce))
  future_126[future_126 == 0] <- NA
  cell_size <- raster::area(future_126, na.rm = TRUE, weights = FALSE)
  mpi_126_area <- sum(values(cell_size), na.rm = TRUE)
  
  future_585 <- crop(mask(raster_mpi_585, doce), extent(doce))
  future_585[future_585 == 0] <- NA
  cell_size <- raster::area(future_585, na.rm = TRUE, weights = FALSE)
  mpi_585_area <- sum(values(cell_size), na.rm = TRUE)
  
  raster_bhrd <- stack(present, future_126, crop(future_585, extent(present)))
  names(raster_bhrd) <- c("present", "mpi_126", "mpi_585")
  
  # compute the results
  results <- rbind(results, c(i, "BHRD", present_area, mpi_126_area, mpi_585_area))
  
  
  ## 1.4. Map gain/losses in geographic distributions  ####
  # set layout for the image exportation
  png(paste("./modelos/gualaxo/posproces_th/", i, ".png", sep = ""), units = "in", width = 4, height = 5, res = 300)
  
  # set figure layout
  par(mfrow = c(3, 2),
      oma = c(0, 1.5, 1.5, 0), mar = c(0.5, 0.5, 0.5, 0), mgp = c(1.8, 0.5, 0),
      cex.lab = 1.4, family = "sans", font.lab = 2)
  
  # color palette
  # blue #0568BF = area gain // yellow #ED553B = area stability // red #F5EA54 = area loss
  # suggestion: c("#0568BF", "#ED553B", "#F5EA54")
  palette <- c("blue", "red", "yellow")
  
  # import world borders shapefiles
  world <- shapefile(border)
  
  # plot map for each extent (South America, no dispersal and BHRD)
  # Valores: para ganho de area = 1; para perda de area = 2; para estabilidade = 3
  for(j in c("raster_sa", "raster_no", "raster_bhrd")){
    
    x <- get(j)
    x[["present"]][x[["present"]] == 1] <- 2
    
    # map area gain, loss and stability for both scenarios
    mpi_126 <- sum(x[["present"]], x[["mpi_126"]], na.rm = TRUE)
    mpi_126[mpi_126 == 0] <- NA
    
    mpi_585 <- sum(x[["present"]], x[["mpi_585"]], na.rm = TRUE)
    mpi_585[mpi_585 == 0] <- NA
    
    # export raster files
    writeRaster(mpi_126, paste("./modelos/gualaxo/posproces_th/", i, "_mpi126_", j, ".tif", sep = ""), 
                format = "GTiff", overwrite = TRUE)
    writeRaster(mpi_585, paste("./modelos/gualaxo/posproces_th/", i, "_mpi585_", j, ".tif", sep = ""), 
                format = "GTiff", overwrite = TRUE)
    
    # plot maps
    if(j == "raster_sa"){
      plot(mpi_126, yaxt = "n", xaxt = "n", col = palette, ylab = NA, legend = FALSE)
      plot(world, add = TRUE)
      
      mtext("MPI 126", side = 3, line = 0.5, cex = 1, font = 2)
      mtext("South Am", side = 2, line = 0.5, cex = 1, font = 2)
      
      plot(mpi_585, yaxt = "n", xaxt = "n", col = palette, ylab = NA, legend = FALSE)
      plot(world, add = TRUE)
      mtext("MPI 585", side = 3, line = 0.5, cex = 1, font = 2)
      
    } else if(j == "raster_no"){
      plot(mpi_126, yaxt = "n", xaxt = "n", col = palette, ylab = NA, legend = FALSE)
      plot(world, add = TRUE)
      
      mtext("no dispersal", side = 2, line = 0.5, cex = 1, font = 2)
      
      plot(mpi_585, yaxt = "n", xaxt = "n", col = palette, ylab = NA, legend = FALSE)
      plot(world, add = TRUE)
      
    } else if(j == "raster_bhrd"){
      plot(mpi_126, yaxt = "n", xaxt = "n", col = palette, ylab = NA, legend = FALSE)
      plot(doce, add = TRUE)
      
      mtext("BHRD", side = 2, line = 0.5, cex = 1, font = 2)
      
      plot(mpi_585, yaxt = "n", xaxt = "n", col = palette, ylab = NA, legend = FALSE)
      plot(doce, add = TRUE)
      
    }
  }
  dev.off()
}

# give column names for the output dataframe
colnames(results) <- c("species", "extent", "present", "mpi_126", "mpi_585")

# correct column class
results$species <- as.factor(results$species)
results$extent <- as.factor(results$extent)
results$present <- as.numeric(results$present)
results$mpi_126 <- as.numeric(results$mpi_126)
results$mpi_585 <- as.numeric(results$mpi_585)

# calculate ratio of area gain/loss
results$mpi_126_ratio <- results$mpi_126/results$present
results$mpi_585_ratio <- results$mpi_585/results$present

# export results
write.csv(results, output)



###-----------------------------------------------------------------------------------
### To sum up the raster files of all species for present and both future
### This will give the hotspots areas of richness for these species

spp_present <- grep(pattern = "(?=.*/present)(?=.*0.5_consensus)", 
                     x = list.files(path = path, recursive = TRUE, pattern = ".tif$", 
                                    full.names = TRUE), value = TRUE, perl = TRUE) %>% 
  stack(.)

spp_mpi126 <- grep(pattern = "(?=.*/mpi_126)", 
                    x = list.files(path = path, recursive = TRUE, pattern = ".tif$", 
                                   full.names = TRUE), value = TRUE, perl = TRUE) %>% 
  stack(.)

spp_mpi585 <- grep(pattern = "(?=.*/mpi_585)", 
                    x = list.files(path = path, recursive = TRUE, pattern = ".tif$", 
                                   full.names = TRUE), value = TRUE, perl = TRUE) %>% 
  stack(.)


plot(spp_present)


#Sum up the raster files to map richness
hot_present <- calc(spp_present, sum)
hot_mpi126 <- calc(spp_mpi126, sum)
hot_mpi585 <- calc(spp_mpi585, sum)
# hot_mpi126_bhrd <- calc(spp_mpi126_bhrd, sum)
# hot_mpi585_bhrd <- calc(spp_mpi585_bhrd, sum)

# export raster files
writeRaster(hot_present, "./modelos/gualaxo/posproces/hot_present", 
            format = "GTiff", overwrite = TRUE)
writeRaster(hot_mpi126, "./modelos/gualaxo/posproces/hot_mpi126", 
            format = "GTiff", overwrite = TRUE)
writeRaster(hot_mpi585, "./modelos/gualaxo/posproces/hot_mpi585",
            format = "GTiff", overwrite = TRUE)

#crop hotspots areas for the watershed
hot_present_bhrd <- crop(mask(hot_present, doce), extent(doce))
hot_mpi126_bhrd <- crop(mask(hot_mpi126, doce), extent(doce))
hot_mpi585_bhrd <- crop(mask(hot_mpi585, doce), extent(doce))

#plot watershed hotspots
par(mfrow = c(2,2), mar = c(2, 2, 2, 2))
plot(hot_present_bhrd, col=rainbow(100))
plot(hot_mpi126_bhrd, col=rainbow(100))
plot(hot_mpi585_bhrd, col=rainbow(100))
dev.off()


# export raster files
writeRaster(hot_present_bhrd, "./modelos/gualaxo/posproces/hotspots/hot_present_bhrd", 
            format = "GTiff", overwrite = TRUE)
writeRaster(hot_mpi126_bhrd, "./modelos/gualaxo/posproces/hotspots/hot_mpi126_bhrd", 
            format = "GTiff", overwrite = TRUE)
writeRaster(hot_mpi585_bhrd, "./modelos/gualaxo/posproces/hotspots/hot_mpi585_bhrd",
            format = "GTiff", overwrite = TRUE)




###-----------------------------------------------------------------------------------
### To crop the raster files of hotsposts to natural areas of the watershed
### This will give the hotspots areas of richness except for non natural areas


# Load hotspots rasters
hot_present_bhrd <- raster("./modelos/gualaxo/posproces/hotspots/hot_present_bhrd.tif")
hot_mpi126_bhrd <- raster("./modelos/gualaxo/posproces/hotspots/hot_mpi126_bhrd.tif")
hot_mpi585_bhrd <- raster("./modelos/gualaxo/posproces/hotspots/hot_mpi585_bhrd.tif")

#Load natural areas
natu <- readOGR(dsn = "./data/shape", layer = "natural-coverage-2020")

#crop hotspots areas to the natural areas of bhrd
hot_present_bhrd_natu <- crop(mask(hot_present_bhrd, natu), extent(natu))
hot_mpi126_bhrd_natu <- crop(mask(hot_mpi126_bhrd, natu), extent(natu))
hot_mpi585_bhrd_natu <- crop(mask(hot_mpi585_bhrd, natu), extent(natu))

#plot watershed hotspots
par(mfrow = c(2,2), mar = c(2, 2, 2, 2))
plot(hot_present_bhrd_natu, col=rainbow(100))
plot(hot_mpi126_bhrd_natu, col=rainbow(100))
plot(hot_mpi585_bhrd_natu, col=rainbow(100))
dev.off()


# export raster files
writeRaster(hot_present_bhrd_natu, "./modelos/gualaxo/posproces/hotspots/hot_present_bhrd_natu", 
            format = "GTiff", overwrite = TRUE)
writeRaster(hot_mpi126_bhrd_natu, "./modelos/gualaxo/posproces/hotspots/hot_mpi126_bhrd_natu", 
            format = "GTiff", overwrite = TRUE)
writeRaster(hot_mpi585_bhrd_natu, "./modelos/gualaxo/posproces/hotspots/hot_mpi585_bhrd_natu",
            format = "GTiff", overwrite = TRUE)



hot_present_bhrd_natu <- raster("./modelos/gualaxo/posproces/hotspots/hot_present_bhrd_natu.tif")
hot_mpi126_bhrd_natu <- raster("./modelos/gualaxo/posproces/hotspots/hot_mpi126_bhrd_natu.tif")
hot_mpi585_bhrd_natu <- raster("./modelos/gualaxo/posproces/hotspots/hot_mpi585_bhrd_natu.tif")


library(dplyr)

options(scipen=999)

area_natu_pres <- as.data.frame(hot_present_bhrd_natu) %>%
  group_by(hot_present_bhrd) %>%
  tally() %>%
  mutate(area_ha = n * 7989.893) # 0.7989893 km2 or 7989.893 ha area of each cell.

area_natu_mpi126 <- as.data.frame(hot_mpi126_bhrd_natu) %>%
  group_by(hot_mpi126_bhrd) %>%
  tally() %>%
  mutate(area_ha = n * 7989.893) # 0.7989893 km2 or 7989.893 ha area of each cell.

area_natu_mpi585 <- as.data.frame(hot_mpi585_bhrd_natu) %>%
  group_by(hot_mpi585_bhrd) %>%
  tally() %>%
  mutate(area_ha = n * 7989.893) # 0.7989893 km2 or 7989.893 ha area of each cell.