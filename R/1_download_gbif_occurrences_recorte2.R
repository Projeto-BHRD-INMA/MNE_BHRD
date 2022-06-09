################################################################################
###                                                                          ###
###                TUTORIAL TO DOWNLOAD OCCURRENCES FROM GGBIF               ###
###                     USING A LIST OF KNOWN NAMES OF SPECIES               ###
###https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/   ###
###                                                                          ###
###                Modified by Danielle Moreira                              ###
###                         02 set 2022                                      ###
###                                                                          ###
################################################################################



#install.packages("remotes")
#remotes::install_github("ropensci/taxize")
if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, purrr, readr, magrittr, rgbif, taxize)
library(taxize)

#The important part here is to use rgbif::occ_download with pred_in and to fill in your gbif credentials.

# fill in your gbif.org credentials. You need to create an account at gbif if you don't have it.

user <- "XXXXX" # your gbif.org username
pwd <- "XXXXX" # your gbif.org password
email <- "XXXXX" # your email

#############################################################################
oc <- read.csv("./data/registros/spp_2022/1_spp_recorte2.csv", sep = ';')
names(oc)

gbif_taxon_keys <-
  read.csv("./data/registros/spp_2022/1_spp_recorte2.csv", sep = ';') %>% #For an file with a list of spp names
  pull(spp) %>% #Specify the column from the list
  taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxonkeys
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
  bind_rows() %T>% # combine all data.frames into one
  readr::write_tsv(path = "all_matches.tsv") %>% # save as side effect for you to inspect if you want
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
  filter(kingdom == "Plantae") %>% # remove anything that might have matched to a non-plant
  pull(usagekey) # get the gbif taxonkeys

# gbif_taxon_keys should be a long vector like this c(2977832,2977901,2977966,2977835,2977863)
# !!very important here to use pred_in!!


# use matched gbif_taxon_keys from above
occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN')),
  #pred("geometry","POLYGON((-43.86 -17.57, -43.88 -21.49, -39.79 -19.86, -39.46 -17.98, -43.86
  #     -17.57))"),
  #pred("country", "BR"),
  #pred("continent", "South America"),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", 1950),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)


# Results of this download
#Check status with
occ_download_wait('0330953-210914110416597')
#After it finishes, use
d <- occ_download_get('0330953-210914110416597') %>%
  occ_download_import()
#to retrieve your download.
#Download Info:
#  Username: XXXXX
# E-mail: XXXXX
# Format: SIMPLE_CSV
# Download key: 0330953-210914110416597
# Created: 2022-06-01T18:23:23.114+00:00
# Citation Info:  
#   Please always cite the download DOI when using this data.
# https://www.gbif.org/citation-guidelines
# DOI: 10.15468/dl.34up3j
# Citation:
#   GBIF Occurrence Download https://doi.org/10.15468/dl.34up3j Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2022-06-01