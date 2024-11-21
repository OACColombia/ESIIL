# How many records of hummingbirds has photo in Macaulay Library (eBird)?

#Packages

library(tidyverse)
library(auk)

#Hummingbird species
#Using taxonomy in eBird from Clements
hummingbird_species <- read_csv("Clements-v2024-October-2024-rev.csv") |>
  filter(family == "Trochilidae (Hummingbirds)") |>
  rename(English_name = `English name`) |>
  select(English_name) |>
  filter(English_name != "Hummingbirds") |>
  drop_na(English_name) |>
  as.data.frame()

#Converting it in a vector
hummingbird_species <- c(hummingbird_species$English_name)

#Removing subcategories from the species
hummingbird_species <- hummingbird_species[!str_detect(hummingbird_species,
                                                       "\\(")]

#Temporal files for filtering
f_ebd <- "ebd_hummingbirds.txt" #Temporal file to save the filtering eBird data (records)
f_sed <- "sed_hummingbirds.txt" #Temporal file to save the filtering sampling event data

#Conducting filtering on the entire eBird dataset (587 GB)
ebd_filter_hummingbirds <- auk_ebd("ebd_relMay-2024.txt") |> 
  #filter only species of hummingbirds
  auk_species(species = hummingbird_species) |>
  #We can add more filters here...
  #Apply filtering (removing duplicates of group lists)
  auk_filter(f_ebd, overwrite=T, keep = cols_to_keep) |>
  #read text file from AWK into R data frame
  read_ebd()

#Filter hummingbirds eBird data by only hummingbirds with media
  #and recover the link to access the photo
hummingbirds_media <- ebd_filter_hummingbirds |>
  #only records with media reported
  filter(has_media == TRUE) |>
  #generate the link to the checklist with the photo (media) reported
  mutate(link_media = paste0("https://ebird.org/checklist/",
                             str_split(sampling_event_identifier, ",") |>
                               map_chr(1)))

saveRDS(hummingbirds_media, "hummingbirds_media_May2024.rds")

#End of this code
