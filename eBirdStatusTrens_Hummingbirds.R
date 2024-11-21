# Map areas of importance during the breeding season for the Hummingbirds in USA
  #(not included Lucifer Hummingbird, White-eared Hummingbird)

#### ~Packages~ ####
library(dplyr)
library(ebirdst)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(terra)
library(geodata)

#### ~Data - eBird Status and Trends~ ####

hummingbird_species <- c("Rivoli's Hummingbird",
                         "Blue-throated Mountain-gem",
                         "Ruby-throated Hummingbird",
                         "Black-chinned Hummingbird",
                         "Anna's Hummingbird",
                         "Costa's Hummingbird",
                         "Broad-tailed Hummingbird",
                         "Rufous Hummingbird",
                         "Allen's Hummingbird",
                         "Calliope Hummingbird",
                         "Broad-billed Hummingbird",
                         "Buff-bellied Hummingbird",
                         "Violet-crowned Hummingbird")

#US boundary
us_boundary <- ne_states(iso_a2 = "US") |>
  #remove Alaska and Hawaii
  filter(name != "Alaska",
         name != "Hawaii") |>
  # transform coordinate system to match the raster data
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") |>
  vect()

range_us <- list()
for (species in hummingbird_species) {
  # download median abundance at 9km
  ebirdst_download_status(species, 
                          pattern = "abundance_median_9km_2022",
                          force = TRUE)
  
  # load breeding/resident season relative abundance
  abd <- load_raster(species,
                     period = "seasonal") 
  # crop and mask to US
  abd_masked <- mask(crop(abd, us_boundary), 
                     us_boundary)
  # convert to binary, presence-absence
  range_us[[species]] <- abd_masked > 0
}

# sum across species to calculate richness
richness <- sum(rast(range_us), na.rm = TRUE)
# make a simple map
plot(richness, axes = FALSE, col = viridis::viridis(100))

# drop zeros
richness <- ifel(richness == 0, NA, richness)
# drop anything below median
cutoff <- global(richness, quantile, probs = 0.5, na.rm = TRUE) |>
  as.numeric()
#filter importance below the cutoff (median)
richness <- ifel(richness > cutoff, richness, NA)
plot(richness, axes = FALSE, col = viridis::viridis(100))

# reproject
richness_proj <- trim(project(richness, "ESRI:102003"))
us_boundary_proj <- project(us_boundary, "ESRI:102003")

# basemap
par(mar = c(0, 0, 0, 0))
plot(us_boundary_proj, col = "lightgrey", axes = FALSE,
     main = "Areas of richness importance for hummingbird in USA")
# add importance raster
plot(richness_proj, legend = FALSE,
     add = TRUE, col = viridis::viridis(100))

# add legend
fields::image.plot(zlim = c(0, 1), legend.only = TRUE,
                   col = viridis::viridis(100),
                   breaks = seq(0, 1, length.out = 101),
                   smallplot = c(0.15, 0.85, 0.12, 0.15),
                   horizontal = TRUE,
                   axis.args = list(at = c(0, 0.5, 1),
                                    labels = c("Low", "Medium", "High"),
                                    fg = "black", col.axis = "black",
                                    cex.axis = 0.75, lwd.ticks = 0.5,
                                    padj = -1.5),
                   legend.args = list(text = "Relative Importance",
                                      side = 3, col = "black",
                                      cex = 1, line = 0))



# for more granularity, use mean proportion of population 
  #(scaling factor, it can change)

# produce proportion of population layers for each species masked to USA
prop_pop_us <- list()
for (species in hummingbird_species) {
  # download seasonal abundance at 9km
  ebirdst_download_status(species,
                          pattern = "proportion-population_median_9km_2022",
                          force = T)
  
  # load breeding season proportion of population
  prop_pop <- load_raster(species,
                          product = "proportion-population",
                          period = "seasonal")
  
  # crop and mask to US
  prop_pop_us[[species]] <- mask(crop(prop_pop, us_boundary), us_boundary)
}

# take mean across species
importance <- mean(rast(prop_pop_us), na.rm = TRUE)
# drop zeros
importance <- ifel(importance == 0, NA, importance)
# drop anything below median
cutoff <- global(importance, quantile, probs = 0.5, na.rm = TRUE) |>
  as.numeric()
#filter importance below the cutoff (median)
importance <- ifel(importance > cutoff, importance, NA)

# make a simple map
plot(importance, axes = FALSE, col = viridis::viridis(100))
plot(us_boundary, col = "grey", axes = FALSE, add = TRUE)
plot(importance, axes = FALSE, legend = FALSE, add = TRUE,
     col = viridis::viridis(100))

# make a slightly nicer map
# reproject
importance_proj <- trim(project(importance, "ESRI:102003"))
us_boundary_proj <- project(us_boundary, "ESRI:102003")
# basemap
par(mar = c(0, 0, 0, 0))
plot(us_boundary_proj, col = "lightgrey", axes = FALSE,
     main = "Areas of importance for hummingbirds in USA")
# add importance raster
plot(importance_proj, legend = FALSE, add = TRUE, col = viridis::viridis(100))
# add legend
fields::image.plot(zlim = c(0, 1), legend.only = TRUE,
                   col = viridis::viridis(100),
                   breaks = seq(0, 1, length.out = 101),
                   smallplot = c(0.15, 0.85, 0.12, 0.15),
                   horizontal = TRUE,
                   axis.args = list(at = c(0, 0.5, 1),
                                    labels = c("Low", "Medium", "High"),
                                    fg = "black", col.axis = "black",
                                    cex.axis = 0.75, lwd.ticks = 0.5,
                                    padj = -1.5),
                   legend.args = list(text = "Relative Importance",
                                      side = 3, col = "black",
                                      cex = 1, line = 0))
