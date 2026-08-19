#Aim: To do the spatial intersection between extinct and threatened birds and island rodent eradication programs considering it's evidence 
#
rm(list = ls())
gc()
#
# 1. Loading packages -----------------------------------------------------
library(data.table)
library(sf)
library(terra)
library(rnaturalearth)
library(ggplot2)
library(readxl)
library(writexl)

# 2. Load data and builds --------------------------------------------------
# NOTE FOR REVIEWERS:
#All intermediate spatial outputs from the overlap between the bird species ranges and the rodent eradication programs, are provided in the builds folder.
#redo <- FALSE by default. 
#Set redo <- TRUE only if you have access to the BirdLife species range maps, available at https://datazone.birdlife.org/contact-us/request-our-data 

#DIISE (eradication programs)
data_DIISE <- fread("data/Working_Databases/DIISE_2018_query.csv")

#Filter df using rodent sp of interest
rodent_sp<- c("Mus musculus","Rattus exulans","Rattus norvegicus","Rattus rattus")
data_DIISE <- data_DIISE[`Scientific Name` %in% rodent_sp] #there are 1104 rodent eradication

#Review and frequency data
data_review <- fread("data/Working_Databases/Systematic_Review_Merged.csv")
review_species <- unique(data_review$scientificName) #List of bird species included in the review

frequency_df <- fread("builds/systematic_review/frequency_table_for_checking.csv")

#Review data with study sites name modified to match the structure in the DIISE dataset
Studies<- fread("data/Working_Databases/Studies_site_fixed.csv")
Studies<- Studies[,.(scientificName, Study_rodent_final, Hypothesis_supported, Study_location, Latitude, Longitude, Island_name, Archipelago, Region, Country)]

#One row per unique study locations and bird species
Studies_location_uniq<-unique(Studies[,.(scientificName, Study_rodent_final, Hypothesis_supported ,Latitude,Longitude,Island_name, Archipelago, Region, Country)])

evidence_levels <- c(
  "No studies found", 
  "All studies are not in support", 
  "Only predation in support",
  "Lethal program in support",
  "Population study without data in support",
  "Population study with data in support",
  "Population study with all qualities in support"
)

eradication_levels <- c(
  "Unknown", "Unknown pre-status", "Trial or Research only", "Incomplete", 
  "Planned", "In Progress", "To Be Confirmed",  "Failed", "Successful (Reinvaded)",
  "Successful"                   
)

#Assign evidence and eradication status order
frequency_df[, synth_col := factor(synth_col, levels = evidence_levels)]
data_DIISE[, `Status (Eradication)` := factor(`Status (Eradication)`, levels = eradication_levels)]

#List of extinct and threatened migratory bird species
 #BirdLife International (2026) Data downloaded from https://datazone.birdlife.org on 17/08/2026
migratory_list <- fread("data/Working_Databases/species-filter-results.csv")

world_sf <- st_as_sf(ne_countries(scale = "medium", returnclass = "sf"))
world_sf <- st_as_sf(world_sf, crs = 4326)

#Plots theme 
theme_lundy <- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

plot_col<-c(
  "No studies found" = "black",
  "All studies are not in support" = "grey20",
  "Only predation in support" = "grey50",
  "Lethal program in support" = "indianred4",
  "Population study without data in support" = "dodgerblue4",
  "Population study with data in support" = "dodgerblue2",
  "Population study with all qualities in support" = "dodgerblue2"
)

col_pal <- c("No studies found" = "transparent",
             "All studies are not in support" = "transparent",
             "Only predation in support" = "transparent",
             "Lethal program in support" = "transparent",
             "Population study without data in support" = "transparent",
             "Population study with data in support" = "transparent",
             "Population study with all qualities in support" = "gold"
)


# 3. Which threatened birds are single island-endemics? --------------------------------------------------
review_species #First we need to define which of these 340 spp are island endemic

#Intersect the ranges of the attributed birds (converted to raster cells) with island polygons.
#Species whose ranges overlapped only one island polygon are flagged as candidate single-island endemics.
review_species_query <- paste0("SELECT * FROM all_species WHERE sci_name IN ('", 
       paste(review_species, collapse = "','"), "')")

redo <- FALSE

if (redo) {
  
  #Create Raster template 
r_template <- rast(
  xmin = -180, xmax = 180,
  ymin = -90, ymax = 90,
  resolution = 1,
  crs = "EPSG:4326"
)
  
  #Read bird range rasters
  ranges_sf <- st_read(
    "databases/BirdLifemaps/BOTW_2024_2.gpkg",
    query = review_species_query
  )
  
  #Convert to terra
  ranges_vect <- vect(ranges_sf)
  rm(ranges_sf); gc()
  
  if (!same.crs(ranges_vect, r_template)) {
    ranges_vect <- project(ranges_vect, r_template)
  }
  
  #Rasterize by species
  out <- rasterize(
    ranges_vect,
    r_template,
    by = "sci_name",
    field = 1,
    background = NA, #0
    touches = TRUE
  )
  
  rm(ranges_vect); gc()
  
  #Convert to data table
  dt <- as.data.frame(out, xy = TRUE)
  setDT(dt)
  rm(out); gc()
  
  dt_long <- melt(
    dt,
    id.vars = c("x", "y"),
    variable.name = "species",
    value.name = "presence"
  )[presence == 1] # > 0
  
  rm(dt); gc()
  
  dt_long <- unique(dt_long)
  
  saveRDS(
    dt_long,
    file.path("builds/eradication_programs/birds_rast_cells.rds"))
  
  
  rm(dt_long); gc()
  

} else {
  
  birds_rast_cells <- readRDS("builds/eradication_programs/birds_rast_cells.rds")
  
}

unique(birds_rast_cells$species) #Great!

#Load island and mainland polygons with area/label — Natural Earth's minor_islands
islands_ne <- ne_download(scale = 10, type = "minor_islands",
                          category = "physical", returnclass = "sf")
land_ne10  <- ne_download(scale = 10, type = "land",
                          category = "physical", returnclass = "sf")

all_land <- st_make_valid(rbind(islands_ne[, "geometry"], land_ne10[, "geometry"]))

#Convert multipolygons into individual discrete polygons and give each an ID
all_land_single <- st_cast(all_land, "POLYGON")
all_land_single$island_id <- seq_len(nrow(all_land_single))
all_land_single$area_km2  <- as.numeric(st_area(all_land_single)) / 1e6

#Separate mainlands (huge area) from discrete islands
mainland_ids <- all_land_single$island_id[all_land_single$area_km2 > 2500000]

#Convert species grid cells to sf points
cells_sf <- st_as_sf(
  birds_rast_cells[, .(x, y)],
  coords = c("x", "y"),
  crs    = 4326
)

#Also add a cell ID
cells_sf$row_id <- seq_len(nrow(cells_sf))

#Join species cells to island_id 
pip <- st_join(cells_sf, all_land_single["island_id"], join = st_intersects)

#Check for duplicates. Entries with count = 2
table(table(pip$row_id)) #12

#For now, exclude duplicates
pip_dedup <- pip[!duplicated(pip$row_id), ]

#Count DISTINCT non-mainland islands per species
birds_rast_cells[, island_id := pip_dedup$island_id]

island_counts <- birds_rast_cells[!is.na(island_id) & !(island_id %in% mainland_ids),
                       .(n_distinct_islands = uniqueN(island_id)),
                       by = species]

single_island_true <- island_counts[n_distinct_islands == 1, species]
length(single_island_true) #91 species are candidates for single island endemic

#Check if those dup rows are an issue
dup_rows <- pip$row_id[duplicated(pip$row_id) | duplicated(pip$row_id, fromLast = TRUE)]
pip[pip$row_id %in% dup_rows, ]

# Which species have cells at these ambiguous points?
check_species <- birds_rast_cells[x %in% c(127.5, -74.5) & y %in% c(34.5, -52.5), 
                       unique(species)]
check_species

#Of those, which are in the single_island_true list?
intersect(check_species, single_island_true) #none of them, so no worries :-)

ggplot() +
  geom_sf(data = world_sf, fill = "grey85", color = "grey60", linewidth = 0.2) +
  geom_point(data = birds_rast_cells[species %in% "Acrocephalus caffer"], aes(x = x, y = y),
             color = "red", size = 0.5) 

#Next step: visually inspect each species' mapped distribution and cross-check it against the 'Range description' 
#in the corresponding BirdLife International species factsheet (datazone.birdlife.org) to confirm that the range did not extend across multiple islands

#Final classification
single_island <- read_xlsx("data/Working_Databases/single_island_endemic_classification.xlsx")
setDT(single_island)
table(single_island$single_island_endemic)

#Join single_island df to freq df and add to this one the Red List status for each species
spp_status <- unique(data_review[,.(scientificName, redlistCategory)])

spp_status <- merge(spp_status, migratory_list[,.(`Scientific name`, `Migratory status`)],
                    by.x = "scientificName", by.y ="Scientific name", all.x = TRUE)
spp_status[is.na(`Migratory status`), `Migratory status` := "Not a Migrant"]
table(spp_status$`Migratory status`) #72 species are full migrant

spp_status <- merge(spp_status, single_island[,.(scientificName, distribution, single_island_endemic)],
                    by = "scientificName")

frequency_df <- merge(frequency_df, spp_status, by = "scientificName")
frequency_df #Nice :-)

table(frequency_df$redlistCategory) #There's a spp without Red List status 
frequency_df[redlistCategory == "", scientificName] #Porphyrio paepae
frequency_df[scientificName == "Porphyrio paepae", redlistCategory := "Extinct"]

freq_studies <- frequency_df[synth_col!="No studies"] #These are the bird species with evidence (studies)

# 4. Overlap between threatened birds range and rodent eradication programs --------------------------------------------------
## House mouse -----------
DIISE_m_musculus<-data_DIISE[`Scientific Name`=="Mus musculus"]
unique(DIISE_m_musculus$`Eradication ID`) #there are 116 eradications
unique(DIISE_m_musculus$`Island Name`) #in 100 islands
table(DIISE_m_musculus$`Status (Eradication)`) #out of 116, 60 were successful
freq_df_m_musculus<-frequency_df[Rodent_attributed_final=="Mus musculus"]

summary_m_musculus <-freq_df_m_musculus[, .N, by = synth_col]
summary_m_musculus[, perc := (N / sum(N))*100]
summary_m_musculus
#45.45% bird species have no studies and 54.55% have studies, of these,
#No study in support: 3.03%, Only predation in support:45.45%, Population study without data in support:6.06%

freq_m_musculus_no_studies <- frequency_df[Rodent_attributed_final=="Mus musculus" & synth_col=="No studies found"]
studies_m_musculus<- freq_studies[Rodent_attributed_final=="Mus musculus"]

#For the species with studies, add the location (from the dataset that matches the DISSE format)
merge_studies_m_musculus<- merge(studies_m_musculus,
                                 Studies_location_uniq[Study_rodent_final=="Mus musculus"],
                                 by= "scientificName")

#Filter the combinations of study and evidence that are duplicated
merge_studies_m_musculus <- merge_studies_m_musculus[!(synth_col == "Only predation in support" & Hypothesis_supported == 0)]
unique(merge_studies_m_musculus$scientificName) #18 species

#Join with the no studies data, to have a single df for the bird species extinction attributed to the house mouse
merge_mus_musculus<-rbindlist(list(merge_studies_m_musculus[,Study_rodent_final:=NULL], freq_m_musculus_no_studies), use.names = TRUE, fill = TRUE)

#Bird species list
birds_m_musculus_review<- unique(merge_mus_musculus$scientificName) #33 species

threatened_species_query <- paste0("SELECT * FROM all_species WHERE sci_name IN ('", 
                                   paste(birds_m_musculus_review, collapse = "','"), "')")

#Load bird species distribution and perform the spatial intersection with eradication programs
redo <- FALSE

if (redo) {
  
  bird_distribution_m_musculus <- st_read(
    "databases/BirdLifemaps/BOTW_2024_2.gpkg",
    query = threatened_species_query
  )
  
  eradications_m_musculus <- st_as_sf(
    DIISE_m_musculus, coords = c("Longitude", "Latitude"), crs = 4326
  )
  eradications_m_musculus <- st_transform(
    eradications_m_musculus, st_crs(bird_distribution_m_musculus)
  )
  
  bird_distribution_m_musculus <- st_make_valid(bird_distribution_m_musculus)
  
  m_musculus_DIISE_bird_overlap <- st_intersection(
    bird_distribution_m_musculus,
    eradications_m_musculus %>%
      select(Country, Region, Archipelago, `Island Name`, `Eradication ID`)
  )
  
  saveRDS(m_musculus_DIISE_bird_overlap, "builds/eradication_programs/m_musculus_DIISE_bird_overlap.rds")
  
} else {
  
  m_musculus_DIISE_bird_overlap <- readRDS("builds/eradication_programs/m_musculus_DIISE_bird_overlap.rds")
  
}

#Add the eradication type
m_musculus_DIISE_bird_overlap <- merge(m_musculus_DIISE_bird_overlap, DIISE_m_musculus[,.(`Eradication ID`, `Status (Eradication)`)], by.x = "Eradication.ID", by.y = "Eradication ID")

#Clean the df
#setDT(m_musculus_DIISE_bird_overlap)
m_musculus_DIISE_bird_overlap <- m_musculus_DIISE_bird_overlap[, .(
  sci_name, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, Archipelago, Region, Country, geom
)]

setnames(m_musculus_DIISE_bird_overlap, "sci_name", "scientificName")

#Add the evidence column for that species
m_musculus_DIISE_bird_overlap <- merge(m_musculus_DIISE_bird_overlap,
                                     unique(merge_mus_musculus[,.(scientificName, synth_col, redlistCategory, `Migratory status`, distribution, single_island_endemic)]),
                                     by= "scientificName")

m_musculus_DIISE_bird_overlap <- m_musculus_DIISE_bird_overlap[,.(
  scientificName, synth_col, redlistCategory, `Migratory status`, distribution, single_island_endemic, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, Archipelago, Region, Country, geom
)]

#This df includes data of the review, rodent eradication programs and birds distribution
m_musculus_DIISE_bird_overlap[, Rodent_attributed_final := "Mus musculus"]

#Are there ex species? exclude them 
table(m_musculus_DIISE_bird_overlap$redlistCategory)

#overlaps between bird species ranges and eradication sites should only include:
#breeding-season range polygons, for migratory species (BirdLife International seasonal code 2);
#resident and breeding range polygons for non-migratory species (seasonal code 1, 2)
#the passage and seasonal occurrence range (codes 4, 5) were excluded as it refers to temporal and uncertain ranges.
#non-breeding ranges for non-migratory species were included only when their total area fell below 5000 km2, to exclude areas that represent a broader dispersal beyond the resident range.
table(m_musculus_DIISE_bird_overlap$seasonal)
table(m_musculus_DIISE_bird_overlap[`Migratory status` == "Full migrant", seasonal])
table(m_musculus_DIISE_bird_overlap[`Migratory status` == "Not a Migrant", seasonal])

#How many species overlapped before filtering?
x <-unique(m_musculus_DIISE_bird_overlap$scientificName) #20 species out of 33 species

#Filter out from the overlap the resident range of those species that are fully migrant since the overlap is potentially a false positive
m_musculus_DIISE_bird_overlap <- m_musculus_DIISE_bird_overlap[`Migratory status` != "Full migrant" | !seasonal %in% c(1,3,4,5), ]
unique(m_musculus_DIISE_bird_overlap$scientificName) #Instead of 20 species, 14 truly overlapped
ex_m_musculus <- setdiff(x, unique(m_musculus_DIISE_bird_overlap$scientificName)) 
ex_m_musculus #Excluded spp: "Hydrobates leucorhous" "Larus heermanni" "Pachyptila macgillivrayi" "Pterodroma arminjoniana" "Pterodroma externa" "Puffinus opisthomelas"

#How many bird species distribution overlap with the house mouse eradications?
length(unique(m_musculus_DIISE_bird_overlap$scientificName)) #14 species out of 33 species!

#How many islands?
length(unique(m_musculus_DIISE_bird_overlap$Island.Name)) #13 islands! (instead of 44)

#How many eradications?
length(unique(m_musculus_DIISE_bird_overlap$Eradication.ID)) #13 eradications! (instead of 47)

#For the maps, plot the eradication programs with the best possible evidence 
#Select the best evidence row per island
summary_DIISE_bird_m_musculus <- m_musculus_DIISE_bird_overlap[, .(
  n_eradications = uniqueN(Eradication.ID)),
  by = .(Island.Name, `Status (Eradication)`)] 

summary_DIISE_bird_m_musculus <- summary_DIISE_bird_m_musculus[, .(
  n_erad_succ = sum(n_eradications[`Status (Eradication)` == "Successful"]),
  n_erad_not_succ = sum(n_eradications[`Status (Eradication)` != "Successful"]),
  total_erad = sum(n_eradications)),
  by = Island.Name]

#Count number of threatened bird species per island
threat_count <- m_musculus_DIISE_bird_overlap[, .(
  n_threat_birds = uniqueN(scientificName)),
  by = Island.Name]

m_musculus_DIISE_bird_overlap <- m_musculus_DIISE_bird_overlap[order(Island.Name,  -as.integer(synth_col))] #, -as.integer(`Status (Eradication)`)

best_evidence_per_island <- m_musculus_DIISE_bird_overlap[, .SD[1], by = Island.Name]  

#join the three of them
m_musculus_island <- merge(summary_DIISE_bird_m_musculus, threat_count, by = "Island.Name", all.x = TRUE)
m_musculus_island <- merge(m_musculus_island, best_evidence_per_island, by = "Island.Name", all.x = TRUE)
m_musculus_island<- m_musculus_island[,.(
  Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, n_erad_succ, n_erad_not_succ, total_erad, n_threat_birds,
  scientificName, redlistCategory, `Migratory status`, distribution, single_island_endemic,
  synth_col, Rodent_attributed_final, Archipelago , Region, Country, geom
  
)]

## Pacific rats -----------
DIISE_r_exulans<-data_DIISE[`Scientific Name`=="Rattus exulans"]
unique(DIISE_r_exulans$`Eradication ID`) #there are 179 eradications
unique(DIISE_r_exulans$`Island Name`) #in 162 islands
table(DIISE_r_exulans$`Status (Eradication)`) #out of 179, 119 were successful
freq_df_r_exulans<-frequency_df[Rodent_attributed_final=="Rattus exulans"]

summary_r_exulans <-freq_df_r_exulans[, .N, by = synth_col]
summary_r_exulans[, perc := (N / sum(N))*100]
summary_r_exulans 
#75% bird species have no studies and 25% have studies, of these,
#Lethal program in support: 2.58%, Only predation in support:8.62%, No study in support: 8.62%,#
#Population study without data in support:2.58%,  Population study with data in support:1.72%
#Population study with all qualities in support: 0.86%

freq_r_exulans_no_studies <- frequency_df[Rodent_attributed_final=="Rattus exulans" & synth_col=="No studies found"]
studies_r_exulans<- freq_studies[Rodent_attributed_final=="Rattus exulans"]

#For the species with studies, add the location (from the dataset that matches the DISSE format)
merge_studies_r_exulans<- merge(studies_r_exulans,
                                Studies_location_uniq[Study_rodent_final=="Rattus exulans"],
                                by= "scientificName")

#Filter the combinations of study and evidence that are duplicated 
merge_studies_r_exulans <- merge_studies_r_exulans[
  !(
    (synth_col == "Only predation in support" & Hypothesis_supported == 0) |
      (synth_col == "Population study with all qualities in support" & Hypothesis_supported == 0)
  )
]

unique(merge_studies_r_exulans$scientificName) #29 species.

#Join with the no studies data, to have a single df for the bird species extinction associated with pacific rat
merge_r_exulans<-rbindlist(list(merge_studies_r_exulans[,Study_rodent_final:=NULL], freq_r_exulans_no_studies), use.names = TRUE, fill = TRUE)

#Bird species list
birds_r_exulans_review<- unique(merge_r_exulans$scientificName) #116 species

threatened_species_query <- paste0("SELECT * FROM all_species WHERE sci_name IN ('", 
                                   paste(birds_r_exulans_review, collapse = "','"), "')")

#Load bird species distribution and perform the spatial intersection with eradication programs
redo <- FALSE

if (redo) {
  
  bird_distribution_r_exulans <- st_read("databases/BirdLifemaps/BOTW_2024_2.gpkg", query = threatened_species_query)
  
  eradications_r_exulans<- st_as_sf(DIISE_r_exulans, coords = c("Longitude", "Latitude"), crs = 4326)
  
  eradications_r_exulans <- st_transform(eradications_r_exulans, st_crs(bird_distribution_r_exulans))
  
  bird_distribution_r_exulans <- st_make_valid(bird_distribution_r_exulans)
  
  r_exulans_DIISE_bird_overlap <- st_intersection(
  bird_distribution_r_exulans,
  eradications_r_exulans %>% select(Country, Region, Archipelago, `Island Name`, `Eradication ID`))
  
  saveRDS(r_exulans_DIISE_bird_overlap, "builds/eradication_programs/r_exulans_DIISE_bird_overlap.rds")
  
} else {
  
  r_exulans_DIISE_bird_overlap<-readRDS("builds/eradication_programs/r_exulans_DIISE_bird_overlap.rds")
  
}

#Add the eradication type
r_exulans_DIISE_bird_overlap <- merge(r_exulans_DIISE_bird_overlap, DIISE_r_exulans[,.(`Eradication ID`, `Status (Eradication)`)], by.x = "Eradication.ID", by.y = "Eradication ID")

#Clean the df
setDT(r_exulans_DIISE_bird_overlap)
r_exulans_DIISE_bird_overlap<- r_exulans_DIISE_bird_overlap[, .(
  sci_name, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name,Archipelago, Region, Country, geom
)]

setnames(r_exulans_DIISE_bird_overlap, "sci_name", "scientificName")

#Add the evidence column for that species
r_exulans_DIISE_bird_overlap<-merge(r_exulans_DIISE_bird_overlap,
                                    unique(merge_r_exulans[,.(scientificName, synth_col, redlistCategory, `Migratory status`, distribution, single_island_endemic)]),
                                    by= "scientificName")

r_exulans_DIISE_bird_overlap<- r_exulans_DIISE_bird_overlap[,.(
  scientificName, synth_col, redlistCategory, `Migratory status`, distribution, single_island_endemic, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, Archipelago, Region, Country, geom
)]

#This df includes data of the review, rodent eradication programs and birds distribution
r_exulans_DIISE_bird_overlap[, Rodent_attributed_final := "Rattus exulans"] 

#Exclude extinct species
table(r_exulans_DIISE_bird_overlap$redlistCategory)
r_exulans_DIISE_bird_overlap <- r_exulans_DIISE_bird_overlap[redlistCategory != "Extinct", ]

#Reduce false positives in the overlap 
table(r_exulans_DIISE_bird_overlap$seasonal) #29 rows with seasonal code 4. Exclude them
r_exulans_DIISE_bird_overlap <- r_exulans_DIISE_bird_overlap[seasonal != 4, ]
table(r_exulans_DIISE_bird_overlap[`Migratory status` == "Full migrant", seasonal])
table(r_exulans_DIISE_bird_overlap[`Migratory status` == "Not a Migrant", seasonal])

#How many species overlapped before filtering?
x2 <-unique(r_exulans_DIISE_bird_overlap$scientificName) #23 species (excluding extinct species, otherwise is 31) out of 116 species

#Filter out from the overlap the resident range of those species that are fully migrant since the overlap is potentially a false positive
r_exulans_DIISE_bird_overlap <- r_exulans_DIISE_bird_overlap[`Migratory status` != "Full migrant" | !seasonal %in% c(1,3,4,5), ]
unique(r_exulans_DIISE_bird_overlap$scientificName) #Instead of 31 species or 23, 16 truly overlapped
ex_r_exulans <- setdiff(x2, unique(r_exulans_DIISE_bird_overlap$scientificName)) 
ex_r_exulans #Excluded spp: "Procellaria parkinsoni" "Procellaria westlandica"  "Pseudobulweria macgillivrayi" "Pseudobulweria rostrata" "Pterodroma sandwichensis" "Puffinus newelli" "Sterna striata"       

#How many bird species distribution overlap with the pacific rat eradications?
length(unique(r_exulans_DIISE_bird_overlap$scientificName)) #16 species out of 116 species!

#How many islands?
length(unique(r_exulans_DIISE_bird_overlap$Island.Name)) #80 islands! (instead of 151 or 96)

#How many eradications?
length(unique(r_exulans_DIISE_bird_overlap$Eradication.ID)) #85 eradications! (instead of 164 or 101)

#To make the maps, plot the eradications with the best possible evidence 
summary_DIISE_bird_r_exulans <- r_exulans_DIISE_bird_overlap[, .(
  n_eradications = uniqueN(Eradication.ID)),
  by = .(Island.Name, `Status (Eradication)`)] 

summary_DIISE_bird_r_exulans <- summary_DIISE_bird_r_exulans[, .(
  n_erad_succ = sum(n_eradications[`Status (Eradication)` == "Successful"]),
  n_erad_not_succ = sum(n_eradications[`Status (Eradication)` != "Successful"]),
  total_erad = sum(n_eradications)),
  by = Island.Name]

#Count number of threatened bird species per island
threat_count <- r_exulans_DIISE_bird_overlap[, .(
  n_threat_birds = uniqueN(scientificName)),
  by = Island.Name]

r_exulans_DIISE_bird_overlap <- r_exulans_DIISE_bird_overlap[order(Island.Name,  -as.integer(synth_col))] #, -as.integer(`Status (Eradication)`)
best_evidence_per_island <- r_exulans_DIISE_bird_overlap[, .SD[1], by = Island.Name]  

#Join the three of them
r_exulans_island <- merge(summary_DIISE_bird_r_exulans, threat_count, by = "Island.Name", all.x = TRUE)
r_exulans_island <- merge(r_exulans_island, best_evidence_per_island, by = "Island.Name", all.x = TRUE)
r_exulans_island<- r_exulans_island[,.(
  Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, n_erad_succ, n_erad_not_succ, total_erad, n_threat_birds,
  scientificName, redlistCategory, `Migratory status`, distribution, single_island_endemic,
  synth_col, Rodent_attributed_final, Archipelago , Region, Country, geom
)]

## Brown rats -----------
DIISE_r_norvegicus<-data_DIISE[`Scientific Name`=="Rattus norvegicus"]
unique(DIISE_r_norvegicus$`Eradication ID`) #there are 296 eradications
unique(DIISE_r_norvegicus$`Island Name`) #in 248 islands
table(DIISE_r_norvegicus$`Status (Eradication)`) #out of 296, 181 were successful
freq_df_r_norvegicus<-frequency_df[Rodent_attributed_final=="Rattus norvegicus"] 

summary_r_norvegicus <-freq_df_r_norvegicus[, .N, by = synth_col]
summary_r_norvegicus[, perc := (N / sum(N))*100]
summary_r_norvegicus
#74.15% bird species have no studies and 25.84% have studies, of these,
# No study in support: 7.3%, Only predation in support:12.92%, Lethal program in support: 1.68%, Population study without data in support:2.80%
#Population study with data in support:0.56%, Population study with all qualities in support: 0.56%

freq_r_norvegicus_no_studies <- frequency_df[Rodent_attributed_final=="Rattus norvegicus" & synth_col=="No studies found"]
studies_r_norvegicus<- freq_studies[Rodent_attributed_final=="Rattus norvegicus"]

#For the species with studies, add the location (from the dataset that matches the DISSE format)
merge_studies_r_norvegicus<- merge(studies_r_norvegicus,
                                   Studies_location_uniq[Study_rodent_final=="Rattus norvegicus"],
                                   by= "scientificName")

#Filter the combinations of study and evidence that are duplicated 
merge_studies_r_norvegicus <- merge_studies_r_norvegicus[
  !(
    (synth_col =="Population study with all qualities in support"  & Hypothesis_supported == 0) |
      (synth_col == "Population study with data in support" & Hypothesis_supported == 0) |
      (synth_col == "Only predation in support" & Hypothesis_supported == 0) |
      (synth_col == "Lethal program in support" & Hypothesis_supported == 0) 
  )
]

unique(merge_studies_r_norvegicus$scientificName) #46 species.

#Join with the no studies data, to have a single df for the bird species associated with brown rat
merge_r_norvegicus<-rbindlist(list(merge_studies_r_norvegicus[,Study_rodent_final:=NULL], freq_r_norvegicus_no_studies), use.names = TRUE, fill = TRUE)

#Bird species list
birds_r_norvegicus_review<- unique(merge_r_norvegicus$scientificName)

threatened_species_query <- paste0("SELECT * FROM all_species WHERE sci_name IN ('", 
                                   paste(birds_r_norvegicus_review, collapse = "','"), "')")

#Load bird species distribution and perform the spatial intersection with eradication programs
redo <- FALSE

if (redo) {
  
  bird_distribution_r_norvegicus <- st_read("databases/BirdLifemaps/BOTW_2024_2.gpkg", query = threatened_species_query)
  
  eradications_r_norvegicus<- st_as_sf(DIISE_r_norvegicus, coords = c("Longitude", "Latitude"), crs = 4326)
  
  eradications_r_norvegicus <- st_transform(eradications_r_norvegicus, st_crs(bird_distribution_r_norvegicus))
  
  bird_distribution_r_norvegicus <- st_make_valid(bird_distribution_r_norvegicus)
  
  r_norvegicus_DIISE_bird_overlap <- st_intersection(
  bird_distribution_r_norvegicus,
  eradications_r_norvegicus %>% select(Country, Region, Archipelago, `Island Name`, `Eradication ID`) # select only relevant columns
)
  
  saveRDS(r_norvegicus_DIISE_bird_overlap, "builds/eradication_programs/r_norvegicus_DIISE_bird_overlap.rds")
  
} else {
  
  r_norvegicus_DIISE_bird_overlap<-readRDS("builds/eradication_programs/r_norvegicus_DIISE_bird_overlap.rds")
  
}

#Add the eradication type
r_norvegicus_DIISE_bird_overlap <- merge(r_norvegicus_DIISE_bird_overlap, DIISE_r_norvegicus[,.(`Eradication ID`, `Status (Eradication)`)], by.x = "Eradication.ID", by.y = "Eradication ID")

#Clean the df
setDT(r_norvegicus_DIISE_bird_overlap)
r_norvegicus_DIISE_bird_overlap<- r_norvegicus_DIISE_bird_overlap[, .(
  sci_name, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name,Archipelago, Region, Country, geom
)]

setnames(r_norvegicus_DIISE_bird_overlap, "sci_name", "scientificName")

#Add the evidence column for that species
r_norvegicus_DIISE_bird_overlap<-merge(r_norvegicus_DIISE_bird_overlap,
                                       unique(merge_r_norvegicus[,.(scientificName, synth_col, redlistCategory, `Migratory status`, distribution, single_island_endemic)]),
                                       by= "scientificName")
#and reorder columns
r_norvegicus_DIISE_bird_overlap<- r_norvegicus_DIISE_bird_overlap[,.(
  scientificName, synth_col, redlistCategory, `Migratory status`, distribution, single_island_endemic, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, Archipelago, Region, Country, geom
)]

#This df includes data of the review, rodent eradication programs and birds distribution
r_norvegicus_DIISE_bird_overlap[, Rodent_attributed_final := "Rattus norvegicus"]

#Exclude extinct species
table(r_norvegicus_DIISE_bird_overlap$redlistCategory)
r_norvegicus_DIISE_bird_overlap <- r_norvegicus_DIISE_bird_overlap[redlistCategory != "Extinct", ]

#Reduce false positives in the overlap 
table(r_norvegicus_DIISE_bird_overlap$seasonal) #87 rows with seasonal code 4. Exclude them
r_norvegicus_DIISE_bird_overlap <- r_norvegicus_DIISE_bird_overlap[seasonal != 4, ]
table(r_norvegicus_DIISE_bird_overlap[`Migratory status` == "Full migrant", seasonal])
table(r_norvegicus_DIISE_bird_overlap[`Migratory status` == "Not a Migrant", seasonal]) #There are non-migrant species with non-breeding ranges. Check if they should be included.

non_migrants_seasonal3_r_norvegicus <-unique(
  r_norvegicus_DIISE_bird_overlap[`Migratory status` == "Not a Migrant" & seasonal == 3, scientificName]) #"Himantopus novaezelandiae" "Ptychoramphus aleuticus"

species_query <- paste0("SELECT * FROM all_species WHERE sci_name IN ('",
                        paste(non_migrants_seasonal3_r_norvegicus, collapse = "','"), "')")

range_sf <- st_read("~/Sixth mass extinction/Luna_project/Databases/BirdLifemaps/BOTW_2024_2.gpkg",
                    query = species_query)

range_sf <- st_make_valid(range_sf)

library(dplyr)
library(tidyverse)

area_by_season <- range_sf %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  .[, .(total_area_km2 = sum(area_km2)), by = .(sci_name, seasonal)]

# reshape wide for easy comparison
wide <- dcast(area_by_season, sci_name ~ seasonal, value.var = "total_area_km2")
setnames(wide, c("sci_name", "seasonal_1", "seasonal_2", "seasonal_3"))

wide[seasonal_3 > 5000] #Ptychoramphus aleuticus seasonal 3 range shouldn't be included
not_included <- r_norvegicus_DIISE_bird_overlap[scientificName == "Ptychoramphus aleuticus" & seasonal ==3, ]

#How many species overlapped before filtering?
x3 <-unique(r_norvegicus_DIISE_bird_overlap$scientificName) #34 (including ex:38) species out of 178 species

#Filter out from the overlap the resident range of those species that are fully migrant since the overlap is potentially a false positive
r_norvegicus_DIISE_bird_overlap <- r_norvegicus_DIISE_bird_overlap[`Migratory status` != "Full migrant" | !seasonal %in% c(1,3,4,5), ]
r_norvegicus_DIISE_bird_overlap <- r_norvegicus_DIISE_bird_overlap[!not_included, on = c("scientificName", "seasonal")]

unique(r_norvegicus_DIISE_bird_overlap$scientificName) #Instead of 38 or 26 species, 22 truly overlapped
ex_r_norvegicus <- setdiff(x3, unique(r_norvegicus_DIISE_bird_overlap$scientificName)) 
ex_r_norvegicus
#"Ardenna carneipes" "Ardenna creatopus" "Calonectris leucomelas" "Chlidonias albostriatus" "Eudyptes pachyrhynchus" "Larus bulleri"
#"Procellaria cinerea" "Procellaria westlandica" "Pterodroma cahow" "Pterodroma hasitata" "Puffinus mauretanicus" "Sterna striata"

#How many bird species distribution overlap with the brown rat eradications?
length(unique(r_norvegicus_DIISE_bird_overlap$scientificName)) #22 out of 178 species!

#How many islands?
length(unique(r_norvegicus_DIISE_bird_overlap$Island.Name)) #143 islands! (instead of 237 or 146)

#How many eradications?
length(unique(r_norvegicus_DIISE_bird_overlap$Eradication.ID)) #174 eradications! (instead of 283 or 178)

#To make the maps, plot the eradications with the best possible evidence 
summary_DIISE_bird_r_norvegicus <- r_norvegicus_DIISE_bird_overlap[, .(
  n_eradications = uniqueN(Eradication.ID)),
  by = .(Island.Name, `Status (Eradication)`)] 

summary_DIISE_bird_r_norvegicus <- summary_DIISE_bird_r_norvegicus[, .(
  n_erad_succ = sum(n_eradications[`Status (Eradication)` == "Successful"]),
  n_erad_not_succ = sum(n_eradications[`Status (Eradication)` != "Successful"]),
  total_erad = sum(n_eradications)),
  by = Island.Name]

#Count number of threatened bird species per island
threat_count <- r_norvegicus_DIISE_bird_overlap[, .(
  n_threat_birds = uniqueN(scientificName)),
  by = Island.Name]

r_norvegicus_DIISE_bird_overlap <- r_norvegicus_DIISE_bird_overlap[order(Island.Name, -as.integer(synth_col))] #, -as.integer(`Status (Eradication)`)
best_evidence_per_island <- r_norvegicus_DIISE_bird_overlap[, .SD[1], by = Island.Name]  

#Join the three of them
r_norvegicus_island <- merge(summary_DIISE_bird_r_norvegicus, threat_count, by = "Island.Name", all.x = TRUE)
r_norvegicus_island <- merge(r_norvegicus_island, best_evidence_per_island, by = "Island.Name", all.x = TRUE)
r_norvegicus_island<- r_norvegicus_island[,.(
  Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, n_erad_succ, n_erad_not_succ, total_erad, n_threat_birds,
  scientificName, redlistCategory, `Migratory status`, distribution, single_island_endemic,
  synth_col, Rodent_attributed_final, Archipelago , Region, Country, geom
)]

## Black rats -----------
DIISE_r_rattus<-data_DIISE[`Scientific Name`=="Rattus rattus"]
unique(DIISE_r_rattus$`Eradication ID`) #there are 513 eradications
unique(DIISE_r_rattus$`Island Name`) #in 417 islands
table(DIISE_r_rattus$`Status (Eradication)`) #out of 513, 285 were successful
freq_df_r_rattus<-frequency_df[Rodent_attributed_final=="Rattus rattus"]

summary_r_rattus <-freq_df_r_rattus[, .N, by = synth_col]
summary_r_rattus[, perc := (N / sum(N))*100]
summary_r_rattus
#62.98% bird species have no studies and 37.02% have studies, of these,
#No study in support: 11.03%, Only predation: 12.09%, Lethal program in support: 2.84%, Population study without data in support:6.40%
#Population study with data in support: 3.20%, Population study with all qualities in support: 1.42% 

freq_r_rattus_no_studies <- frequency_df[Rodent_attributed_final=="Rattus rattus" & synth_col=="No studies found"]
studies_r_rattus<- freq_studies[Rodent_attributed_final=="Rattus rattus"]

#For the species with studies, add the location
merge_studies_r_rattus<- merge(studies_r_rattus,
                               Studies_location_uniq[Study_rodent_final=="Rattus rattus"],
                               by= "scientificName")

#Filter the combinations of study and evidence that are duplicated
merge_studies_r_rattus <- merge_studies_r_rattus[
  !(
    (synth_col =="Population study with all qualities in support"  & Hypothesis_supported == 0) |
      (synth_col == "Population study with data in support" & Hypothesis_supported == 0) |
      (synth_col == "Only predation in support" & Hypothesis_supported == 0) |
      (synth_col == "Lethal program in support" & Hypothesis_supported == 0) 
  )
]

unique(merge_studies_r_rattus$scientificName) #105 species.

#Join with the no studies data, to have a single df for the bird species associated with black rat
merge_r_rattus<-rbindlist(list(merge_studies_r_rattus[,Study_rodent_final:=NULL], freq_r_rattus_no_studies), use.names = TRUE, fill = TRUE)

#Bird species list
birds_r_rattus_review<- unique(merge_r_rattus$scientificName)

threatened_species_query <- paste0("SELECT * FROM all_species WHERE sci_name IN ('", 
                                   paste(birds_r_rattus_review, collapse = "','"), "')")

#Load bird species distribution and perform the spatial intersection with eradication programs
redo <- FALSE

if (redo) {
  
bird_distribution_r_rattus <- st_read("databases/BirdLifemaps/BOTW_2024_2.gpkg", query = threatened_species_query)  
  
eradications_r_rattus<- st_as_sf(DIISE_r_rattus, coords = c("Longitude", "Latitude"), crs = 4326)

eradications_r_rattus <- st_transform(eradications_r_rattus, st_crs(bird_distribution_r_rattus))

bird_distribution_r_rattus <- st_make_valid(bird_distribution_r_rattus)

r_rattus_DIISE_bird_overlap <- st_intersection(
  bird_distribution_r_rattus,
  eradications_r_rattus %>% select(Country, Region, Archipelago, `Island Name`, `Eradication ID`) # select only relevant columns
)
 
saveRDS(r_rattus_DIISE_bird_overlap, "builds/eradication_programs/r_rattus_DIISE_bird_overlap.rds")

} else {
  
  r_rattus_DIISE_bird_overlap<-readRDS("builds/eradication_programs/r_rattus_DIISE_bird_overlap.rds")
  
}

#Add the eradication type
r_rattus_DIISE_bird_overlap <- merge(r_rattus_DIISE_bird_overlap, DIISE_r_rattus[,.(`Eradication ID`, `Status (Eradication)`)], by.x = "Eradication.ID", by.y = "Eradication ID")

#Clean the df
setDT(r_rattus_DIISE_bird_overlap)
r_rattus_DIISE_bird_overlap<- r_rattus_DIISE_bird_overlap[, .(
  sci_name, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name,Archipelago, Region, Country, geom
)]

setnames(r_rattus_DIISE_bird_overlap, "sci_name", "scientificName")

#Add the evidence column for that species
r_rattus_DIISE_bird_overlap<-merge(r_rattus_DIISE_bird_overlap,
                                   unique(merge_r_rattus[,.(scientificName, synth_col, redlistCategory, `Migratory status`, distribution, single_island_endemic)]),
                                   by= "scientificName")
#and reorder columns
r_rattus_DIISE_bird_overlap<- r_rattus_DIISE_bird_overlap[,.(
  scientificName, synth_col, redlistCategory, `Migratory status`, distribution, single_island_endemic, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, Archipelago, Region, Country, geom
)]

#This df includes data of the review, rodent eradication programs and birds distribution
r_rattus_DIISE_bird_overlap[, Rodent_attributed_final := "Rattus rattus"]

#Exclude ex species
table(r_rattus_DIISE_bird_overlap$redlistCategory)
r_rattus_DIISE_bird_overlap<- r_rattus_DIISE_bird_overlap[redlistCategory != "Extinct", ]

#Reduce false positives in the overlap 
table(r_rattus_DIISE_bird_overlap$seasonal) #88 rows with seasonal code 4. Exclude them
r_rattus_DIISE_bird_overlap <- r_rattus_DIISE_bird_overlap[seasonal != 4, ]

table(r_rattus_DIISE_bird_overlap[`Migratory status` == "Full migrant", seasonal])
table(r_rattus_DIISE_bird_overlap[`Migratory status` == "Not a Migrant", seasonal]) #There are non-migrant species with non-breeding ranges. Check if they should be included.

non_migrants_seasonal3_r_rattus <-unique(
  r_rattus_DIISE_bird_overlap[`Migratory status` == "Not a Migrant" & seasonal == 3, scientificName])

non_migrants_seasonal3_r_rattus #"Ptychoramphus aleuticus" "Spheniscus mendiculus" "Synthliboramphus craveri" "Synthliboramphus hypoleucus" "Synthliboramphus scrippsi"  

species_query <- paste0("SELECT * FROM all_species WHERE sci_name IN ('",
                        paste(non_migrants_seasonal3_r_rattus, collapse = "','"), "')")

range_sf <- st_read("~/Sixth mass extinction/Luna_project/Databases/BirdLifemaps/BOTW_2024_2.gpkg",
                    query = species_query)

range_sf <- st_make_valid(range_sf)

area_by_season <- range_sf %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  .[, .(total_area_km2 = sum(area_km2)), by = .(sci_name, seasonal)]

# reshape wide for easy comparison
wide <- dcast(area_by_season, sci_name ~ seasonal, value.var = "total_area_km2")
setnames(wide, c("sci_name", "seasonal_1", "seasonal_2", "seasonal_3"))

wide[seasonal_3 > 5000, sci_name] #4 spp with seasonal 3 range shouldn't be included
not_included <- r_rattus_DIISE_bird_overlap[scientificName %in% wide[seasonal_3 > 5000, sci_name] & seasonal ==3, ]

#How many species overlapped before filtering?
x4 <-unique(r_rattus_DIISE_bird_overlap$scientificName) #76 species (84 including ex) out of 281 species

#Filter out from the overlap the resident range of those species that are fully migrant since the overlap is potentially a false positive
r_rattus_DIISE_bird_overlap <- r_rattus_DIISE_bird_overlap[`Migratory status` != "Full migrant" | !seasonal %in% c(1,3,4,5), ]
r_rattus_DIISE_bird_overlap <- r_rattus_DIISE_bird_overlap[!not_included, on = c("scientificName", "seasonal")]

unique(r_rattus_DIISE_bird_overlap$scientificName) #Instead of 84 species, 52 truly overlapped
ex_r_rattus <- setdiff(x4, unique(r_rattus_DIISE_bird_overlap$scientificName)) 
ex_r_rattus
# "Ardenna creatopus"          "Ardeola idae"               "Bulweria fallax"            "Calonectris leucomelas"    
# "Chlidonias albostriatus"    "Diomedea dabbenena"         "Hydrobates matsudairae"     "Larus heermanni"           
# "Nesofregetta fuliginosa"    "Pachyptila macgillivrayi"   "Phoebetria fusca"           "Procellaria aequinoctialis"
# "Procellaria parkinsoni"     "Procellaria westlandica"    "Pseudobulweria rostrata"    "Pterodroma baraui"         
# "Pterodroma cahow"           "Pterodroma externa"         "Pterodroma hasitata"        "Pterodroma sandwichensis"  
# "Puffinus newelli"           "Puffinus opisthomelas"      "Sterna striata"             "Thalasseus elegans"   

#How many bird species distribution overlap with the black rat eradications?
length(unique(r_rattus_DIISE_bird_overlap$scientificName)) #52 (60) out of 283 species!

#How many islands?
length(unique(r_rattus_DIISE_bird_overlap$Island.Name)) #153 islands! (instead of 381 or 157)

#How many eradications?
length(unique(r_rattus_DIISE_bird_overlap$Eradication.ID)) #182 eradications! (instead of 454 or 188)

#To make the maps, plot the eradications with the best possible evidence
summary_DIISE_bird_r_rattus <- r_rattus_DIISE_bird_overlap[, .(
  n_eradications = uniqueN(Eradication.ID)),
  by = .(Island.Name, `Status (Eradication)`)] 

summary_DIISE_bird_r_rattus <- summary_DIISE_bird_r_rattus[, .(
  n_erad_succ = sum(n_eradications[`Status (Eradication)` == "Successful"]),
  n_erad_not_succ = sum(n_eradications[`Status (Eradication)` != "Successful"]),
  total_erad = sum(n_eradications)),
  by = Island.Name]

#Count number of threatened bird species per island
threat_count <- r_rattus_DIISE_bird_overlap[, .(
  n_threat_birds = uniqueN(scientificName)),
  by = Island.Name]

r_rattus_DIISE_bird_overlap <- r_rattus_DIISE_bird_overlap[order(Island.Name,  -as.integer(synth_col))] #, -as.integer(`Status (Eradication)`)

best_evidence_per_island <- r_rattus_DIISE_bird_overlap[, .SD[1], by = Island.Name]  

#Join the three of them
r_rattus_island <- merge(summary_DIISE_bird_r_rattus, threat_count, by = "Island.Name", all.x = TRUE)
r_rattus_island <- merge(r_rattus_island, best_evidence_per_island, by = "Island.Name", all.x = TRUE)
r_rattus_island<- r_rattus_island[,.(
  Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, n_erad_succ, n_erad_not_succ, total_erad, n_threat_birds,
  scientificName, redlistCategory, `Migratory status`, distribution, single_island_endemic,
  synth_col, Rodent_attributed_final, Archipelago , Region, Country, geom
)]

######
# # for species classified as non-migrant, do codes 1 and 3 ever
# # represent non-overlapping/distinct areas rather than duplicates?
# non_migrants_with_code3 <- m_musculus_DIISE_bird_overlap[
#   seasonal == 3 & `Migratory status` == "Not a Migrant",  ]
# nrow(non_migrants_with_code3)
# 
# non_migrants_with_code3 <- r_exulans_DIISE_bird_overlap[
#   seasonal == 3 & `Migratory status` == "Not a Migrant",  ]
# nrow(non_migrants_with_code3)
# 
# non_migrants_with_code3 <- r_norvegicus_DIISE_bird_overlap[
#   seasonal == 3 & `Migratory status` == "Not a Migrant",  ]
# nrow(non_migrants_with_code3)
# unique(non_migrants_with_code3$scientificName)
# 
# non_migrants_with_code3 <- r_rattus_DIISE_bird_overlap[
#   seasonal == 3 & `Migratory status` == "Not a Migrant",  ]
# nrow(non_migrants_with_code3)
# unique(non_migrants_with_code3$scientificName)
# 
# non_migrants_with_code3_spp <-c("Spheniscus mendiculus", "Ptychoramphus aleuticus",
#   "Himantopus novaezelandiae", "Synthliboramphus craveri",
#   "Synthliboramphus hypoleucus", "Synthliboramphus scrippsi")
# 
# species_query <- paste0("SELECT * FROM all_species WHERE sci_name IN ('",
#                         paste(non_migrants_with_code3_spp, collapse = "','"), "')")
# 
# range_sf <- st_read("~/Sixth mass extinction/Luna_project/Databases/BirdLifemaps/BOTW_2024_2.gpkg",
#                                    query = species_query)
# 
# range_sf <- st_make_valid(range_sf)
# 
# area_by_season <- range_sf %>%
#   mutate(area_km2 = as.numeric(st_area(.)) / 1e6) %>%
#   st_drop_geometry() %>%
#   as.data.table() %>%
#   .[, .(total_area_km2 = sum(area_km2)), by = .(sci_name, seasonal)]
# 
# # reshape wide for easy comparison
# wide <- dcast(area_by_season, sci_name ~ seasonal, value.var = "total_area_km2")
# setnames(wide, c("sci_name", "area_1", "area_2", "area_3"))
# 
# # visualize where the natural break falls across ALL species, not just these 6
# hist(log10(wide$area_3), breaks = 30, 
#      xlab = "log10(non-breeding range area, km²)")
# 
# 
# excluded <- c(ex_m_musculus, ex_r_exulans, ex_r_norvegicus, ex_r_rattus)
# excluded <- unique(excluded)
# excluded 
# 
# unique(m_musculus_DIISE_bird_overlap[scientificName %in% excluded, scientificName])
# unique(r_exulans_DIISE_bird_overlap[scientificName %in% excluded, scientificName])
# unique(r_norvegicus_DIISE_bird_overlap[scientificName %in% excluded, scientificName])
# unique(r_rattus_DIISE_bird_overlap[scientificName %in% excluded, scientificName])
# 
# not_truly_excluded <- c("Diomedea dabbenena", "Phoebetria fusca", "Procellaria cinerea",
#                         "Eudyptes pachyrhynchus", "Nesofregetta fuliginosa",
#                         "Hydrobates leucorhous",
#                         "Ardenna carneipes", "Puffinus mauretanicus")
# 
# really_excluded <- setdiff(excluded, not_truly_excluded) 
# really_excluded #when accounting for migratory species that have huge ranges, 24 species are not part of the overlap anymore. This reduced false positive results.

#Load the maps of these species to confirm the range -> CONFIRMED!! THIS SPP HAVE THE LARGE RANGE AS SEASONAL = 1.
# excluded_species_query <- paste0("SELECT * FROM all_species WHERE sci_name IN ('",
#                                  paste(really_excluded, collapse = "','"), "')")
# 
# bird_distribution_excluded <- st_read(
#   "~/Sixth mass extinction/Luna_project/Databases/BirdLifemaps/BOTW_2024_2.gpkg",
#   query = excluded_species_query)
# 
# ggplot() +
#   geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +
#   geom_sf(data = bird_distribution_excluded[bird_distribution_excluded$sci_name == "Procellaria aequinoctialis", ], 
#           aes(color = factor(seasonal))) +
#   scale_color_brewer(palette = "Set1")
# 
# library(mapview)
# mapview(bird_distribution_excluded[bird_distribution_excluded$sci_name == "Diomedea dabbenena", zcol= "seasonal"])

# 5. Summary  --------------------------------------------------
# -- For overall DIISE eradications --
length(unique(data_DIISE$`Eradication ID`)) #1104 eradications
unique(data_DIISE$`Island Name`) #794 islands
table(data_DIISE$`Status (Eradication)`) #Successful: 645 

sum_DIISE <- data_DIISE[, .(
  count = .N
), by = `Status (Eradication)`][
  , proportion := (count / sum(count))*100
  ]

sum_DIISE #'*There have been 1104 rodent eradication on 794 islands, of which 58.4% were successful.*

# -- Overlapped points of rodent eradication and threatened bird ranges --
Rodents_overlap_data<- rbindlist(list(
  r_rattus_DIISE_bird_overlap,
  r_norvegicus_DIISE_bird_overlap,
  r_exulans_DIISE_bird_overlap,
  m_musculus_DIISE_bird_overlap
))

Rodents_overlap_data$synth_col <- factor(Rodents_overlap_data$synth_col, 
                                         levels = rev(levels(Rodents_overlap_data$synth_col)))

## All species -----------
length(unique(Rodents_overlap_data$scientificName)) #76 instead of 111 or 87 species
length(unique(Rodents_overlap_data$Island.Name)) #340 instead of 719 or 353islands
length(unique(Rodents_overlap_data$Eradication.ID)) #454 instead of 948 or 480 eradications 
#'*A total of 480 rodent eradication programs were conducted within the range of 87 attributed bird species*

#Count and proportion of each evidence type overall
sum_overlap <- unique(Rodents_overlap_data[, .(Eradication.ID, `Status (Eradication)`, Rodent_attributed_final, synth_col)])
sum_overlap <- sum_overlap[order(Eradication.ID, as.integer(synth_col))]
sum_overlap <- sum_overlap[, .SD[1], by = .(Eradication.ID)] 

sum_overlap <- sum_overlap[, .(
  .N
), by = synth_col][
  , Proportion :=  round((N / sum(N)) * 100, digits = 2)
]

sum_overlap 
#'*Of these birds - on whose behalf eradications were conducted - we found:*
#'*no studies connecting them to rodents for 40.53%;* 
#'*only studies not in support for 12.11%;* 
#'*only evidence that predation occurs for 5.73%;* 
#'*at least one population study in support, but without data, for 21.81%;* 
#'*at least one population study in support with data for 11.67%;* 
#'*and at least one population study with data and qualities for 8.15%.*
setnames(sum_overlap, "synth_col", "Evidence")
setorder(sum_overlap, -Evidence)
sum_overlap
sum(sum_overlap$N) #correct! 454 eradications

#Count and proportion of each evidence type per rodent species
sum_rodent <- unique(Rodents_overlap_data[, .(Eradication.ID, `Status (Eradication)`, Rodent_attributed_final, synth_col)])
sum_rodent <- sum_rodent[order(Eradication.ID, as.integer(synth_col))]
sum_rodent <- sum_rodent[, .SD[1], by = Eradication.ID]
sum_rodent <- sum_rodent[, .N, by = .(Rodent_attributed_final, synth_col)][
  , Proportion := round((N / sum(N)) * 100, digits = 2), by = Rodent_attributed_final
]

sum_rodent
setnames(sum_rodent, "synth_col", "Evidence")
setnames(sum_rodent, "Rodent_attributed_final", "Rodent attributed")
setorder(sum_rodent, -Evidence)
sum_rodent

sum(sum_rodent$N) #correct! 454 eradications
sum(sum_rodent[Evidence == "All studies are not in support", N]) #Should be 55
sum(sum_rodent[Evidence == "Population study without data in support", N]) #Should be 99

## All species by eradication status ----------- 
#Count and proportion of each evidence type overall
sum_overlap_erad_status <- unique(Rodents_overlap_data[, .(Eradication.ID, `Status (Eradication)`, Rodent_attributed_final, synth_col)])
sum_overlap_erad_status <- sum_overlap_erad_status[order(Eradication.ID, as.integer(synth_col))]
sum_overlap_erad_status <- sum_overlap_erad_status[, .SD[1], by = .(Eradication.ID)]

sum_overlap_erad_status <- sum_overlap_erad_status[, .(
  .N
), by = .(synth_col, `Status (Eradication)`)][
  , Proportion := round((N / sum(N)) * 100, digits = 2)
]

setnames(sum_overlap_erad_status, "synth_col", "Evidence")
setorder(sum_overlap_erad_status, -`Status (Eradication)`, -Evidence)
sum_overlap_erad_status

sum(sum_overlap_erad_status[Evidence == "All studies are not in support", N]) #okay
sum(sum_overlap_erad_status[Evidence == "Population study without data in support", N]) #okay

#Count and proportion of each eradication type
sum_rodent_erad_status <- unique(Rodents_overlap_data[, .(Eradication.ID, `Status (Eradication)`, Rodent_attributed_final, synth_col)])
sum_rodent_erad_status <- sum_rodent_erad_status[order(Eradication.ID, as.integer(synth_col))]
sum_rodent_erad_status <- sum_rodent_erad_status[, .SD[1], by = Eradication.ID]
sum_rodent_erad_status <- sum_rodent_erad_status[, .N, by = .(Rodent_attributed_final, synth_col,`Status (Eradication)`)][
  , Proportion := round((N / sum(N)) * 100, digits = 2), by = Rodent_attributed_final
]

sum_rodent_erad_status
setnames(sum_rodent_erad_status, "synth_col", "Evidence")
setnames(sum_rodent_erad_status, "Rodent_attributed_final", "Rodent attributed")
setorder(sum_rodent_erad_status, -`Status (Eradication)`, -Evidence)

sum(sum_rodent_erad_status[`Rodent attributed` == "Rattus exulans", Proportion]) #Should be 100%
sum(sum_rodent_erad_status$N) #Perfect!
sum(sum_rodent_erad_status[Evidence == "All studies are not in support", N]) #okay
sum(sum_rodent_erad_status[Evidence == "Population study without data in support", N]) #okay

# ggplot(Rodents_overlap_data,
#        aes(reorder(`Status (Eradication)`, `Status (Eradication)`, function(x) -length(x)),
#            fill=synth_col, color=synth_col)) +
#   geom_bar(stat = "count", linewidth=.75) +
#   facet_grid(~Rodent_attributed_final)+
#   scale_fill_manual(values = plot_col)+
#   scale_color_manual(values = col_pal) +
#   labs(color = "", fill="")+
#   ylab("Number of eradications")+
#   xlab("")+
#   scale_x_discrete(labels = c(c("Rattus rattus" = "Black rats", 
#                                 "Rattus norvegicus" = "Brown rats",
#                                 "Rattus exulans" = "Pacific rats",
#                                 "Mus musculus" = "House mouse")))+
#   
#   theme_lundy+
#   theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))

rodent_names <- c("Rattus rattus" = "Black rats", 
                        "Rattus norvegicus" = "Brown rats",
                        "Rattus exulans" = "Pacific rats",
                        "Mus musculus" = "House mouse")

status_order <- sum_rodent_erad_status[, .(total = sum(N)), by = `Status (Eradication)`][order(-total), `Status (Eradication)`]

ggplot(sum_rodent_erad_status,
       aes(x = factor(`Status (Eradication)`, levels = status_order), y = N,
           fill = Evidence, color = Evidence)) +
  geom_col(linewidth=.75) +
  facet_grid(~`Rodent attributed`,labeller = labeller(`Rodent attributed` = rodent_names))+
  scale_fill_manual(values = plot_col)+
  scale_color_manual(values = col_pal) +
  labs(color = "", fill="")+
  ylab("Number of eradications")+
  xlab("")+
  theme_lundy+
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0))

ggsave("figures/eradication_programs/evidence_by_status.pdf", plot = last_plot(), device = cairo_pdf, 
       width = 11.46, height = 8.30)

## All extant species -----------
#(excluding extinct species)
#' table(Rodents_overlap_data$redlistCategory) #281 rows of extinct species
#' 
#' #Count and proportion of each evidence type overall
#' sum_overlap_extant <- Rodents_overlap_data[redlistCategory != "Extinct", ]
#' sum_overlap_extant <- unique(sum_overlap_extant[, .(Eradication.ID, `Status (Eradication)`, Rodent_attributed_final, synth_col)])
#' sum_overlap_extant <- sum_overlap_extant[order(Eradication.ID, as.integer(synth_col))]
#' sum_overlap_extant <- sum_overlap_extant[, .SD[1], by = .(Eradication.ID)]
#' 
#' sum_overlap_extant <- sum_overlap_extant[, .(.N), by = synth_col][
#'   , Proportion := round((N / sum(N)) * 100, digits = 2)
#' ]
#' setnames(sum_overlap_extant, "synth_col", "Evidence")
#' setorder(sum_overlap_extant, -Evidence)
#' 
#' #'*When extinct species are excluded the total number of eradications is reduced,*
#' #'*meaning that some of the points where ex species overlapped,*
#' #'*did not overlap with extant species*
#' sum_overlap_extant 
#' sum(sum_overlap_extant$N) #454 eradications
#' sum(sum_overlap_extant[Evidence == "All studies are not in support", N]) #Changed to 55
#' sum(sum_overlap_extant[Evidence == "Population study without data in support", N]) #Changed to 99
#' 
#' #Some eradications are completely lost when excluding extinct species
#' erad_ids_full <- unique(Rodents_overlap_data$Eradication.ID)
#' erad_ids_extant <- unique(Rodents_overlap_data[redlistCategory != "Extinct", Eradication.ID])
#' setdiff(erad_ids_full, erad_ids_extant)  
#' length(setdiff(erad_ids_full, erad_ids_extant)) #26
#' 
#' #Count and proportion of each evidence type per rodent species
#' sum_rodent_extant <- Rodents_overlap_data[redlistCategory != "Extinct", ]
#' sum_rodent_extant <- unique(sum_rodent_extant[, .(Eradication.ID, `Status (Eradication)`, Rodent_attributed_final, synth_col)])
#' sum_rodent_extant <- sum_rodent_extant[order(Eradication.ID, as.integer(synth_col))]
#' sum_rodent_extant <- sum_rodent_extant[, .SD[1], by = Eradication.ID]
#' sum_rodent_extant <- sum_rodent_extant[, .N, by = .(Rodent_attributed_final, synth_col)][
#'   , Proportion := round((N / sum(N)) * 100, digits = 2), by = Rodent_attributed_final
#' ]
#' 
#' sum_rodent_extant
#' setnames(sum_rodent_extant, "synth_col", "Evidence")
#' setnames(sum_rodent_extant, "Rodent_attributed_final", "Rodent attributed")
#' setorder(sum_rodent_extant, -Evidence)
#' sum_rodent_extant
#' 
#' sum(sum_rodent_extant$N) #The total is the same
#' sum(sum_rodent_extant[Evidence == "All studies are not in support", N]) #Changed to 55
#' sum(sum_rodent_extant[Evidence == "Population study without data in support", N]) #Changed to 99

## All extant multi-island endemics and regional species -----------
#(excluding extinct and single island endemics)
table(Rodents_overlap_data$single_island_endemic) 
table(Rodents_overlap_data$redlistCategory) 

#Count and proportion of each evidence type overall
sum_overlap_extant_no_sing_end <- Rodents_overlap_data[
  redlistCategory != "Extinct" & single_island_endemic != "yes"
  ,]
sum_overlap_extant_no_sing_end <- unique(sum_overlap_extant_no_sing_end[, .(Eradication.ID, `Status (Eradication)`, Rodent_attributed_final, synth_col)])
sum_overlap_extant_no_sing_end <- sum_overlap_extant_no_sing_end[order(Eradication.ID, as.integer(synth_col))]
sum_overlap_extant_no_sing_end <- sum_overlap_extant_no_sing_end[, .SD[1], by = .(Eradication.ID)]

sum_overlap_extant_no_sing_end <- sum_overlap_extant_no_sing_end[, .(.N), by = synth_col][
  , Proportion := round((N / sum(N)) * 100, digits = 2)
]

setnames(sum_overlap_extant_no_sing_end, "synth_col", "Evidence")
setorder(sum_overlap_extant_no_sing_end, -Evidence)

#'*When extinct species and single-endemic species are excluded*
#'*the total number of eradications is reduced, same as before*
#'*some of the points where ex species and single-endemic species overlapped,*
#'*did not overlap with extant species*
sum_overlap_extant_no_sing_end
sum(sum_overlap_extant_no_sing_end$N) #451 eradications
sum(sum_overlap_extant_no_sing_end[Evidence == "All studies are not in support", N]) #okay
sum(sum_overlap_extant_no_sing_end[Evidence == "Population study without data in support", N]) #okay

#How many eradications were lost?
erad_ids_full <- unique(Rodents_overlap_data$Eradication.ID)
erad_ids_extant <- unique(Rodents_overlap_data[redlistCategory != "Extinct" &
                                                 single_island_endemic != "yes", Eradication.ID])

setdiff(erad_ids_full, erad_ids_extant)  
length(setdiff(erad_ids_full, erad_ids_extant)) #3

table(Rodents_overlap_data[redlistCategory != "Extinct", single_island_endemic]) #yes:9
table(Rodents_overlap_data[single_island_endemic == "yes", redlistCategory]) #CR: 7 EN:2

sing_end_extant <- Rodents_overlap_data[redlistCategory != "Extinct" & single_island_endemic == "yes"]
sing_end_extant[, .(Eradication.ID, scientificName, synth_col)]

#For each of these eradications, what's the full set of species/evidence overlapping that point?
Rodents_overlap_data[Eradication.ID %in% sing_end_extant$Eradication.ID,
                     .(Eradication.ID, scientificName, synth_col, single_island_endemic)][order(Eradication.ID)]


#Count and proportion of each evidence type per rodent species
sum_rodent_extant_no_sing_end <- Rodents_overlap_data[
  redlistCategory != "Extinct" & single_island_endemic != "yes"
  ,]
sum_rodent_extant_no_sing_end <- unique(sum_rodent_extant_no_sing_end[, .(Eradication.ID, `Status (Eradication)`, Rodent_attributed_final, synth_col)])
sum_rodent_extant_no_sing_end <- sum_rodent_extant_no_sing_end[order(Eradication.ID, as.integer(synth_col))]
sum_rodent_extant_no_sing_end <- sum_rodent_extant_no_sing_end[, .SD[1], by = Eradication.ID]
sum_rodent_extant_no_sing_end <- sum_rodent_extant_no_sing_end[, .N, by = .(Rodent_attributed_final, synth_col)][
  , Proportion := round((N / sum(N)) * 100, digits = 2), by = Rodent_attributed_final
]

sum_rodent_extant_no_sing_end
setnames(sum_rodent_extant_no_sing_end, "synth_col", "Evidence")
setnames(sum_rodent_extant_no_sing_end, "Rodent_attributed_final", "Rodent attributed")
setorder(sum_rodent_extant_no_sing_end, -Evidence)
sum_rodent_extant_no_sing_end

sum(sum_rodent_extant_no_sing_end$N) #The total is the same
sum(sum_rodent_extant_no_sing_end[Evidence == "All studies are not in support", N]) #okay
sum(sum_rodent_extant_no_sing_end[Evidence == "Population study without data in support", N]) #okay

sum_erad_tables <- list(
  "summary" = sum_overlap,
  "sum_per_rodent" = sum_rodent,
  "sum_erad_status" = sum_overlap_erad_status,
  "sum_per_rodent_erad_status" = sum_rodent_erad_status,
  #"sum_extant" = sum_overlap_extant,
  #"sum_per_rodent_extant" = sum_rodent_extant,
  "sum_no_sing_end" = sum_overlap_extant_no_sing_end,
  "sum_per_rodent_no_sing_end" = sum_rodent_extant_no_sing_end
  )

writexl::write_xlsx(sum_erad_tables, "builds/eradication_programs/sum_erad_tables.xlsx")

#How many bird species (that overlap with eradications) have ‘population studies with data in support’ and ‘population studies with qualities in support’?
pop_data_sup<-Rodents_overlap_data[synth_col=="Population study with data in support"]
unique(pop_data_sup$scientificName)

pop_data_sup_all<-Rodents_overlap_data[synth_col=="Population study with all qualities in support"]
unique(pop_data_sup_all$scientificName)

## Frequency of support -----------
#number of studies in each evidence category and in support and not in support of the hypothesis (for those bird species that overlap with an eradication)
data_review[, value := ifelse(Hypothesis_supported == 0, -1, 1)]
data_review 

#Filter rodent sp different to the ones in our study
data_review  <- data_review[Rodent_attributed_final %in% rodent_sp]
unique(data_review [, .(Evidence_category, Evidence_effect, Evidence_method)])

#Create a continuous quality column
data_review [Evidence_category == "Predation", Evidence_quality := 1]
unique(data_review [is.na(Evidence_quality), .(Evidence_category, Evidence_effect, Evidence_method)])

data_review [Evidence_category == "Lethal program" &
               Evidence_effect == "Reproductive success", Evidence_quality := 2]

data_review [Evidence_category == "Lethal program" &
               Evidence_effect == "Abundance", Evidence_quality := 3]

data_review [Evidence_category == "Population" &
               Evidence_effect == "Reproductive success", Evidence_quality := 4]

data_review [Evidence_category == "Population" &
               Evidence_effect == "Abundance", Evidence_quality := 5]
unique(data_review [is.na(Evidence_quality), .(Evidence_category, Evidence_effect, Evidence_method)]) 

#Add a highlight column
unique(data_review [, .(Predator_and_prey_population_data, Experiment_control_site_or_spatial_variation,
                        Sample_size_greater_than_1, Experiment_confounding_variables_included)])

data_review [Predator_and_prey_population_data == 1 &
               Experiment_control_site_or_spatial_variation == 1 &
               Sample_size_greater_than_1 == 1 &
               Experiment_confounding_variables_included == 1,]

data_review [Predator_and_prey_population_data == 1 |
               Experiment_control_site_or_spatial_variation == 1 |
               Sample_size_greater_than_1 == 1 |
               Experiment_confounding_variables_included == 1,]

data_review [Predator_and_prey_population_data == 1 &
               Experiment_control_site_or_spatial_variation == 1 &
               Sample_size_greater_than_1 == 1 &
               Experiment_confounding_variables_included == 1,
             Quality_Study_Highlight := "Highlight"]

data_review [is.na(Quality_Study_Highlight), Quality_Study_Highlight := "No"]

data_review [, synth_evidence := ifelse(Evidence_category == "Population",
                                        paste(Evidence_category, Predator_and_prey_population_data, Quality_Study_Highlight),
                                        Evidence_category)]

#Filter this by the bird species with eradications
bird_sp_rodent_overlap <- unique(Rodents_overlap_data$scientificName) #76 bird species

data_review <-data_review [scientificName %in% bird_sp_rodent_overlap]

data_review [, Evidence_category_effect := ifelse(!Evidence_effect %in% c("Predation", "NONE"),
                                                  paste(Evidence_category, Evidence_effect),
                                                  Evidence_category)]

data_review [, Evidence_category_effect_data := ifelse(Evidence_category == "Population",
                                                       paste(Evidence_category_effect, Predator_and_prey_population_data),
                                                       Evidence_category_effect)]

study_freq <- data_review [, .(number_studies = sum(value)),
                           by = .(Evidence_category_effect, 
                                  Evidence_category_effect_data,
                                  Hypothesis_supported,
                                  Quality_Study_Highlight)]
study_freq

unique(study_freq$Evidence_category_effect)
study_freq$Evidence_category_effect <- factor(study_freq$Evidence_category_effect ,
                                              levels = c("NONE", "Predation", 
                                                         "Lethal program Reproductive success",
                                                         "Lethal program Abundance",
                                                         "Population Reproductive success",
                                                         "Population Abundance"))
unique(study_freq$Evidence_category_effect_data)
study_freq$Evidence_category_effect_data <- factor(study_freq$Evidence_category_effect_data ,
                                                   levels = c("NONE","Predation", 
                                                              "Lethal program Reproductive success",
                                                              "Lethal program Abundance",
                                                              "Population Reproductive success 1",
                                                              "Population Reproductive success 0",
                                                              "Population Abundance 1",
                                                              "Population Abundance 0"))
study_freq$Quality_Study_Highlight <- factor(study_freq$Quality_Study_Highlight,
                                             levels = rev(c("No", "Highlight")))

#summary stats for support and non in support studies (panel C of main figure)
study_freq <- study_freq[!is.na(Evidence_category_effect_data), ]
study_freq[, n_abs := abs(number_studies)]
study_freq[, proportion := (n_abs / sum(n_abs)) * 100]
study_freq$proportion<-round(study_freq$proportion, digits = 2)

# 6. Save builds  --------------------------------------------------
#Full dataset including data of the review, eradication programs and threatened birds distribution
Rodents_overlap_data[, seasonal := NULL]

setnames(Rodents_overlap_data, "scientificName", "Bird_scientific_name")
setnames(Rodents_overlap_data, "synth_col", "Evidence")
setnames(Rodents_overlap_data, "Eradication.ID", "Eradication_ID")
setnames(Rodents_overlap_data, "Migratory status", "Migratory_status")
setnames(Rodents_overlap_data, "single_island_endemic", "Single_island_endemic")
setnames(Rodents_overlap_data, "Status (Eradication)", "Eradication_status")
setnames(Rodents_overlap_data, "Island.Name", "Island_Name")
setnames(Rodents_overlap_data, "geom", "Longitude_latitude")

str(Rodents_overlap_data)
coords <- st_coordinates(Rodents_overlap_data$Longitude_latitude)
Rodents_overlap_data$Longitude <- coords[, 1]
Rodents_overlap_data$Latitude <- coords[, 2]
Rodents_overlap_data[, Longitude_latitude := NULL]

Rodents_overlap_data <- Rodents_overlap_data[,.(
  Bird_scientific_name, redlistCategory, Migratory_status, Single_island_endemic, Evidence,
  Eradication_ID, Eradication_status, Island_Name, Archipelago, Region, Country,
  Longitude, Latitude, Rodent_attributed_final
)]

fwrite(Rodents_overlap_data, "builds/eradication_programs/Eradications.csv")

# -- Best evidence per island for all rodents --
#Data for each rodent species map
saveRDS(m_musculus_island, "builds/eradication_programs/m_musculus_island.rds")
saveRDS(r_exulans_island, "builds/eradication_programs/r_exulans_island.rds")
saveRDS(r_norvegicus_island, "builds/eradication_programs/r_norvegicus_island.rds")
saveRDS(r_rattus_island, "builds/eradication_programs/r_rattus_island.rds")

#Frequency of support and not in support
fwrite(study_freq, "builds/eradication_programs/study_freq.csv")
