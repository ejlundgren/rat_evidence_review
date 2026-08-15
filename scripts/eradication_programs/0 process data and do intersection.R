#Aim: To do the spatial intersection between extinct and threatened birds and island rodent eradication programs considering it's evidence 
#
rm(list = ls())
gc()
#
# 1. Loading packages -----------------------------------------------------
library(data.table)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(readxl)
library(writexl)

# 2. Load data and builds --------------------------------------------------
# NOTE FOR REVIEWERS:
#All intermediate spatial outputs from the intersection between the bird species ranges and the rodent eradication programs, are provided in the builds folder.
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

#List of EX and threatened migratory birds
 #BirdLife International (2026) Data downloaded from https://datazone.birdlife.org on 13/08/2026.
migratory_list <- fread("data/Working_Databases/species-filter-results.csv")

world_sf <- st_as_sf(ne_countries(scale = "medium", returnclass = "sf"))
world_sf <- st_as_sf(world_sf, crs = 4326)

# 3. Which threatened birds are single island-endemics? --------------------------------------------------
single_island <- read_xlsx("data/Working_Databases/single_island_endemic_classification.xlsx")
setDT(single_island)
table(single_island$single_island_endemic)

#Join single_island df to freq df and add to this one the Red List status for each species
spp_status <- unique(data_review[,.(scientificName, redlistCategory)])

spp_status <- merge(spp_status, migratory_list[,.(`Scientific name`, `Migratory status`)],
                    by.x = "scientificName", by.y ="Scientific name", all.x = TRUE)
spp_status[is.na(`Migratory status`), `Migratory status` := "Not a Migrant"]

spp_status <- merge(spp_status, single_island[,.(scientificName, distribution, single_island_endemic)],
                    by = "scientificName")

frequency_df <- merge(frequency_df, spp_status, by = "scientificName")
frequency_df #Nice :-)

table(frequency_df$redlistCategory) #There's a spp without Red List status 
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
summary_m_musculus #47.22% bird species have no studies and 52.77% have studies, of these, No study in support: 2.77%, Only predation in support:41.66%, Population study without data in support:8.33%

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

#Plot bird ranges according to the different seasonal categories 
# m_musculus_DIISE_bird_overlap_sf <- st_as_sf(m_musculus_DIISE_bird_overlap)
# 
# ggplot() +
#   geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +
#   geom_sf(data = m_musculus_DIISE_bird_overlap_sf[m_musculus_DIISE_bird_overlap_sf$scientificName == "Rowettia goughensis", ], #m_musculus_DIISE_bird_overlap_sf[m_musculus_DIISE_bird_overlap_sf$sci_name == "Diomedea dabbenena", ]
#           aes(color = factor(seasonal))) +
#   scale_color_brewer(palette = "Set1")
# 
# table(m_musculus_DIISE_bird_overlap$seasonal) #no migratory ranges are present, only resident, breeding and non-breeding areas.

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

#How many bird species distribution overlap with the house mouse eradications?
unique(m_musculus_DIISE_bird_overlap$scientificName) #20 species out of 33 species!

#How many islands?
unique(m_musculus_DIISE_bird_overlap$Island.Name) #44 islands!

#How many eradications?
length(unique(m_musculus_DIISE_bird_overlap$Eradication.ID)) #47 eradications!

#Now check if there are fully migrant species with island distribution
#' m_musculus_DIISE_bird_overlap[`Migratory status` == "Full migrant" & distribution == "island", ] #Pachyptila macgillivrayi
#' 
#' #Filter out from the overlap the resident range of those species that are fullly migrant since the overlap is potentially a false positive
#' x <- m_musculus_DIISE_bird_overlap[`Migratory status` != "Full migrant" | seasonal != 1, ]
#' x <- rbind(x, m_musculus_DIISE_bird_overlap[`Migratory status` == "Full migrant" & distribution == "island", ])
#' unique(x$scientificName) #Instead of 20 species, 17 truly overlapped
#' ex_m_musculus <- setdiff(unique(m_musculus_DIISE_bird_overlap$scientificName), unique(x$scientificName)) #Excluded spp: "Hydrobates leucorhous"   "Pterodroma arminjoniana" "Pterodroma externa"
#' 
#' #'*continue from here, but try this same thing with the other spp*
#' 
#' 
#' table(m_musculus_DIISE_bird_overlap$scientificName)
#' table(x$scientificName)
#' table(m_musculus_DIISE_bird_overlap$redlistCategory) #No extinct species
#' table(m_musculus_DIISE_bird_overlap$`Migratory status`)
#' table(m_musculus_DIISE_bird_overlap$synth_col)
#' table(m_musculus_DIISE_bird_overlap$single_island_endemic) #No single-island endemic species
#' table(m_musculus_DIISE_bird_overlap$distribution) #85 out of 89 overlapped points are with spp that have a large distribution..
#' table(m_musculus_DIISE_bird_overlap$seasonal)
#' table(m_musculus_DIISE_bird_overlap[distribution == "mixed", seasonal])
#' table(m_musculus_DIISE_bird_overlap[single_island_endemic == "yes", synth_col])
#' unique(m_musculus_DIISE_bird_overlap[seasonal != 1 & distribution == "mixed", scientificName]) #12 species
#' unique(m_musculus_DIISE_bird_overlap[distribution == "mixed", scientificName]) #17 species
#' setdiff(unique(m_musculus_DIISE_bird_overlap[distribution == "mixed", scientificName]),
#'         unique(m_musculus_DIISE_bird_overlap[seasonal != 1 & distribution == "mixed", scientificName])) #5 spp are not included if we don't use the resident range

#maybe it makes sense to only use the breeding range for those spp with large distribution...
#the breeding range is very localized, opposite to the resident range

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

m_musculus_DIISE_bird_overlap <- m_musculus_DIISE_bird_overlap[order(Island.Name, -as.integer(synth_col), -as.integer(`Status (Eradication)`))]

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
summary_r_exulans #75.42% bird species have no studies and 24.57% have studies, of these, Lethal program in support: 2.54%, Only predation in support:8.47%, Population study without data in support:2.54%
#Population study with all qualities in support: 0.84%, No study in support: 8.47%, Population study with data in support:1.69%

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
      (synth_col == "Population study with all qualities in support" & Hypothesis_supported == 0) |
      (synth_col == "Population study without data in support" & Hypothesis_supported == 0)
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
r_exulans_DIISE_bird_overlap[, Rodent_attributed_final := "Rattus exulans"] #maybe join Pseudobulweria macgillivrayi

#How many bird species distribution overlap with the pacific rat eradications?
unique(r_exulans_DIISE_bird_overlap$scientificName) #31 species out of 116 species!

#How many islands?
unique(r_exulans_DIISE_bird_overlap$Island.Name) #151 islands!

#How many eradications?
length(unique(r_exulans_DIISE_bird_overlap$Eradication.ID)) #164 eradications!

#Now check if there are fully migrant species with island distribution
# r_exulans_DIISE_bird_overlap[`Migratory status` == "Full migrant" & distribution == "island", ] #many spp
# 
# #Filter out from the overlap the resident range of those species that are fullly migrant since the overlap is potentially a false positive
# x2 <- r_exulans_DIISE_bird_overlap[`Migratory status` != "Full migrant" | seasonal != 1, ]
# #x <- rbind(x, m_musculus_DIISE_bird_overlap[`Migratory status` == "Full migrant" & distribution == "island", ])
# unique(x2$scientificName) #Instead of 31 species, 26 truly overlapped
# ex_r_exulans <- setdiff(unique(r_exulans_DIISE_bird_overlap$scientificName), unique(x2$scientificName)) #5 spp excl: "Procellaria parkinsoni"       "Procellaria westlandica"      "Pseudobulweria macgillivrayi" "Pterodroma sandwichensis"     "Puffinus newelli"  


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

r_exulans_DIISE_bird_overlap <- r_exulans_DIISE_bird_overlap[order(Island.Name, -as.integer(synth_col), -as.integer(`Status (Eradication)`))]
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
summary_r_norvegicus #74.15% bird species have no studies and 25.84% have studies, of these,
#Lethal program in support: 1.68%, Only predation in support:12.92%, Population study without data in support:2.80%
#Population study with all qualities in support: 0.56%, No study in support: 7.3%, Population study with data in support:0.56%

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
      (synth_col == "Lethal program in support" & Hypothesis_supported == 0) |
      (synth_col == "Population study without data in support" & Hypothesis_supported == 0) 
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

#How many bird species distribution overlap with the brown rat eradications?
unique(r_norvegicus_DIISE_bird_overlap$scientificName) # 38 out of 178 species!

#How many islands?
unique(r_norvegicus_DIISE_bird_overlap$Island.Name) #237 islands!

#How many eradications?
length(unique(r_norvegicus_DIISE_bird_overlap$Eradication.ID)) #283 eradications!

#Now check if there are fully migrant species with island distribution
# m_musculus_DIISE_bird_overlap[`Migratory status` == "Full migrant" & distribution == "island", ] #Pachyptila macgillivrayi 
# 
# #Filter out from the overlap the resident range of those species that are fullly migrant since the overlap is potentially a false positive
# x3 <- r_norvegicus_DIISE_bird_overlap[`Migratory status` != "Full migrant" | seasonal != 1, ]
# #x <- rbind(x, m_musculus_DIISE_bird_overlap[`Migratory status` == "Full migrant" & distribution == "island", ])
# unique(x3$scientificName) #Instead of 20 species, 17 truly overlapped
# ex_r_norvegicus <- setdiff(unique(r_norvegicus_DIISE_bird_overlap$scientificName), unique(x3$scientificName)) #Excluded spp: "Hydrobates leucorhous"   "Pterodroma arminjoniana" "Pterodroma externa"


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

r_norvegicus_DIISE_bird_overlap <- r_norvegicus_DIISE_bird_overlap[order(Island.Name, -as.integer(synth_col), -as.integer(`Status (Eradication)`))]

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
summary_r_rattus #63.25% bird species have no studies and 36.74% have studies, of these,
#Lethal program in support: 2.82%, Only predation in support:12.01%, Population study without data in support:6.36%
#Population study with all qualities in support: 1.41%, No study in support: 10.95%, Population study with data in support:3.18%

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
      (synth_col == "Lethal program in support" & Hypothesis_supported == 0) |
      (synth_col == "Population study without data in support" & Hypothesis_supported == 0) 
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

#How many bird species distribution overlap with the black rat eradications?
unique(r_rattus_DIISE_bird_overlap$scientificName) #84 out of 283 species!

#How many islands?
unique(r_rattus_DIISE_bird_overlap$Island.Name) #381 islands!

#How many eradications?
unique(r_rattus_DIISE_bird_overlap$Eradication.ID) #454 eradications!

#Now check if there are fully migrant species with island distribution
# r_rattus_DIISE_bird_overlap[`Migratory status` == "Full migrant" & distribution == "island", ] #Pachyptila macgillivrayi 
# 
# #Filter out from the overlap the resident range of those species that are fullly migrant since the overlap is potentially a false positive
# x4 <- r_rattus_DIISE_bird_overlap[`Migratory status` != "Full migrant" | seasonal != 1, ]
# #x <- rbind(x, m_musculus_DIISE_bird_overlap[`Migratory status` == "Full migrant" & distribution == "island", ])
# unique(x4$scientificName) #Instead of 84 species, 69 truly overlapped
# ex_r_rattus <- setdiff(unique(r_rattus_DIISE_bird_overlap$scientificName), unique(x4$scientificName)) #Excluded spp: "Hydrobates leucorhous"   "Pterodroma arminjoniana" "Pterodroma externa"
# 
# excluded <- c(ex_m_musculus, ex_r_exulans, ex_r_norvegicus, ex_r_rattus)
# excluded <- unique(excluded)
# excluded
# 
# x[scientificName %in% excluded, scientificName]
# x2[scientificName %in% excluded, scientificName]
# x3[scientificName %in% excluded, scientificName]
# x4[scientificName %in% excluded, scientificName]
# 
# not_truly_excluded <- c("Diomedea dabbenena", "Phoebetria fusca", "Phoebetria fusca",
#                         "Nesofregetta fuliginosa",
#                         "Hydrobates leucorhous",
#                         "Puffinus mauretanicus")
# 
# really_excluded <- setdiff(excluded, not_truly_excluded)
# 
# #Load the maps of these species to confirm the range -> CONFIRMED!! THIS SPP HAVE THE LARGE RANGE AS SEASONAL = 1. 
# excluded_species_query <- paste0("SELECT * FROM all_species WHERE sci_name IN ('", 
#                                    paste(really_excluded, collapse = "','"), "')")
# 
# bird_distribution_excluded <- st_read(
#   "~/Sixth mass extinction/Luna_project/Databases/BirdLifemaps/BOTW_2024_2.gpkg",
#   query = threatened_species_query)
# 
# ggplot() +
#   geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +
#   geom_sf(data = bird_distribution_excluded[bird_distribution_excluded$sci_name == "Pseudobulweria macgillivrayi", ], #m_musculus_DIISE_bird_overlap_sf[m_musculus_DIISE_bird_overlap_sf$sci_name == "Diomedea dabbenena", ]
#           aes(color = factor(seasonal))) +
#   scale_color_brewer(palette = "Set1")
# 
# library(mapview)
# mapview(bird_distribution_excluded[bird_distribution_excluded$sci_name == "Pseudobulweria macgillivrayi", zcol= "seasonal"])

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

r_rattus_DIISE_bird_overlap <- r_rattus_DIISE_bird_overlap[order(Island.Name, -as.integer(synth_col), -as.integer(`Status (Eradication)`))]

best_evidence_per_island <- r_rattus_DIISE_bird_overlap[, .SD[1], by = Island.Name]  

#Join the three of them
r_rattus_island <- merge(summary_DIISE_bird_r_rattus, threat_count, by = "Island.Name", all.x = TRUE)
r_rattus_island <- merge(r_rattus_island, best_evidence_per_island, by = "Island.Name", all.x = TRUE)
r_rattus_island<- r_rattus_island[,.(
  Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, n_erad_succ, n_erad_not_succ, total_erad, n_threat_birds,
  scientificName, redlistCategory, `Migratory status`, distribution, single_island_endemic,
  synth_col, Rodent_attributed_final, Archipelago , Region, Country, geom
)]

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

## Sys review -----------
#'[For the systematic review we need the evidence plot and summaries
unique(Rodents_overlap_data$scientificName) #111 species
unique(Rodents_overlap_data$Island.Name) #719 islands
length(unique(Rodents_overlap_data$Eradication.ID)) #948 eradications 
#'*Rodent eradication programs were conducted within the range of 111 attributed bird species*

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
#'*no studies connecting them to rodents for 20.04%;* 
#'*only studies not in support for 11.92%;* 
#'*only evidence that predation occurs for 10.13%;* 
#'*lethal program with data in support for 0.42%;* 
#'*at least one population study in support, but without data, for 39.45%;* 
#'*at least one population study in support with data for 12.13%;* 
#'*and at least one population study with data and qualities for 5.91%.*
setnames(sum_overlap, "synth_col", "Evidence")
setorder(sum_overlap, -Evidence)
sum_overlap

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

sum(sum_rodent$N) #correct! 948 eradications
sum(sum_rodent[Evidence == "All studies are not in support", N]) #Should be 113
sum(sum_rodent[Evidence == "Population study without data in support", N]) #Should be 374

## Sys review by eradication status ----------- 
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

sum(sum_overlap_erad_status[Evidence == "All studies are not in support", N]) #Should be 113
sum(sum_overlap_erad_status[Evidence == "Population study without data in support", N]) #Should be 374

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
sum(sum_rodent_erad_status[Evidence == "All studies are not in support", N]) #Should be 113
sum(sum_rodent_erad_status[Evidence == "Population study without data in support", N]) #Should be 374

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
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))

ggsave("figures/evidence_by_status.pdf", plot = last_plot(), device = cairo_pdf, 
       width = 11.46, height = 8.30)

## Sys review for extant species only -----------
#(excluding extinct species)
table(Rodents_overlap_data$redlistCategory) #281 rows of extinct species

#Count and proportion of each evidence type overall
sum_overlap_extant <- Rodents_overlap_data[redlistCategory != "Extinct", ]
sum_overlap_extant <- unique(sum_overlap_extant[, .(Eradication.ID, `Status (Eradication)`, Rodent_attributed_final, synth_col)])
sum_overlap_extant <- sum_overlap_extant[order(Eradication.ID, as.integer(synth_col))]
sum_overlap_extant <- sum_overlap_extant[, .SD[1], by = .(Eradication.ID)]

sum_overlap_extant <- sum_overlap_extant[, .(.N), by = synth_col][
  , Proportion := round((N / sum(N)) * 100, digits = 2)
]
setnames(sum_overlap_extant, "synth_col", "Evidence")
setorder(sum_overlap_extant, -Evidence)

#'*When extinct species are excluded the total number of eradications is not reduced,*
#'*meaning that the points where ex species overlapped,*
#'*also overlapped with extant species*
sum_overlap_extant 
sum(sum_overlap$N) #correct! 948 eradications
sum(sum_overlap_extant[Evidence == "All studies are not in support", N]) #Changed to 99
sum(sum_overlap_extant[Evidence == "Population study without data in support", N]) #Changed to 372

#Confirm that no eradication is lost entirely
erad_ids_full <- unique(Rodents_overlap_data$Eradication.ID)
erad_ids_extant <- unique(Rodents_overlap_data[redlistCategory != "Extinct", Eradication.ID])
setdiff(erad_ids_full, erad_ids_extant)  #should be 0

#Count and proportion of each evidence type per rodent species
sum_rodent_extant <- Rodents_overlap_data[redlistCategory != "Extinct", ]
sum_rodent_extant <- unique(sum_rodent_extant[, .(Eradication.ID, `Status (Eradication)`, Rodent_attributed_final, synth_col)])
sum_rodent_extant <- sum_rodent_extant[order(Eradication.ID, as.integer(synth_col))]
sum_rodent_extant <- sum_rodent_extant[, .SD[1], by = Eradication.ID]
sum_rodent_extant <- sum_rodent_extant[, .N, by = .(Rodent_attributed_final, synth_col)][
  , Proportion := round((N / sum(N)) * 100, digits = 2), by = Rodent_attributed_final
]

sum_rodent_extant
setnames(sum_rodent_extant, "synth_col", "Evidence")
setnames(sum_rodent_extant, "Rodent_attributed_final", "Rodent attributed")
setorder(sum_rodent_extant, -Evidence)
sum_rodent_extant

sum(sum_rodent_extant$N) #The total is the same
sum(sum_rodent_extant[Evidence == "All studies are not in support", N]) #Changed to 99
sum(sum_rodent_extant[Evidence == "Population study without data in support", N]) #Changed to 372

## Sys review for extant multi-island endemics and regional species -----------
#(excluding extinct and single island endemics)
table(Rodents_overlap_data$single_island_endemic) #64 rows
table(Rodents_overlap_data$redlistCategory) #281 rows

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
#'*the total number of eradications is not reduced, same as before*
#'*the points where ex species and single-endemic species overlapped,*
#'*also overlapped with extant species*
sum_overlap_extant_no_sing_end
sum(sum_overlap_extant_no_sing_end$N) #correct! 948 eradications
sum(sum_overlap_extant_no_sing_end[Evidence == "All studies are not in support", N]) #Changed to 99
sum(sum_overlap_extant_no_sing_end[Evidence == "Population study without data in support", N]) #Changed to 372

#Confirm that no eradication is lost entirely
erad_ids_full <- unique(Rodents_overlap_data$Eradication.ID)
erad_ids_extant <- unique(Rodents_overlap_data[redlistCategory != "Extinct" &
                                                 single_island_endemic != "yes", Eradication.ID])
setdiff(erad_ids_full, erad_ids_extant)  #should be 0

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
sum(sum_rodent_extant_no_sing_end[Evidence == "All studies are not in support", N]) #Changed to 99
sum(sum_rodent_extant_no_sing_end[Evidence == "Population study without data in support", N]) #Changed to 372


sum_review_tables <- list(
  "summary" = sum_overlap,
  "sum_per_rodent" = sum_rodent,
  "sum_erad_status" = sum_overlap_erad_status,
  "sum_per_rodent_erad_status" = sum_rodent_erad_status,
  "sum_extant" = sum_overlap_extant,
  "sum_per_rodent_extant" = sum_rodent_extant,
  "sum_extant_no_sing_end" = sum_overlap_extant_no_sing_end,
  "sum_per_rodent_extant_no_sing_end" = sum_rodent_extant_no_sing_end
  )

writexl::write_xlsx(sum_review_tables, "builds/eradication_programs/sum_review_tables.xlsx")
---------------------------------------
  
#Per unique bird species and evidence
pop_data_sup<-Rodents_overlap_data[synth_col=="Population study with data in support"]
unique(pop_data_sup$scientificName)

pop_data_sup_all<-Rodents_overlap_data[synth_col=="Population study with all qualities in support"]
unique(pop_data_sup_all$scientificName)


sum_era <- unique(Rodents_overlap_data[, .(Eradication.ID, `Status (Eradication)`)])
table(sum_era$`Status (Eradication)`)

#Count and proportion of each eradication type
sum_era <- sum_era[, .(
  count = .N
), by = `Status (Eradication)`][
  , proportion := (count / sum(count))*100
] #60% of the eradications were successful

#BUT CHECK THIS % ALSO INCLUDING THE EVIDENCE TYPE!!!!!
sum_era_evi <- Rodents_overlap_data[, .N,
                                    by = .(Rodent_attributed_final, synth_col, `Status (Eradication)`)][
                                      , proportion := (N / sum(N)) * 100, by = .(Rodent_attributed_final, `Status (Eradication)`)]

# -- Best evidence per island for all rodents --
all_eradications <- rbindlist(list(
  m_musculus_island,
  r_exulans_island,
  r_norvegicus_island,
  r_rattus_island
), use.names = TRUE, fill = TRUE)

setDT(all_eradications) #converts back to data.table for summary stats only




#Now check if there are fully migrant species with island distribution
m_musculus_DIISE_bird_overlap[`Migratory status` == "Full migrant" & distribution == "island", ] #Pachyptila macgillivrayi 

#Filter out from the overlap the resident range of those species that are fullly migrant since the overlap is potentially a false positive
x <- m_musculus_DIISE_bird_overlap[`Migratory status` != "Full migrant" | seasonal != 1, ]
x <- rbind(x, m_musculus_DIISE_bird_overlap[`Migratory status` == "Full migrant" & distribution == "island", ])
unique(x$scientificName) #Instead of 20 species, 17 truly overlapped
ex_m_musculus <- setdiff(unique(m_musculus_DIISE_bird_overlap$scientificName), unique(x$scientificName)) #Excluded spp: "Hydrobates leucorhous"   "Pterodroma arminjoniana" "Pterodroma externa"




ggplot(Rodents_overlap_data,
       aes(reorder(Rodent_attributed_final, Rodent_attributed_final, function(x) -length(x)),
           fill=synth_col, color=synth_col)) +
  geom_bar(stat = "count", linewidth=.75) +
  scale_fill_manual(values = plot_col)+
  scale_color_manual(values = col_pal) +
  ylab("Number of eradications")+
  xlab("")+
  scale_x_discrete(labels = c(c("Rattus rattus" = "Black rats", 
                                "Rattus norvegicus" = "Brown rats",
                                "Rattus exulans" = "Pacific rats",
                                "Mus musculus" = "House mouse")))+
  
  theme_lundy+
  theme(legend.position = "none")



#Frequency of support
#number of studies in each evidence category and in support and not in support of the hypothesis (for those bird species that overlap with an eradication)
data_review[, value := ifelse(Hypothesis_supported == 0, -1, 1)]
data_review 

#Filter rodent sp different to the ones in our study
data_review  <- data_review [Rodent_attributed_final %in% rodent_sp]
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
bird_sp_rodent_overlap<-unique(Rodents_overlap_data$scientificName) #114 bird species

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
fwrite(Rodents_overlap_data, "builds/eradication_programs/Rodents_overlap_data.csv")

#Data for each rodent species map
saveRDS(m_musculus_island, "builds/eradication_programs/m_musculus_island.rds")
saveRDS(r_exulans_island, "builds/eradication_programs/r_exulans_island.rds")
saveRDS(r_norvegicus_island, "builds/eradication_programs/r_norvegicus_island.rds")
saveRDS(r_rattus_island, "builds/eradication_programs/r_rattus_island.rds")

#Frequency of support and not in support
fwrite(study_freq, "builds/eradication_programs/study_freq.csv")
