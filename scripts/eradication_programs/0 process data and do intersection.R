#Aim: To do the spatial intersection between extinct and threatened birds and island rodent eradication programs considering it's evidence 
#
rm(list = ls())
gc()
#
# 1. Loading packages -----------------------------------------------------
library(data.table)
library(sf)
library(rnaturalearth)

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
freq_studies <- frequency_df[synth_col!="No studies"] #These are the bird species with evidence (studies)

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

# 3. Which threatened birds are single island-endemics? --------------------------------------------------
review_species #Out of these 340 spp, how many only exist in one island?


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
library(ggplot2)
world_sf <- st_as_sf(ne_countries(scale = "medium", returnclass = "sf"))
world_sf <- st_as_sf(world_sf, crs = 4326)

m_musculus_DIISE_bird_overlap_sf <- st_as_sf(m_musculus_DIISE_bird_overlap)

ggplot() + 
  geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +
  geom_sf(data = m_musculus_DIISE_bird_overlap_sf, #m_musculus_DIISE_bird_overlap_sf[m_musculus_DIISE_bird_overlap_sf$sci_name == "Diomedea dabbenena", ] 
          aes(color = factor(seasonal))) +
  scale_color_brewer(palette = "Set1")

table(m_musculus_DIISE_bird_overlap$seasonal) #no migratory ranges are present, only resident, breeding and non-breeding areas.

#How many bird species distribution overlap with the house mouse eradications?
unique(m_musculus_DIISE_bird_overlap$sci_name) #21 species out of 33 species!

#How many islands?
unique(m_musculus_DIISE_bird_overlap$Island.Name) #44 islands!

#How many eradications?
length(unique(m_musculus_DIISE_bird_overlap$Eradication.ID)) #47 eradications!

#Clean the df
#setDT(m_musculus_DIISE_bird_overlap)
m_musculus_DIISE_bird_overlap <- m_musculus_DIISE_bird_overlap[, .(
  sci_name, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, Archipelago, Region, Country, geom
)]

setnames(m_musculus_DIISE_bird_overlap, "sci_name", "scientificName")

#Add the evidence column for that species
m_musculus_DIISE_bird_overlap <- merge(m_musculus_DIISE_bird_overlap,
                                     unique(merge_mus_musculus[,.(scientificName, synth_col)]),
                                     by= "scientificName")

m_musculus_DIISE_bird_overlap <- m_musculus_DIISE_bird_overlap[,.(
  scientificName, synth_col, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, Archipelago, Region, Country, geom
)]

#This df includes data of the review, rodent eradication programs and birds distribution
m_musculus_DIISE_bird_overlap[, Rodent_attributed_final := "Mus musculus"]

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

m_musculus_DIISE_bird_overlap[, synth_col := factor(synth_col, levels = evidence_levels)]
m_musculus_DIISE_bird_overlap[, `Status (Eradication)` := factor(`Status (Eradication)`, levels = eradication_levels)]
m_musculus_DIISE_bird_overlap <- m_musculus_DIISE_bird_overlap[order(Island.Name, -as.integer(`Status (Eradication)`, synth_col))]

best_evidence_per_island <- m_musculus_DIISE_bird_overlap[, .SD[1], by = Island.Name]  

#join the three of them
m_musculus_island <- merge(summary_DIISE_bird_m_musculus, threat_count, by = "Island.Name", all.x = TRUE)
m_musculus_island <- merge(m_musculus_island, best_evidence_per_island, by = "Island.Name", all.x = TRUE)
m_musculus_island<- m_musculus_island[,.(
  Eradication.ID, `Status (Eradication)`, seasonal, Island.Name, n_erad_succ, n_erad_not_succ, total_erad, n_threat_birds, scientificName, synth_col, Rodent_attributed_final, Archipelago , Region, Country, geom
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

#How many bird species distribution overlap with the pacific rat eradications?
unique(r_exulans_DIISE_bird_overlap$sci_name) #33 species out of 116 species!

#How many islands?
unique(r_exulans_DIISE_bird_overlap$Island.Name) #151 islands!

#How many eradications?
length(unique(r_exulans_DIISE_bird_overlap$Eradication.ID)) #164 eradications!

#Clean the df
setDT(r_exulans_DIISE_bird_overlap)
r_exulans_DIISE_bird_overlap<- r_exulans_DIISE_bird_overlap[, .(
  sci_name, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name,Archipelago, Region, Country, geom
)]

setnames(r_exulans_DIISE_bird_overlap, "sci_name", "scientificName")

#Add the evidence column for that species
r_exulans_DIISE_bird_overlap<-merge(r_exulans_DIISE_bird_overlap,
                                    unique(merge_r_exulans[,.(scientificName, synth_col)]),
                                    by= "scientificName")
r_exulans_DIISE_bird_overlap<- r_exulans_DIISE_bird_overlap[,.(
  scientificName, synth_col, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name,Archipelago, Region, Country, geom
)]

unique(r_exulans_DIISE_bird_overlap$scientificName) #31 species out of 116 species! This is the correct value

#This df includes data of the review, rodent eradication programs and birds distribution
r_exulans_DIISE_bird_overlap[, Rodent_attributed_final := "Rattus exulans"]

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

r_exulans_DIISE_bird_overlap[, synth_col := factor(synth_col, levels = evidence_levels)]
r_exulans_DIISE_bird_overlap[, `Status (Eradication)` := factor(`Status (Eradication)`, levels = eradication_levels)]

r_exulans_DIISE_bird_overlap <- r_exulans_DIISE_bird_overlap[order(Island.Name, -as.integer(`Status (Eradication)`, synth_col))]
best_evidence_per_island <- r_exulans_DIISE_bird_overlap[, .SD[1], by = Island.Name]  

#Join the three of them
r_exulans_island <- merge(summary_DIISE_bird_r_exulans, threat_count, by = "Island.Name", all.x = TRUE)
r_exulans_island <- merge(r_exulans_island, best_evidence_per_island, by = "Island.Name", all.x = TRUE)
r_exulans_island<- r_exulans_island[,.(
  Eradication.ID,`Status (Eradication)`, seasonal, Island.Name, n_erad_succ, n_erad_not_succ, total_erad, n_threat_birds, scientificName, synth_col, Rodent_attributed_final, Archipelago , Region, Country, geom
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

#How many bird species distribution overlap with the brown rat eradications?
unique(r_norvegicus_DIISE_bird_overlap$sci_name) # 38 out of 178 species!

#How many islands?
unique(r_norvegicus_DIISE_bird_overlap$Island.Name) #237 islands!

#How many eradications?
length(unique(r_norvegicus_DIISE_bird_overlap$Eradication.ID)) #283 eradications!

#Clean the df
setDT(r_norvegicus_DIISE_bird_overlap)
r_norvegicus_DIISE_bird_overlap<- r_norvegicus_DIISE_bird_overlap[, .(
  sci_name, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name,Archipelago, Region, Country, geom
)]

setnames(r_norvegicus_DIISE_bird_overlap, "sci_name", "scientificName")

#Add the evidence column for that species
r_norvegicus_DIISE_bird_overlap<-merge(r_norvegicus_DIISE_bird_overlap,
                                       unique(merge_r_norvegicus[,.(scientificName, synth_col)]),
                                       by= "scientificName")
#and reorder columns
r_norvegicus_DIISE_bird_overlap<- r_norvegicus_DIISE_bird_overlap[,.(
  scientificName, synth_col, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name,Archipelago, Region, Country, geom
)]

#This df includes data of the review, rodent eradication programs and birds distribution
r_norvegicus_DIISE_bird_overlap[, Rodent_attributed_final := "Rattus norvegicus"]

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

r_norvegicus_DIISE_bird_overlap[, synth_col := factor(synth_col, levels = evidence_levels)]
r_norvegicus_DIISE_bird_overlap[, `Status (Eradication)` := factor(`Status (Eradication)`, levels = eradication_levels)]

r_norvegicus_DIISE_bird_overlap <- r_norvegicus_DIISE_bird_overlap[order(Island.Name, -as.integer(`Status (Eradication)`, synth_col))]

best_evidence_per_island <- r_norvegicus_DIISE_bird_overlap[, .SD[1], by = Island.Name]  

#Join the three of them
r_norvegicus_island <- merge(summary_DIISE_bird_r_norvegicus, threat_count, by = "Island.Name", all.x = TRUE)
r_norvegicus_island <- merge(r_norvegicus_island, best_evidence_per_island, by = "Island.Name", all.x = TRUE)
r_norvegicus_island<- r_norvegicus_island[,.(
  Eradication.ID, seasonal, Island.Name, n_erad_succ, n_erad_not_succ, total_erad, n_threat_birds, scientificName, synth_col, Rodent_attributed_final, Archipelago , Region, Country, geom
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

#How many bird species distribution overlap with the black rat eradications?
unique(r_rattus_DIISE_bird_overlap$sci_name) #85 out of 283 species!

#How many islands?
unique(r_rattus_DIISE_bird_overlap$Island.Name) #381 islands!

#How many eradications?
unique(r_rattus_DIISE_bird_overlap$Eradication.ID) #454 eradications!

#Clean the df
setDT(r_rattus_DIISE_bird_overlap)
r_rattus_DIISE_bird_overlap<- r_rattus_DIISE_bird_overlap[, .(
  sci_name, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name,Archipelago, Region, Country, geom
)]

setnames(r_rattus_DIISE_bird_overlap, "sci_name", "scientificName")

#Add the evidence column for that species
r_rattus_DIISE_bird_overlap<-merge(r_rattus_DIISE_bird_overlap,
                                   unique(merge_r_rattus[,.(scientificName, synth_col)]),
                                   by= "scientificName")
#and reorder columns
r_rattus_DIISE_bird_overlap<- r_rattus_DIISE_bird_overlap[,.(
  scientificName, synth_col, Eradication.ID, `Status (Eradication)`, seasonal, Island.Name,Archipelago, Region, Country, geom
)]

#This df includes data of the review, rodent eradication programs and birds distribution
r_rattus_DIISE_bird_overlap[, Rodent_attributed_final := "Rattus rattus"]

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

r_rattus_DIISE_bird_overlap[, synth_col := factor(synth_col, levels = evidence_levels)]
r_rattus_DIISE_bird_overlap[, `Status (Eradication)` := factor(`Status (Eradication)`, levels = eradication_levels)]

r_rattus_DIISE_bird_overlap <- r_rattus_DIISE_bird_overlap[order(Island.Name, -as.integer(`Status (Eradication)`, synth_col))]

best_evidence_per_island <- r_rattus_DIISE_bird_overlap[, .SD[1], by = Island.Name]  

#Join the three of them
r_rattus_island <- merge(summary_DIISE_bird_r_rattus, threat_count, by = "Island.Name", all.x = TRUE)
r_rattus_island <- merge(r_rattus_island, best_evidence_per_island, by = "Island.Name", all.x = TRUE)
r_rattus_island<- r_rattus_island[,.(
  Eradication.ID, seasonal, Island.Name, n_erad_succ, n_erad_not_succ, total_erad, n_threat_birds, scientificName, synth_col, Rodent_attributed_final, Archipelago , Region, Country, geom
)]

# 5. Summary  --------------------------------------------------
all_eradications <- rbindlist(list(
  m_musculus_island,
  r_exulans_island,
  r_norvegicus_island,
  r_rattus_island
), use.names = TRUE, fill = TRUE)

setDT(all_eradications) #converts back to data.table for summary stats only

Rodents_overlap_data<- rbindlist(list(
  r_rattus_DIISE_bird_overlap,
  r_norvegicus_DIISE_bird_overlap,
  r_exulans_DIISE_bird_overlap,
  m_musculus_DIISE_bird_overlap
))

#For overall eradications
unique(data_DIISE$`Island Name`) #794 islands
unique(data_DIISE$`Eradication ID`) #1104 eradications
table(data_DIISE$`Status (Eradication)`) #Successful: 645 

#Rodent eradication programs were conducted within the range of X attributed bird species
unique(Rodents_overlap_data$scientificName) #111 species
unique(Rodents_overlap_data$Island.Name) #719 islands
unique(Rodents_overlap_data$Eradication.ID) #948 eradications
table(Rodents_overlap_data$`Status (Eradication)`) #continue from here!!

#Count and proportion of each evidence type
evidence_summary <- all_eradications[, .(
  count = .N
), by = synth_col][
  , proportion := (count / sum(count))*100
]

sum(evidence_summary$proportion) #it's okay.

#Count and proportion of each evidence type per rodent species
evidence_summary_rodent <- all_eradications[, .N,
                                            by = .(Rodent_attributed_final, synth_col)][
  , proportion := (N / sum(N)) * 100, by = Rodent_attributed_final]

#Per unique bird species and evidence
pop_data_sup<-Rodents_overlap_data[synth_col=="Population study with data in support"]
unique(pop_data_sup$scientificName)

pop_data_sup_all<-Rodents_overlap_data[synth_col=="Population study with all qualities in support"]
unique(pop_data_sup_all$scientificName)

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
