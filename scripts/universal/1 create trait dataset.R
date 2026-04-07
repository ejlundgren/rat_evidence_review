
# Prepare environment -----------------------------------------------------

rm(list = ls())

library("groundhog")

date <- "2024-07-15"
pcks <- c("data.table", "ggplot2", "tidyr", "readxl",
          "stringr", "dplyr")
groundhog.library(pcks, date)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Load systematic review --------------------------------------------------

dat <- fread("data/Working_Databases/Attribution.csv")
dat

# >>> Merge in bird traits ------------------------------------------------
# I do not fully understand this database...Different species are in the 
# 3 tabs (birdlife, ebird, and BirdTree)
# Load all of them?
avonet <- rbind(read_excel("../../Resources/Databases/AVONET/AVONET Supplementary dataset 1.xlsx",
                           sheet = "AVONET1_BirdLife") |>
                  rename(Species = Species1,
                         Family = Family1,
                         Order = Order1) |>
                  dplyr::select(Species, Family, Order, Habitat, Primary.Lifestyle,
                                Mass) |>
                  mutate(Source = "BirdLife") |>
                  setDT(),
                read_excel("../../Resources/Databases/AVONET/AVONET Supplementary dataset 1.xlsx",
                           sheet = "AVONET2_eBird") |>
                  rename(Species = Species2,
                         Family = Family2,
                         Order = Order2) |>
                  dplyr::select(Species, Family, Order, Habitat, Primary.Lifestyle,
                                Mass) |>
                  mutate(Source = "eBird") |>
                  setDT(),
                read_excel("../../Resources/Databases/AVONET/AVONET Supplementary dataset 1.xlsx",
                           sheet = "AVONET3_BirdTree") |>
                  rename(Species = Species3,
                         Family = Family3,
                         Order = Order3) |>
                  dplyr::select(Species, Family, Order, Habitat, Primary.Lifestyle,
                                Mass) |>
                  mutate(Source = "BirdTree") |>
                  setDT(),
                fill = T)


avonet
setDT(avonet)
unique(avonet$Habitat)
unique(avonet$Primary.Lifestyle)

setdiff(dat$scientificName, avonet$Species)
dat[!scientificName %in% avonet$Species &
      Synonyms_or_previous_lump %in% avonet$Species]

dat[scientificName %in% avonet$Species, 
    avonet_species := scientificName]

dat[!scientificName %in% avonet$Species &
      Synonyms_or_previous_lump %in% avonet$Species,
    avonet_species := Synonyms_or_previous_lump]


sort(unique(dat[is.na(avonet_species), ]$scientificName))
sort(dat[is.na(avonet_species) & redlistCategory != "Extinct", ]$scientificName)

to_add <- list()

# ---------Gallirallus lafresnayanus----------- -----------------------------------------------!
avonet[Species == "Cabalus lafresnayanus"]
sort(avonet[grepl("Rallus", Species)]$Species)
avonet[grepl("Rallus", Species)]
dat[scientificName == "Gallirallus lafresnayanus", avonet_species := "Gallirallus lafresnayanus"]
# Does not seem to exist in database...
addendum <- avonet[1, ]
addendum[, `:=` (Species = "Gallirallus lafresnayanus",
                 Family = "Rallidae",
                 Order = "Gruiformes",
                 Habitat = "Forest",
                 Primary.Lifestyle = "Terrestrial",
                 Mass = mean(avonet[grepl("Rallus", Species) |
                                      grepl("Hypotaenidia", Species), ]$Mass),
                 Source = "BirdsoftheWorld Web & genus mean mass")]
to_add[[1]] <- copy(addendum)

# ---------Pampusana canifrons----------- -----------------------------------------------!
avonet[grepl("Alopecoenas", Species)]

dat[scientificName == "Pampusana canifrons", avonet_species := "Alopecoenas canifrons"]

# ---------Pampusana erythroptera----------- -----------------------------------------------!
dat[scientificName == "Pampusana erythroptera", avonet_species := "Alopecoenas erythropterus"]

# ---------Pampusana kubaryi----------- -----------------------------------------------!
dat[scientificName == "Pampusana kubaryi", avonet_species := "Alopecoenas kubaryi"]

# ---------Pampusana sanctaecrucis----------- -----------------------------------------------!
dat[scientificName == "Pampusana sanctaecrucis", avonet_species := "Alopecoenas sanctaecrucis"]

# ---------Pampusana stairi----------- -----------------------------------------------!
dat[scientificName == "Pampusana stairi", avonet_species := "Alopecoenas stairi"]


# >>> Bind addendum in ----------------------------------------------------
avonet <- rbind(avonet,
                addendum)

avonet

avonet[, Genus := word(Species, 1, sep = " ")]

# >>> Create final trait dataset for extant species -----------------------
extant.dat <- dat[redlistCategory != "Extinct"]

extant.avonet <- avonet[Species %in% extant.dat$avonet_species]
extant.avonet
#
extant.avonet <- extant.avonet[, .(Family = paste(sort(unique(Family)), collapse = "; "),
                                   Order = paste(sort(unique(Order)), collapse = "; "),
                                   Habitat = paste(sort(unique(Habitat)), collapse = "; "),
                                   Primary.Lifestyle = paste(sort(unique(Primary.Lifestyle)), collapse = "; "),
                                   Mass = mean(Mass)),
                               by = .(Species, Genus)]
extant.avonet[grepl(";", Family)]

# Let's ignore these contradictions for now.
extant.avonet[grepl(";", Order)]
# Let's fix these though...
extant.avonet[Species == "Aegotheles savesi", Order := "Caprimulgiformes"]
extant.avonet[Species == "Sephanoides fernandensis", Order := "Apodiformes"]
extant.avonet[Species == "Apteryx mantelli", Order := "Apterygiformes"]

extant.avonet[grepl(";", Order)]

# extant.avonet[, merge_key := Species]
# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# Populate extinct species --------------------------------------------
dat[, Genus := word(scientificName, 1, sep = " ")]

unique(dat$redlistCategory)
extinct.dat <- dat[redlistCategory == "Extinct"]

extinct.dat[, avonet_species := scientificName]

# extinct.dat[, avonet_species := Genus]
extinct.dat[Genus %in% avonet$Genus]
extinct.dat[!Genus %in% avonet$Genus, .(scientificName, Genus)]
#
extinct.dat[scientificName == "Pampusana salamonis", `:=` (Genus =  "Alopecoenas",
                                                           avonet_species = "Alopecoenas salamonis")]

# Add addendums for fully extinct genera
# avonet[grepl("braccatus", Species)]
# extinct.dat[scientificName == "Moho braccatus", avonet_species := "Moho braccatus"]
addendum <- avonet[1, ]
addendum[, `:=` (Species = "Moho braccatus", Family = "Mohoidae", Order = "Passeriformes",
                 Habitat = "Forest", Genus = "Moho",
                 Primary.Lifestyle = "", Mass = NA, Source = "Wikipedia")]
add_list <- list()
add_list[[1]] <- copy(addendum)

extinct.dat[scientificName == "Moho bishopi", avonet_species := "Moho bishopi"]
addendum[, `:=` (Species = "Moho bishopi", Family = "Mohoidae", Order = "Passeriformes",
                 Habitat = "Forest",Genus = "Moho",
                 Primary.Lifestyle = "", Mass = NA, Source = "Wikipedia")]
add_list[[2]] <- copy(addendum)

extinct.dat[scientificName == "Turnagra capensis", avonet_species := "Turnagra capensis"]
addendum[, `:=` (Species = "Turnagra capensis", Family = "Oriolidae", Order = "Passeriformes",
                 Habitat = "",Genus = "Turnagra",
                 Primary.Lifestyle = "", Mass = NA, Source = "Wikipedia")]
add_list[[3]] <- copy(addendum)


extinct.dat[scientificName == "Turnagra tanagra", avonet_species := "Turnagra tanagra"]
addendum[, `:=` (Species = "Turnagra tanagra", Family = "Oriolidae", Order = "Passeriformes",
                 Habitat = "", Genus = "Turnagra",
                 Primary.Lifestyle = "", Mass = NA, Source = "Wikipedia")]
add_list[[4]] <- copy(addendum)


#
extinct.dat[scientificName == "Ciridops anna", avonet_species := "Ciridops anna"]
addendum[, `:=` (Species = "Ciridops anna", Family = "Fringillidae", Order = "Passeriformes",
                 Habitat = "", Genus = "Ciridops",
                 Primary.Lifestyle = "", Mass = NA, Source = "Wikipedia")]
add_list[[5]] <- copy(addendum)


#
extinct.dat[scientificName == "Mundia elpenor", avonet_species := "Mundia elpenor"]
addendum[, `:=` (Species = "Mundia elpenor", Family = "Rallidae", Order = "Gruiformes",
                 Habitat = "Desert", Genus = "Mundia",
                 Primary.Lifestyle = "Terrestrial", Mass = NA, Source = "Wikipedia")]
add_list[[6]] <- copy(addendum)


#
extinct.dat[scientificName == "Cabalus modestus", avonet_species := "Cabalus modestus"]
addendum[, `:=` (Species = "Cabalus modestus", Family = "Rallidae", Order = "Gruiformes",
                 Habitat = "", Genus = "Cabalus",
                 Primary.Lifestyle = "Terrestrial", Mass = NA, Source = "Wikipedia")]
add_list[[7]] <- copy(addendum)

#
extinct.dat[scientificName == "Dysmorodrepanis munroi", avonet_species := "Dysmorodrepanis munroi"]
addendum[, `:=` (Species = "Dysmorodrepanis munroi", Family = "Fringillidae", Order = "Passeriformes",
                 Habitat = "Forest", Genus = "Dysmorodrepanis",
                 Primary.Lifestyle = "Terrestrial", Mass = NA, Source = "Wikipedia")]
add_list[[8]] <- copy(addendum)
add_list


add_list <- rbindlist(add_list)
add_list[!Species %in% extinct.dat$avonet_species]

# add_list[, merge_key := Species]
# *** Genus level traits --------------------------------------------------

avonet[Genus == "Alopecoenas"]
extinct.dat[Genus == "Alopecoenas"]
genus.avonet <- avonet[Genus %in% extinct.dat[!avonet_species %in% add_list$Species, ]$Genus, ]
genus.avonet[Genus == "Alopecoenas"]

# genus.avonet[, merge_key := Genus]
genus.avonet <- genus.avonet[, .(Family = paste(sort(unique(Family)), collapse = "; "),
                                     Order = paste(sort(unique(Order)), collapse = "; "),
                                     Habitat = paste(sort(unique(Habitat)), collapse = "; "),
                                 Primary.Lifestyle = paste(sort(unique(Primary.Lifestyle)), collapse = "; "),
                                     Mass = mean(Mass)),
                                 by = .(Genus)]
genus.avonet
genus.avonet[Genus == "Alopecoenas"]
extinct.dat[Genus == "Alopecoenas"]


# Merge in the species name from extinct dat for Species field
genus.avonet <- merge(genus.avonet,
                      extinct.dat[, .(Genus, avonet_species)],
                      by = "Genus")

genus.avonet[, Species := avonet_species]
genus.avonet[, `:=` (avonet_species = NULL,
                     Source = "Genus average")]
genus.avonet
genus.avonet[Genus == "Alopecoenas"]

# This is a goddamn mess
setdiff(extinct.dat$avonet_species,
        genus.avonet$Species)

# *** Bind extinct species together ---------------------------------------

#
extinct.avonet <- rbind(add_list,
                        genus.avonet, fill = T)

extinct.avonet

setdiff(extinct.dat$avonet_species,
        extinct.avonet$Species)
# Thank god. That took forever. Jesus christ.



# *** reconcile order conflicts -------------------------------------------

extinct.avonet[grepl(";", Order)]
# good.
# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------

# Bind extant and extinct species back together ---------------------------

extant.avonet[, Status := "Extant"]
extinct.avonet[, Status := "Extinct"]

final.traits.taxonomy <- rbind(extant.avonet, extinct.avonet,
                               fill = T)

final.traits.taxonomy

extinct.dat
extant.dat

merge_key <- rbind(extinct.dat[, .(scientificName, avonet_species)],
                   extant.dat[, .(scientificName, avonet_species)])

setdiff(merge_key$avonet_species, final.traits.taxonomy$Species)

setdiff(merge_key$scientificName, dat$scientificName)


# *** Bind in key to relate to evidence database --------------------------

final.traits.taxonomy.mrg <- merge(final.traits.taxonomy,
                                   merge_key,
                                   all.x = T,
                                   by.x = "Species",
                                   by.y = "avonet_species")

final.traits.taxonomy.mrg
setnames(final.traits.taxonomy.mrg, "Species", "Avonet_Species")
final.traits.taxonomy.mrg

setdiff(dat$scientificName,
        final.traits.taxonomy.mrg$scientificName)
# Thank god. 

final.traits.taxonomy.mrg[duplicated(scientificName)]
final.traits.taxonomy.mrg[scientificName == "Acrocephalus aequinoctialis"]
final.traits.taxonomy.mrg <- unique(final.traits.taxonomy.mrg)
# ok good.


# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# Classify seabirds versus land birds? -------------------------------------

final.traits.taxonomy.mrg

unique(final.traits.taxonomy.mrg$Order)

final.traits.taxonomy.mrg[Order %in% c("Passeriformes",
                                       "Caprimulgiformes", "Columbiformes",
                                       "Psittaciformes", "Gruiformes",
                                       "Apterygiformes", "Charadriiformes",
                                       "Cuculiformes", "Galliformes",
                                       "Falconiformes", "Mesitornithiformes",
                                       "Strigiformes", "Eurypygiformes",
                                       "Bucerotiformes", "Apodiformes",
                                       "Coraciiformes",
                                       "Anseriformes"),
                          Bird_Type_Coarse := "Landbirds"]

final.traits.taxonomy.mrg[Order %in% c("Procellariiformes", "Pelecaniformes",
                                       "Sphenisciformes", "Suliformes",
                                       ""),
                          Bird_Type_Coarse := "Seabirds"]
final.traits.taxonomy.mrg[is.na(Bird_Type_Coarse), ]$Order

# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# Flightlessness? ---------------------------------------------------------
final.traits.taxonomy.mrg

flight <- read_excel("data/Traits/Sayol_et_al_2020_Sci_Adv_Flightlessness.xlsx")
flight
setDT(flight)

flight2 <- read_excel("data/Traits/Sayol_et_al_2020_flightless2.xlsx")
flight2
setDT(flight2)

flight <- rbind(flight[, .(species, Volancy, IslandEndemic)],
                flight2[, .(species, Volancy, IslandEndemic)])


unique(flight$species)
setdiff(unique(final.traits.taxonomy.mrg$Avonet_Species), unique(flight$species))
setdiff(unique(final.traits.taxonomy.mrg$Avonet_Species), unique(flight$species))
# ~7 non-matches.
final.traits.taxonomy.mrg[Avonet_Species %in% flight$species]
final.traits.taxonomy.mrg[scientificName %in% flight$species]

#
flights.traits.taxonomy.mrg2 <- merge(final.traits.taxonomy.mrg,
                                      flight,
                                      by.x = "Avonet_Species",
                                      by.y = "species",
                                      all.x = T,
                                      all.y = F)

flights.traits.taxonomy.mrg2[!is.na(Volancy), Volancy_Source := "Sayol et al. 2020 Sci Adv"]
flights.traits.taxonomy.mrg2

flights.traits.taxonomy.mrg2[is.na(Volancy), ]

unique(flights.traits.taxonomy.mrg2$IslandEndemic)
unique(flights.traits.taxonomy.mrg2$Volancy)
flights.traits.taxonomy.mrg2[Avonet_Species == "Alexandrinus eques", 
                             `:=` (Volancy = "Volant",
                                   IslandEndemic = "Yes",
                                   Volancy_Source = "Birds of the World")]
flights.traits.taxonomy.mrg2[Avonet_Species == "Aphrastura masafuerae", 
                             `:=` (Volancy = "Volant",
                                   IslandEndemic = "Yes",
                                   Volancy_Source = "Birds of the World")]

flights.traits.taxonomy.mrg2[Avonet_Species == "Cincloramphus grosvenori", 
                             `:=` (Volancy = "Volant",
                                   IslandEndemic = "Yes",
                                   Volancy_Source = "Birds of the World")]

flights.traits.taxonomy.mrg2[Avonet_Species == "Cincloramphus llaneae", 
                             `:=` (Volancy = "Volant",
                                   IslandEndemic = "Yes",
                                   Volancy_Source = "Birds of the World")]

flights.traits.taxonomy.mrg2[Avonet_Species == "Cincloramphus rufus", 
                             `:=` (Volancy = "Volant",
                                   IslandEndemic = "Yes",
                                   Volancy_Source = "Birds of the World")]

flights.traits.taxonomy.mrg2[Avonet_Species == "Melopyrrha grandis", 
                             `:=` (Volancy = "Volant",
                                   IslandEndemic = "Yes",
                                   Volancy_Source = "Birds of the World")]

flights.traits.taxonomy.mrg2[Avonet_Species == "Pelecanoides whenuahouensis", 
                             `:=` (Volancy = "Volant",
                                   IslandEndemic = "Yes",
                                   Volancy_Source = "Birds of the World")]
flights.traits.taxonomy.mrg2[is.na(Volancy)]


# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# Migratory ---------------------------------------------------------
flights.traits.taxonomy.mrg2

breeding.loc <- fread("data/Traits/Rivas_Salvador_2023_Science_of_Nature.csv")
breeding.loc[duplicated(Species), ]
setnames(breeding.loc, names(breeding.loc), gsub(" ", "_", names(breeding.loc)))
setnames(breeding.loc, names(breeding.loc), gsub(")", "", names(breeding.loc)))
setnames(breeding.loc, names(breeding.loc), gsub("\\(", "", names(breeding.loc)))

setdiff(flights.traits.taxonomy.mrg2$Avonet_Species, 
        breeding.loc$Species)
setdiff(flights.traits.taxonomy.mrg2$scientificName, 
        breeding.loc$Species)

#
flights.traits.taxonomy.mrg3 <- merge(flights.traits.taxonomy.mrg2,
                                      breeding.loc[, .(Species, Migratory_status,
                                                       Extent_of_occurrence_km2,
                                                       Generation_length_yrs,
                                                       Neartic, Paleartic,
                                                       Afrotropical, Neotropical,
                                                       Indomalayan, Australasian,
                                                       Oceanic)],
                                      by.x = "Avonet_Species",
                                      by.y = "Species",
                                      all.x = T,
                                      all.y = F)
flights.traits.taxonomy.mrg3
flights.traits.taxonomy.mrg3

# Not going to populate missing ones yet.
# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# Breeding location -------------------------------------------------------

breeding.loc <- read_excel("data/Traits/Spatz_et_al_2017_Sci_Adv.xlsx",
                           skip = 1)
breeding.loc
setDT(breeding.loc)

setnames(breeding.loc, names(breeding.loc), gsub(" ", "_", names(breeding.loc)))
breeding.loc
setnames(breeding.loc, names(breeding.loc), gsub(")", "", names(breeding.loc)))
setnames(breeding.loc, names(breeding.loc), gsub("\\(", "", names(breeding.loc)))
breeding.loc
setnames(breeding.loc, names(breeding.loc), gsub("#_Islands_extant", "Number_islands_extant", names(breeding.loc)))

breeding.loc

setdiff(flights.traits.taxonomy.mrg3$Avonet_Species,
        breeding.loc$Scientific_Name)

setdiff(flights.traits.taxonomy.mrg3$scientificName,
        breeding.loc$Scientific_Name)


flights.traits.taxonomy.mrg4 <- merge(flights.traits.taxonomy.mrg3,
                                      breeding.loc[, .(Scientific_Name, Breeding_Distribution,
                                                       Number_islands_extant)],
                                      by.x = "Avonet_Species",
                                      by.y = "Scientific_Name",
                                      all.x = T,
                                      all.y = F)

flights.traits.taxonomy.mrg4
# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# Nest type -------------------------------------
flights.traits.taxonomy.mrg4

nests <- fread("data/Traits/NestTraits_Yuan_et_al_2023/NestTrait_v2.csv")
nests
fread("data/Traits/NestTraits_Yuan_et_al_2023/NestTrait_v2_metadata.csv")

setdiff(flights.traits.taxonomy.mrg4$Avonet_Species,
         nests$Scientific_name)
#

#
nests <- nests[, .(Scientific_name, Parasite,
                   Mound, NestSite_ground, NestSite_tree,
                   NestSite_nontree, NestSite_cliff_bank,
                   NestSite_underground, NestSite_waterbody,
                   NestSite_termite_ant)]
nests

flights.traits.taxonomy.mrg5 <- merge(flights.traits.taxonomy.mrg4,
                                      nests,
                                      by.x = "Avonet_Species",
                                      by.y = "Scientific_name",
                                      all.x = T,
                                      all.y = F)

nrow(flights.traits.taxonomy.mrg5[is.na(NestSite_ground)])
nrow(flights.traits.taxonomy.mrg5[!is.na(NestSite_ground)])

# Too much to do manually, but ehre are some:
# Aphrastura masafuerae == NestSite_cliff_bank
# Cincloramphus grosvenori
nests[grepl("Cincloramphus", Scientific_name)]
# ground and non-tree veg
# Cincloramphus llaneae
# ground and non-tree veg
#Cincloramphus rufus
# ground and non-tree veg
#Melopyrrha grandis
nests[grepl("Melopyrrha", Scientific_name)]
nests[grepl("Loxigilla", Scientific_name)]
# I guess tree and non-tree

#
# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# Create a synthetic Bird type column -------------------------------------
unique(flights.traits.taxonomy.mrg4$Volancy)
flights.traits.taxonomy.mrg5[, Bird_Type := ifelse(Volancy == "Flightless",
                                                   paste("Flightless", Bird_Type_Coarse),
                                                   Bird_Type_Coarse)]

unique(flights.traits.taxonomy.mrg5$Bird_Type)

# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# Save ----------------------------------------------------------------
fwrite(flights.traits.taxonomy.mrg5,
       "data/Working_Databases/Provisional_Trait_Taxonomy.csv")

# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------


# OLD ---------------------------------------------------------------------


#
# add_list[[9]] <- avonet[Genus %in% extinct.dat$Genus]

extinct.avonet <- rbindlist(add_list,
                            fill = T)


extinct.avonet

extinct.avonet[Species %in% extinct.dat$Species, merge_key := Species]
extinct.avonet[!Species %in% extinct.dat$Species, merge_key := Genus]

extinct.avonet

setdiff(extinct.dat$avonet_species,
        extinct.avonet$merge_key)





#
extinct.avonet <- avonet[Genus %in% extinct.dat$avonet_species]

extinct.avonet <- extinct.avonet[, .(Family = paste(sort(unique(Family)), collapse = "; "),
                                   Order = paste(sort(unique(Order)), collapse = "; "),
                                   Habitat = paste(sort(unique(Habitat)), collapse = "; "),
                                   Mass = mean(Mass)),
                               by = .(merge_key, Genus)]
extinct.avonet[grepl(";", Family)]
# Let's ignore these contradictions for now.
extinct.avonet[grepl(";", Order)]
# Let's fix these though...
extinct.avonet


extinct.dat[!avonet_species %in% extinct.avonet$merge_key, ]$Genus




# ---------Acrocephalus musae----------- -----------------------------------------------!
sort(unique(avonet[grepl("Acrocephalus", Species)]$Species))
unique(avonet[grepl("Acrocephalus", Species)])
addendum[, `:=` (Species = "Acrocephalus musae",
                 Family = "Acrocephalidae",
                 Order = "Passeriformes",
                 Habitat = NA,
                 Primary.Lifestyle = "Insessorial",
                 Mass = mean(avonet[grepl("Acrocephalus", Species), ]$Mass),
                 Source = "Genus mean")]
to_add[[1]] <- addendum

dat[scientificName == "Acrocephalus musae", avonet_species := "Acrocephalus musae"]

# ---------Acrocephalus nijoi----------- -----------------------------------------------!

avonet[grepl("XXX", Species)]
addendum[, `:=` (Species = "Acrocephalus nijoi",
                 Family = "Acrocephalidae",
                 Order = "Passeriformes",
                 Habitat = NA,
                 Primary.Lifestyle = "Insessorial",
                 Mass = mean(avonet[grepl("Acrocephalus", Species), ]$Mass),
                 Source = "Genus mean")]

to_add[[2]] <- addendum

dat[scientificName == "Acrocephalus nijoi", avonet_species := "Acrocephalus nijoi"]

# ---------Alectroenas payandeei----------- -----------------------------------------------!

avonet[grepl("Alectroenas", Species)]
addendum[, `:=` (Species = "Alectroenas payandeei",
                 Family = "Columbidae",
                 Order = "Columbiformes",
                 Habitat = "Forest",
                 Primary.Lifestyle = "Insessorial",
                 Mass = mean(avonet[grepl("Alectroenas", Species), ]$Mass),
                 Source = "Genus mean")]
to_add[[3]] <- addendum

dat[scientificName == "Alectroenas payandeei", avonet_species := "Alectroenas payandeei"]

# ---------Anthornis melanocephala----------- -----------------------------------------------!
avonet[grepl("Anthornis", Species)]

addendum[, `:=` (Species = "Anthornis melanocephala",
                 Family = "Meliphagidae",
                 Order = "Passeriformes",
                 Habitat = "Forest",
                 Primary.Lifestyle = "Insessorial",
                 Mass = mean(avonet[grepl("Anthornis", Species), ]$Mass),
                 Source = "Genus mean")]
to_add[[4]] <- addendum

dat[scientificName == "Anthornis melanocephala", avonet_species := "nthornis melanocephala"]

# ---------Aplonis corvina----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Aplonis corvina", avonet_species := "XXXXX"]

# ---------Aplonis fusca----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Aplonis fusca", avonet_species := "XXXXX"]

# ---------Aplonis mavornata----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Aplonis mavornata", avonet_species := "XXXXX"]

# ---------Aplonis ulietensis----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Aplonis ulietensis", avonet_species := "XXXXX"]

# ---------Cabalus modestus----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Cabalus modestus", avonet_species := "XXXXX"]

# ---------Carpodacus ferreorostris----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Carpodacus ferreorostris", avonet_species := "XXXXX"]

# ---------Chenonetta finschi----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Chenonetta finschi", avonet_species := "XXXXX"]

# ---------Ciridops anna----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Ciridops anna", avonet_species := "XXXXX"]

# ---------Coenocorypha barrierensis----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Coenocorypha barrierensis", avonet_species := "XXXXX"]

# ---------Columba thiriouxi----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Columba thiriouxi", avonet_species := "XXXXX"]

# ---------Columba versicolor----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Columba versicolor", avonet_species := "XXXXX"]

# ---------Coturnix novaezelandiae----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Coturnix novaezelandiae", avonet_species := "XXXXX"]

# ---------Coua delalandei----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Coua delalandei", avonet_species := "XXXXX"]

# ---------Drepanis funerea----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Drepanis funerea", avonet_species := "XXXXX"]

# ---------Dryolimnas augusti----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Dryolimnas augusti", avonet_species := "XXXXX"]

# ---------Dysmorodrepanis munroi----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Dysmorodrepanis munroi", avonet_species := "XXXXX"]

# ---------Foudia delloni----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Foudia delloni", avonet_species := "XXXXX"]

# ---------Gerygone insularis----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Gerygone insularis", avonet_species := "XXXXX"]

# ---------Haematopus meadewaldoi----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Haematopus meadewaldoi", avonet_species := "XXXXX"]

# ---------Hypotaenidia dieffenbachii----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Hypotaenidia dieffenbachii", avonet_species := "XXXXX"]

# ---------Hypotaenidia pacifica----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Hypotaenidia pacifica", avonet_species := "XXXXX"]

# ---------Mergus australis----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Mergus australis", avonet_species := "XXXXX"]

# ---------Moho bishopi----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Moho bishopi", avonet_species := "XXXXX"]

# ---------Moho braccatus----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Moho braccatus", avonet_species := "XXXXX"]

# ---------Mundia elpenor----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Mundia elpenor", avonet_species := "XXXXX"]

# ---------Nesillas aldabrana----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Nesillas aldabrana", avonet_species := "XXXXX"]

# ---------Nesoenas cicur----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Nesoenas cicur", avonet_species := "XXXXX"]

# ---------Nesoenas duboisi----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Nesoenas duboisi", avonet_species := "XXXXX"]

# ---------Nesoenas rodericanus----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Nesoenas rodericanus", avonet_species := "XXXXX"]

# ---------Pampusana salamonis----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Pampusana salamonis", avonet_species := "XXXXX"]

# ---------Pomarea fluxa----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Pomarea fluxa", avonet_species := "XXXXX"]


# ---------Pomarea nukuhivae----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Pomarea nukuhivae", avonet_species := "XXXXX"]

# ---------Porphyrio paepae----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Porphyrio paepae", avonet_species := "XXXXX"]

# ---------Prosobonia ellisi----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Prosobonia ellisi", avonet_species := "XXXXX"]

# ---------Prosobonia leucoptera----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Prosobonia leucoptera", avonet_species := "XXXXX"]

# ---------Ptilinopus mercierii----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Ptilinopus mercierii", avonet_species := "XXXXX"]

# ---------Pyrocephalus dubius----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Pyrocephalus dubius", avonet_species := "XXXXX"]

# ---------Tribonyx hodgenorum----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Tribonyx hodgenorum", avonet_species := "XXXXX"]

# ---------Turnagra capensis----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Turnagra capensis", avonet_species := "XXXXX"]

# ---------Turnagra tanagra----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Turnagra tanagra", avonet_species := "XXXXX"]

# ---------Zapornia monasa----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Zapornia monasa", avonet_species := "XXXXX"]

# ---------Zapornia palmeri----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Zapornia palmeri", avonet_species := "XXXXX"]

# ---------Zapornia sandwichensis----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Zapornia sandwichensis", avonet_species := "XXXXX"]

# ---------Zoothera terrestris----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Zoothera terrestris", avonet_species := "XXXXX"]

# ---------Zosterops strenuus----------- -----------------------------------------------!
avonet[Species == "XXXX"]
dat[scientificName == "Zosterops strenuus", avonet_species := "XXXXX"]

