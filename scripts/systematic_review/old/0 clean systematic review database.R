#
#
# Load and clean systematic review database
#
#

rm(list = ls())

library("groundhog")

date <- "2024-07-15"
pcks <- c("data.table", "ggplot2", "tidyr", "readxl")
groundhog.library(pcks, date)

# groundhog.library("github::nickmckay/geoChronR",
#                   date)
# remotes::install_github("nickmckay/geoChronR")
# library("geoChronR")

# Load and clean data -----------------------------------------------------
dat <- read_excel("data/Raw/Systematic_review.xlsx", 
                  na = "NA")
dat
setDT(dat)

dat[scientificName == "Treron griveaudi", ]$Rodent_attributed_IUCN

names(dat)
names(dat)[duplicated(names(dat))]
# Good.
names(dat)[grepl(" ", names(dat))]
setnames(dat, names(dat), gsub(" ", "_", names(dat)))
names(dat)[grepl("/", names(dat))]
names(dat)[grepl("-", names(dat))]
setnames(dat, names(dat), gsub("-", "_", names(dat)))

dat <- dat[, !c("RODENT_MENTIONED_IN_THREATS", 
                "RODENT_MENTIONED_IN_CONSERVATION_ACTIONS", 
                "RODENT_MENTIONED_IN_RATIONALE", 
                "RODENT_MENTIONED_SOMEWHERE",
                "possiblyExtinct", "possiblyExtinctInTheWild", "scopes",
                "language")]
# Drop some spurious columns

# 
# dat[, original_row_id := seq(1:.N)]
# dat
dat <- dat[!scientificName %in% "Acrocephalus familiaris", ]

dat[scientificName == "Treron griveaudi", ]$Rodent_attributed_IUCN

dat$Primary_causes
dat[scientificName == "Acrocephalus aequinoctialis", ]$Primary_causes
dat[scientificName == "Acrocephalus aequinoctialis", Primary_causes := "Unknown"]
dat[scientificName == "Aerodramus bartschi", ]$Percent_nests_or_individuals_predated
unique(dat$Percent_nests_or_individuals_predated)
# what the hell. Damnit. These are real bad.

dat[scientificName == "Puffinus opisthomelas", ]
dat[scientificName == "Acrocephalus aequinoctialis", ]
# @@@ NOTES ---------------------------------------------------------------
# Treron griveaudi has "" for attribution. ADW:Shouldn’t be blank - I’ve got Rattus rattus listed in my file

# Split into 2 databases --------------------------------------------------
names(dat)
# names(dat)[29:106]
# dput(names(dat)[29:106])
# These are the 'other causes'
nms <- c("Deforestation_logging", "Development", "Mining", "Farming_and_other_habitat_loss", 
         "Human_presence", "Human_hunting_harvesting", "Human_foraging", 
         "Domestic_grazing", "Ending_domestic_grazing", "Fire_burning", 
         "Garbage_plastic", "Artificial_light", "Collision", "Fishing_bycatch_collision", 
         "Fishing_prey_loss", "Toxins_pesticides_pollution_oil_spills_radioactive_exposure_heavy_metals", 
         "Pollinator_loss", "Tree_dieback", "Global_warming", "Landslides", 
         "Volcano", "Flood", "Ocean_swell_tsunami", "Prey_base_currents_temp_weather", 
         "Weather_extreme_unfavourable_sea_temp_cyclone", "Pig_introduced", 
         "Cat_introduced", "Mongoose_introduced", "Opposum_introduced", 
         "Coati_introduced", "Civets_introduced", "Ferret_introduced", 
         "Raccoon_introduced", "Mink_introduced", "Foxes_introduced", 
         "Gennet_introduced", "Stoat_introduced", "Weasel_introduced", 
         "Mustelid_family_introduced", "Hedgehog_introduced", "Barbary_ground_squirrels_introduced", 
         "Dog_domestic", "Barn_owl_introduced", "Brown_tree_snake_introduced", 
         "Monitor_lizard_introduced", "Crab_eating_macaques_introduced", 
         "Green_monkey_introduced", "Mona_Monkey_introduced", "Cattle_introduced", 
         "Mouflon_introduced", "Rabbits_introduced", "Donkey_horse_introduced", 
         "Sheep_introduced", "Guinea_pigs_introduced", "Goats_introduced", 
         "Horses_introduced", "Herbivores_introduced_other_or_unspecified", 
         "Brushtail_possum_introduced", "Deer_introduced", "Egret_introduced", 
         "Plants_introduced", "Fish_introduced", "Insects_invertebrates_introduced", 
         "Reptile_introduced", "Hybridization", "Bird_other_introduced", 
         "Myna_introduced", "Seal_native_habitat_effects", "Tortoise_native", 
         "Insects_native", "Reptile_native", "Competition_native_mammal", 
         "Predation_native_mammal", "Competition_native_bird", "Predation_native_bird", 
         "Disease", "Algal_bloom", "Parasites")
nms

other_causes <- unique(dat[, c("assessmentId", "internalTaxonId", "scientificName", "Common_name",
                        "Synonyms_or_previous_lump", "Rodent_attributed_IUCN",
                        nms,
                        "Primary_causes", "Rodent_primary", "Rodent_only_primary"), with = F])
other_causes

fwrite(other_causes, "data/Working_Databases/Other_Causes.csv")

# Now split off evidence:
nms2 <- names(dat)[!names(dat) %in% nms]
evidence <- dat[, nms2, with = F]
evidence

# >>> Fix some errors -----------------------------------------------------

evidence[scientificName == "Puffinus newelli"]
# This species is in here twice...
evidence[scientificName == "Puffinus newelli",
         redlistCategory := "Critically Endangered"]
evidence[scientificName == "Puffinus newelli",
         redlistCriteria := "A3bce+4bce"]

evidence[scientificName == "Prosobonia parvirostris"]

evidence[scientificName == "Prosobonia parvirostris",
         Synonyms_or_previous_lump := "Prosobonia cancellata"]

# >>> Save first draft of database ----------------------------------------

fwrite(evidence, "data/Temp/Evidence July 9 2024.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Reorganize attribution and study rodent to be rodent-focused ----------------------------------------------
# This is tricky. We want each attribution row to be a single rodent taxa, associated with
# all corresponding studies.
evidence <- fread("data/Temp/Evidence July 9 2024.csv")
evidence

# >>> Split evidence into attribution vs evidence -------------------------
names(evidence)
evidence[, has_study := ifelse(is.na(Article) | Article == "", 
                               "No", "Yes")]
evidence$Article

attrib <- unique(evidence[, .(assessmentId, internalTaxonId, scientificName,
                              Common_name, #Range_description,
                              Synonyms_or_previous_lump, Rodent_attributed_IUCN,
                              redlistCategory, redlistCriteria, Primary_causes, Rodent_primary, 
                              Rodent_only_primary, has_study)])
attrib[duplicated(scientificName)]

attrib[, orig_attrib_ID := seq(1:.N)]
attrib

#
studies <- evidence[Study_rodent != "", .(scientificName, Article, Article_secondary_same_data, 
                                          Study_type,
                                          Fishy_Study_Type, Sample_size_nests_monitored, Hypothesis_supported,
                                          Percent_nests_or_individuals_predated, Predator_and_prey_data,
                                          Experiment_control_site_or_spatial_variation, Experiment_sample_size,
                                          Experiment_confounding_variables_included,
                                          Experiment_sites_randomly_selected_REMOVE, Experiment_BACI, Study_location,
                                          Latitude, Longitude, Study_notes, Study_rodent)]
studies
studies[, orig_study_ID := seq(1:.N)]

# >>> Use rat key to identify Rattus species ------------------------
# Need to do this both on attributed and studies...

# Arian prepared a key which will hopefully help....
rat_key <- read_excel("data/Raw/Rattus.xlsx")
rat_key
setDT(rat_key)
unique(rat_key$Rodent_attributed_IUCN)
rat_key
setnames(rat_key, names(rat_key), gsub("-", "_", names(rat_key)))
rat_key
rat_key[scientificName == "Puffinus opisthomelas", ]


rat_key[R_rattus_present == "0" & R_norvegicus_present == "0" &
        R_exulans_present == "0" & Mus_musculus == "0" &
          is.na(Other)]

rat_key.mlt <- melt(rat_key[, .(scientificName, Range_description,
                                Rodent_attributed_IUCN, Other,
                                R_rattus_present, R_norvegicus_present,
                                R_exulans_present, Mus_musculus)],
                    measure.vars = c("R_rattus_present", "R_norvegicus_present",
                                     "R_exulans_present", "Mus_musculus"))
rat_key.mlt
unique(rat_key.mlt$value)
rat_key.mlt[, value := as.numeric(value)]
rat_key.mlt <- rat_key.mlt[value == 1, ]

rat_key.mlt[, variable := gsub("_present", "", variable)]
rat_key.mlt[, variable := gsub("R_", "Rattus ", variable)]
rat_key.mlt

# Reconcatenate to make this formatted similar to attribution / studies (which will be split and melted themselves...But whatever)
rat_key.mlt.conc <- rat_key.mlt[, .(Rodent_attributed_conc = paste(variable, collapse = ", ")),
                                by = .(scientificName, Rodent_attributed_IUCN,
                                       Range_description, Other)]
rat_key.mlt.conc

rat_key.mlt.conc[!is.na(Other), ]$Other
rat_key.mlt.conc[!is.na(Other), Rodent_attributed_conc := paste(Rodent_attributed_conc,
                                                                ", ",
                                                                Other, " (native)")]
rat_key.mlt.conc
rat_key.mlt.conc[scientificName == "Puffinus opisthomelas", ]

# >>> Merge key into attribution ----------------------------
# rat_key.mlt.conc[duplicated(scientificName), ]
# rat_key.mlt.conc[, key := paste(scientificName)]
attrib[duplicated(scientificName), ]
attrib[, key := paste(scientificName)]
attrib[scientificName == "Prosobonia parvirostris"]
attrib[scientificName == "Puffinus newelli"]
rat_key.mlt.conc[duplicated(scientificName), ]

setdiff(rat_key.mlt.conc$scientificName,
        attrib$scientificName)
attrib.mrg <- merge(attrib,
                    rat_key.mlt.conc[, .(scientificName, Rodent_attributed_conc)],
                    by = "scientificName",
                    all.x = T,
                    all.y = T)

attrib.mrg[Rodent_attributed_IUCN == "", ]
unique(attrib.mrg[!is.na(Rodent_attributed_conc)]$Rodent_attributed_IUCN)
attrib.mrg[, .(scientificName, Rodent_attributed_IUCN, Rodent_attributed_conc)]
setnames(attrib.mrg, "Rodent_attributed_IUCN", "Original_rodent_attributed_IUCN")
attrib.mrg[, Rodent_attributed_final := ifelse(is.na(Rodent_attributed_conc),
                                               Original_rodent_attributed_IUCN, Rodent_attributed_conc)]
attrib.mrg[is.na(Rodent_attributed_final)]

attrib.mrg[!is.na(Rodent_attributed_conc),
           Notes := "Attributed rodent species was Rattus spp according to IUCN. Specific rodent was determined from lit review. See Rattus.xlsx for reference and comments"]
attrib.mrg$Rodent_attributed_conc <- NULL

attrib.mrg


attrib.mrg[scientificName == "Puffinus opisthomelas", ]
















# >>> Merge key into studies that are ABOVE species level. ----------------------------
unique(studies$Study_rodent)


studies[scientificName == "Puffinus opisthomelas", ]


studies
# *** Save for Arian to workup species specific identities ----------------
fwrite(studies, "data/Temp/studies_figure_out_species_raw.csv")



# LEFT OFF HERE 
#
#
#
#

studies.complete <- studies[!Study_rodent %in% c("Rattus", "Rodent")]
studies.incomplete <- studies[Study_rodent %in% c("Rattus", "Rodent")]

unique(studies.incomplete$Study_rodent)
# studies[, key := paste(scientificName, Study_rodent)]
# rat_key.mlt.conc[, key := paste(scientificName, "Rattus")]
# setdiff(rat_key.mlt.conc$key, studies$key)
nrow(studies.incomplete)
studies.incomplete.mrg <- merge(studies.incomplete,
                                rat_key.mlt.conc[, .(scientificName, Rodent_attributed_conc)],
                                by = "scientificName",
                                all.x = T,
                                all.y = F)

studies.incomplete.mrg[!is.na(Rodent_attributed_conc)]
# Still missing quite a few....

studies.incomplete.mrg2 <- merge(studies.incomplete.mrg,
                                 attrib.mrg[, .(scientificName, Rodent_attributed_final)],
                                 by = "scientificName",
                                 all.x = T,
                                 all.y = F)

studies.incomplete.mrg2[, Study_rodent_final := ifelse(!is.na(Rodent_attributed_conc),
                                                       Rodent_attributed_conc, Rodent_attributed_final)]

unique(studies.incomplete.mrg2$Study_rodent_final)

studies.incomplete.mrg2[, Study_rodent_final := gsub("Mus musculus", "", Study_rodent_final)]
studies.incomplete.mrg2[, `:=` (Rodent_attributed_final = NULL,
                                Rodent_attributed_conc = NULL)]

studies.incomplete.mrg2[, Study_rodent_notes := "Study only reported rodent as 'Rattus' or 'Rodent'. We used IUCN attribution and rattus range file to assign species."]
studies.incomplete.mrg2

studies.complete[, Study_rodent_notes := NA]
studies.complete[, Study_rodent_final := Study_rodent]

studies.recomb <- rbind(studies.complete,
                        studies.incomplete.mrg2)
studies.recomb

# >>> Separate and melt ---------------------------------------------------
attrib.mrg
studies.recomb

attrib.mrg.sep <- attrib.mrg %>%
  separate_longer_delim(cols = "Rodent_attributed_final",
                        delim = ",")
attrib.mrg.sep
setdiff(attrib.mrg$scientificName, attrib.mrg.sep$scientificName)
# must be length 0
setDT(attrib.mrg.sep)

studies.recomb.sep <- studies.recomb %>%
  separate_longer_delim(cols = "Study_rodent_final",
                        delim = ",")
setdiff(studies.recomb$scientificName, studies.recomb.sep$scientificName)
# must be length 0
setDT(studies.recomb.sep)
setnames(studies.recomb.sep, "Study_rodent", "Original_study_rodent")

attrib.mrg.sep[, `:=` (scientificName = trimws(scientificName),
                       Rodent_attributed_final = trimws(Rodent_attributed_final))]

studies.recomb.sep[, `:=` (scientificName = trimws(scientificName),
                           Study_rodent_final = trimws(Study_rodent_final))]

# >>> Merge evidence back into attribution by rodent and bird species --------------------------------
attrib.mrg.sep[, key := paste(scientificName, Rodent_attributed_final, sep = "_")]
studies.recomb.sep[, key := paste(scientificName, Study_rodent_final, sep = "_")]

setdiff(studies.recomb.sep$key, attrib.mrg.sep$key)
attrib.mrg.sep[scientificName == "Eunymphicus cornutus", ]
studies.recomb.sep[scientificName == "Eunymphicus cornutus", ]
# OK...How do screen these out?
# Hmmm.
final_dat <- merge(attrib.mrg.sep,
                   studies.recomb.sep[, !"scientificName"],
                   by = "key",
                   all.x = T,
                   all.y = F)

final_dat[is.na(redlistCategory)]
# should be 0 rows.

# OK. I guess we're good then?
setdiff(attrib.mrg.sep$scientificName, final_dat$scientificName)

final_dat$key <- NULL

fwrite(final_dat, "data/Working_Databases/Systematic_review_attribution_and_evidence.csv")


