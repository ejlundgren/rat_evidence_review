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
dat <- read_excel("data/Raw/Systematic_review-10-Dec.xlsx", 
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

setnames(dat, "Sample_size>1", "Sample_size_greater_than_1")

names(dat)
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

dat$Assigned_local_rodent
dat[!is.na(Assigned_local_rodent), ]$Study_rodent

dat[scientificName == "Pseudobulweria rostrata" & !is.na(Evidence_category)]
dat[scientificName == "Pseudobulweria rostrata" & is.na(Article)]
dat[scientificName == "Pseudobulweria rostrata" ]$Article
# dat[scientificName == "Pseudobulweria rostrata" & !is.na(Evidence_category),
#     Article := "BirdLife International. 2018. Pseudobulweria rostrata. The IUCN Red List of Threatened Species 2018: e.T22697925A132612667. https://dx.doi.org/10.2305/IUCN.UK.2018-2.RLTS.T22697925A132612667.en. Accessed on 11 September 2024"]

#
dat[grepl("zino", Article, ignore.case = T)]$Article
#
dat[grepl("Innes", Article, ignore.case = T)]$Article
#
dat[grepl("Wills", Article, ignore.case = T)]$Article
#
dat[grepl("Rayner", Article, ignore.case = T)]$Article
#
dat[grepl("VanderWerf", Article, ignore.case = T)]$Article
#
dat[grepl("Cruz", Article, ignore.case = T)]$Article
#

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
                        "redlistCategory",
                        nms,
                        "Primary_causes", "Rodent_primary", "Rodent_only_primary"), with = F])
other_causes

fwrite(other_causes, "data/Working_Databases/Other_Causes.csv")

# Now split off evidence:
nms2 <- names(dat)[!names(dat) %in% nms]
evidence <- dat[, nms2, with = F]
evidence

# Is this column attribution or studies:
dat[!is.na(Predation_type) & is.na(Article)]

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

# attrib[, orig_attrib_ID := seq(1:.N)]
attrib

#
studies <- evidence[Study_rodent != "", .(scientificName, Article, Article_secondary_same_data, 
                                          Evidence_category, Evidence_effect, Evidence_method,
                                          Study_rodent, Assigned_local_rodent,
                                          Exclude_row, Sample_size_greater_than_1,
                                          Predation_type,
                                          Sample_size_nests_monitored, Hypothesis_supported,
                                          Percent_nests_or_individuals_predated, Predator_and_prey_population_data,
                                          Experiment_control_site_or_spatial_variation, Note_experiment_sample_size,
                                          Experiment_confounding_variables_included,
                                          Experiment_BACI, Study_location,
                                          Latitude, Longitude, Study_notes)]
studies
# studies[, orig_study_ID := seq(1:.N)]

studies$Article
studies[Article != "", Study_ID := seq(1:.N)]

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

rat_key[scientificName == "Gymnocrex talaudensis"]

# Arian datasets...
unique(rat_key$Other)
rat_key[is.na(Other), other_val := 0]
rat_key[!is.na(Other), other_val := 1]
rat_key.cst <- dcast(rat_key,
                     ... ~ Other,
                     value.var = "other_val",
                     fill = 0)
rat_key.cst$`NA` <- NULL
rat_key.cst
nrow(rat_key.cst) == nrow(rat_key)
# should be the same number of rows, strangely. Hahaha

all(c("R_rattus_present", "R_norvegicus_present",
      "R_exulans_present", "Mus_musculus",
      "Rattus argentiventer", "Rattus losea", "Rattus praetor",
      "Rattus tanezumi", "Rattus tiomanicus") %in% names(rat_key.cst))

rat_key.mlt <- melt(rat_key.cst,
                    measure.vars = c("R_rattus_present", "R_norvegicus_present",
                                     "R_exulans_present", "Mus_musculus",
                                     "Rattus argentiventer", "Rattus losea", "Rattus praetor",
                                     "Rattus tanezumi", "Rattus tiomanicus"))
rat_key.mlt
unique(rat_key.mlt$value)
rat_key.mlt[, value := as.numeric(value)]
rat_key.mlt[is.na(value), value := 0]
rat_key.mlt[scientificName == "Gymnocrex talaudensis"]

rat_key.mlt <- rat_key.mlt[value == 1, ]

rat_key.mlt[, variable := gsub("_present", "", variable)]
rat_key.mlt[, variable := gsub("R_", "Rattus ", variable)]
rat_key.mlt

unique(rat_key.mlt$variable)

# Reconcatenate to make this formatted similar to attribution / studies (which will be split and melted themselves...But whatever)
rat_key.mlt[scientificName == "Gymnocrex talaudensis"]

rat_key.mlt.conc <- rat_key.mlt[, .(Rodent_attributed_conc = paste(variable, collapse = ", ")),
                                by = .(scientificName, Rodent_attributed_IUCN)]
rat_key.mlt.conc

rat_key.mlt.conc[, Rodent_attributed_conc := gsub("_", " ", Rodent_attributed_conc)]
rat_key.mlt.conc[scientificName == "Puffinus opisthomelas", ]

setnames(rat_key.mlt.conc, "Rodent_attributed_conc", "Rodent_attributed_manual")
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
                    rat_key.mlt.conc[, .(scientificName, Rodent_attributed_manual)],
                    by = "scientificName",
                    all.x = T,
                    all.y = T)

attrib.mrg[Rodent_attributed_IUCN == "", ]
unique(attrib.mrg[!is.na(Rodent_attributed_manual)]$Rodent_attributed_IUCN)

attrib.mrg[, .(scientificName, Rodent_attributed_IUCN, Rodent_attributed_manual)]
setnames(attrib.mrg, "Rodent_attributed_IUCN", "Original_rodent_attributed_IUCN")

unique(attrib.mrg[is.na(Rodent_attributed_manual)]$Original_rodent_attributed_IUCN)
# Ah missed one huh.

attrib.mrg[is.na(Rodent_attributed_manual) & Original_rodent_attributed_IUCN == "Rattus", ]

rat_key[scientificName == "Aptenorallus calayanensis"] # None are present.

attrib.mrg[, Rodent_attributed_final := ifelse(is.na(Rodent_attributed_manual),
                                               Original_rodent_attributed_IUCN, Rodent_attributed_manual)]
attrib.mrg[is.na(Rodent_attributed_final)]
# attrib.mrg[!is.na(Rodent_attributed_conc),
#            Notes := "Attributed rodent species was Rattus spp according to IUCN. Specific rodent was determined from lit review. See Rattus.xlsx for reference and comments"]

attrib.mrg

attrib.mrg[, .(Original_rodent_attributed_IUCN, 
               Rodent_attributed_manual, 
               Rodent_attributed_final)]

attrib.mrg[is.na(Rodent_attributed_manual), .(Original_rodent_attributed_IUCN, 
                                            Rodent_attributed_manual, 
                                            Rodent_attributed_final)]

attrib.mrg[is.na(Rodent_attributed_manual), Rodent_attribution_source := "Rodent attribution from IUCN."]

attrib.mrg[!is.na(Rodent_attributed_manual), Rodent_attribution_source := "IUCN attribution was generic. Specific rodent was determined from literature review. See Table SX for references and notes."]
attrib.mrg[is.na(Rodent_attribution_source)]

attrib.mrg[scientificName == "Puffinus opisthomelas", ]

attrib.mrg$Rodent_attributed_manual <- NULL

# >>> Merge key into studies that are ABOVE species level. ----------------------------

unique(studies$Study_rodent)

studies[scientificName == "Puffinus opisthomelas", ]

studies
#
studies[, Reported_study_rodent := Study_rodent]

#
studies[!is.na(Assigned_local_rodent) & Assigned_local_rodent != "", 
        .(Study_rodent, Assigned_local_rodent)]

studies[!is.na(Assigned_local_rodent) & Assigned_local_rodent != "", 
        Study_rodent := gsub(Study_rodent, Assigned_local_rodent, Study_rodent)]
#
studies[!is.na(Assigned_local_rodent) & Assigned_local_rodent != "", 
        Study_rodent_notes := "Authors reported rodent as 'Rattus'. Based on REF, this must have been Rattus rattus"]
#
studies$Assigned_local_rodent <- NULL

unique(studies[, .(Reported_study_rodent, Study_rodent, Study_rodent_notes)])

#
unique(studies$Study_rodent)
studies.complete <- studies[!Study_rodent %in% c("Rattus", "Rodent")]
studies.incomplete <- studies[Study_rodent %in% c("Rattus", "Rodent")]

unique(studies.incomplete$Study_rodent)
# studies[, key := paste(scientificName, Study_rodent)]
# rat_key.mlt.conc[, key := paste(scientificName, "Rattus")]
# setdiff(rat_key.mlt.conc$key, studies$key)
nrow(studies.incomplete)
studies.incomplete.mrg <- merge(studies.incomplete,
                                attrib.mrg[, .(scientificName, Rodent_attributed_final, Rodent_attribution_source)],
                                by = "scientificName",
                                all.x = T,
                                all.y = F)
studies.incomplete.mrg[is.na(Rodent_attributed_final)]

studies.incomplete.mrg

# should be 0 rows:
studies.incomplete.mrg[!is.na(Study_rodent_notes), ]
# 
studies.incomplete.mrg[, Study_rodent := Rodent_attributed_final]

studies.incomplete.mrg[Rodent_attribution_source == "Rodent attribution from IUCN.", 
                       Study_rodent_notes := paste0("Rodent species assigned according to IUCN attribution")]


studies.incomplete.mrg[Rodent_attribution_source == "IUCN attribution was generic. Specific rodent was determined from literature review. See Table SX for references and notes.", 
                       Study_rodent_notes := paste0("Rodent species determined from literature review (Table SX)")]


studies.incomplete.mrg[Reported_study_rodent != "Rodent" & grepl("Mus", Study_rodent), ]
studies.incomplete.mrg[Reported_study_rodent != "Rodent", Study_rodent := gsub("Mus musculus", "", Study_rodent)]


studies.incomplete.mrg[, `:=` (Rodent_attributed_final = NULL,
                               Rodent_attribution_source = NULL)]
studies.incomplete.mrg

studies.recomb <- rbind(studies.complete,
                        studies.incomplete.mrg)
studies.recomb

unique(studies.recomb$Study_rodent_notes)

setnames(studies.recomb, "Study_rodent", "Study_rodent_final")

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

unique(studies.recomb$Study_rodent_final)
studies.recomb.sep <- studies.recomb %>%
  separate_longer_delim(cols = "Study_rodent_final",
                        delim = ",")
setdiff(studies.recomb$scientificName, studies.recomb.sep$scientificName)
# must be length 0
setDT(studies.recomb.sep)
studies.recomb.sep

unique(studies.recomb.sep$Study_rodent_final)
unique(attrib.mrg.sep$Rodent_attributed_final)

#
attrib.mrg.sep[, `:=` (scientificName = trimws(scientificName),
                       Rodent_attributed_final = trimws(Rodent_attributed_final))]

studies.recomb.sep[, `:=` (scientificName = trimws(scientificName),
                           Study_rodent_final = trimws(Study_rodent_final))]


# THERE"S STILL A RATTUS HERE. WHAT THE FUCK
attrib.mrg.sep[Rodent_attributed_final == "Rattus", ]$scientificName
attrib.mrg[scientificName %in% attrib.mrg.sep[Rodent_attributed_final == "Rattus", ]$scientificName,
           ]

rat_key[scientificName == "Aptenorallus calayanensis"]
# No predators in rat key.
#
#
# Wait on Arian for this.
#
#
attrib.mrg[scientificName == "Aptenorallus calayanensis"]
studies.recomb.sep[scientificName == "Aptenorallus calayanensis"]

attrib.mrg.sep <- attrib.mrg.sep[scientificName != "Aptenorallus calayanensis"]

studies.recomb.sep[Study_rodent_final == ""]
studies.recomb.sep[scientificName == "Puffinus newelli"]

studies.recomb.sep <- studies.recomb.sep[Study_rodent_final != ""]

# >>> Merge evidence back into attribution by rodent and bird species --------------------------------
#
unique(studies.recomb.sep$Study_rodent_final)
unique(attrib.mrg.sep$Rodent_attributed_final)

attrib.mrg.sep[Rodent_attributed_final %in% c("Rattus norvegicu"), 
               Rodent_attributed_final := "Rattus norvegicus"]
attrib.mrg.sep[Rodent_attributed_final %in% c("Rattus argentiventer (native)"), 
               Rodent_attributed_final := "Rattus argentiventer"]

#
attrib.mrg.sep[, key := paste(scientificName, Rodent_attributed_final, sep = "_")]
studies.recomb.sep[, key := paste(scientificName, Study_rodent_final, sep = "_")]

setdiff(studies.recomb.sep$key, attrib.mrg.sep$key)
attrib.mrg.sep[scientificName == "Eunymphicus cornutus", ]

studies.recomb.sep[scientificName == "Eunymphicus cornutus", ]

# MANUAL EDITS & EXCLUSIONS -------------------------------------------------------

studies.recomb.sep[scientificName == "Eunymphicus cornutus", ]
#
unique(studies.recomb.sep$Study_type)
studies.recomb.sep[scientificName == "Eunymphicus cornutus" &
                     grepl("Robinet", Article), 
                   Study_type := "Predation (artificial)"]

unique(studies.recomb.sep$Exclude_row)

studies.recomb.sep <- studies.recomb.sep[is.na(Exclude_row), ]
studies.recomb.sep[scientificName == "Eunymphicus cornutus", ]

unique(studies.recomb$Study_type)

# SAVE files separately ----------------------------------------------
studies.recomb.sep
sort(unique(studies.recomb.sep$Article))
studies.recomb.sep <- studies.recomb.sep[Article != "", ]
studies.recomb.sep$Study_ID
# there should be duplicates in Study_ID
studies.recomb.sep[duplicated(Study_ID), ]
unique(studies.recomb.sep$Study_type)
unique(studies.recomb.sep$Evidence_category)
studies.recomb.sep$Study_type <- NULL

fwrite(studies.recomb.sep, "data/Working_Databases/Studies.csv")


attrib.mrg.sep
fwrite(attrib.mrg.sep, "data/Working_Databases/Attribution.csv")

# OK so rodents were in study but weren't attributed.
# 
# # Hmmm.
# final_dat <- merge(attrib.mrg.sep,
#                    studies.recomb.sep[, !"scientificName"],
#                    by = "key",
#                    all.x = T,
#                    all.y = F)
# 
# final_dat[is.na(redlistCategory)]
# # should be 0 rows.
# 
# # OK. I guess we're good then?
# setdiff(attrib.mrg.sep$scientificName, final_dat$scientificName)
# 
# final_dat$key <- NULL

# fwrite(final_dat, "data/Working_Databases/Systematic_review_attribution_and_evidence.csv")


