#
#
#
# With great trepidation, let's take a look at this dataset.
#
#
#
# Prepare environment -----------------------------------------------------
# 
rm(list = ls())

library("groundhog")

date <- "2024-07-15"
ubuntu <- FALSE
if(ubuntu == TRUE){
  date <- "2023-04-15"
}
pcks <- c("data.table", "ggplot2", "tidyr", "readxl",
          "stringr", "dplyr", "metafor")
groundhog.library(pcks, date)

theme_lundy <- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

# Load data ---------------------------------------------------------------

meta <- read_excel("data/Raw/Meta-analysis-26-Sep.xlsx")
meta
setDT(meta)
names(meta)
setnames(meta, names(meta), gsub("-", "_", names(meta)))
setnames(meta, names(meta), gsub("%", "", names(meta)))

unique(meta$Predator_mean)
meta <- meta[Predator_mean != "no data"]

# *** Merge in Study Info?? -------------------------------------------------
unique(meta$Study_rodent)

# studies <- fread("data/Working_Databases/Studies.csv")
# studies
# studies$orig_study_ID
# meta$Study_ID
# 
# studies$Article
# meta$Article
# 
# unique(setdiff(meta$Article, studies$Article))
# #
# 
# "Breeding of Cassin's auklets Ptychoramphus aleuticus at Anacapa Island, California, after eradication of black rats Rattus rattus"
# # Can't find this one in studies:
# studies[grepl("whitworth", Article, ignore.case = T)]$Article
# #
# #
# studies[grepl("Breeding of Cassin", Article, ignore.case = T)]
# meta[grepl("WHITWORTH", Article, ignore.case = T)]$Study_ID
# meta[grepl("WHITWORTH", Article_secondary_same_data, ignore.case = T)]$Article_secondary_same_data
# 
# # Not sure what Study_ID links to...
# 
# #
# studies[grepl("zino", Article, ignore.case = T)]$Article
# studies[grepl("zino", Article_secondary_same_data, ignore.case = T)]$Article_secondary_same_data
# 
# #
# studies[grepl("Innes", Article, ignore.case = T)]$Article
# studies[grepl("Innes", Article_secondary_same_data, ignore.case = T)]$Article
# 
# #
# studies[grepl("Wills", Article, ignore.case = T)]$Article
# studies[grepl("Wills", Article_secondary_same_data, ignore.case = T)]$Article
# 
# #
# studies[grepl("Rayner", Article, ignore.case = T)]$Article
# studies[grepl("Rayner", Article_secondary_same_data, ignore.case = T)]$Article
# 
# #
# studies[grepl("VanderWerf", Article, ignore.case = T)]$Article
# studies[grepl("VanderWerf", Article_secondary_same_data, ignore.case = T)]$Article
# 
# #
# studies[grepl("Cruz", Article, ignore.case = T)]$Article
# studies[grepl("Cruz", Article_secondary_same_data, ignore.case = T)]$Article
# # Well we'll just have to wait for Arian on that.

# Divide by study type ----------------------------------------------------
meta
sort(unique(meta$Prey_units))
unique(meta$Predator_units)
unique(meta$Study_type)

meta[, Effect_type := fcase(
  Prey_units %in% c("Present", "Absent", "presence/absence", "Presence/absence"), 
          "Odds_ratio",
  Predator_units %in% c("Presence/absence", "Present", "Absent") & !Prey_units %in% c("Present", "Absent", "presence/absence",
                                                                                      "Presence/absence"),
           "SMD",
  !Predator_units %in% c("Presence/absence", "Present", "Absent") & !Prey_units %in% c("Present", "Absent", "presence/absence",
                                                                                       "Presence/absence"),
            "Correlation",
  default = NA
), ]

meta[is.na(Effect_type), ]

# Temporal SMD studies are actually interrupted time series...Which we could
# analyze as a before / after with SMD...But we have to drop the error if we do so. 
meta[Study_type == "Temporal" & 
       Effect_type == "SMD",
     Effect_type := "Before_after_interrupted_time_series"]


# CHECK WITH ARIAN:
meta[grepl("Imber, M.J., West, J.A. and Cooper, W.J., 2003. ", Article),
     `:=` (Effect_type = "Before_after_interrupted_time_series",
           Study_type = "Temporal")]

meta[Article == "Tabak, M.A., Poncet, S., Passfield, K., Goheen, J.R. and Martinez del Rio, C., 2016. The ghost of invasives past: rat eradication and the community composition and energy flow of island bird communities. Ecosphere, 7(8), p.e01442.",
     Effect_type := "SMD"]

# meta[Predator_units]
# >>> Format numeric columns ----------------------------------------------
meta$Predator_mean
unique(meta$Predator_mean)
#
meta[, Predator_mean := str_trim(Predator_mean)]
meta[is.na(as.numeric(Predator_mean)), ]$Predator_mean

# meta[Predator_mean == "0.?"]$Article
# meta[grepl("Amarasekare", Article)]
meta[Predator_mean == "0.?", Predator_mean := "0.1"]
meta[Predator_mean == "present", Predator_mean := "1"]
meta[, Predator_mean := as.numeric(Predator_mean)]
#
meta[is.na(as.numeric(Predator_error)), ]$Predator_error
meta[, Predator_error := str_trim(Predator_error)]
meta[, Predator_error := as.numeric(Predator_error)]

#
meta[is.na(as.numeric(Prey_mean)), ]$Prey_mean
meta[, Prey_mean := str_trim(Prey_mean)]
meta[, Prey_mean := gsub("%", "", Prey_mean)]
meta <- meta[Prey_mean != "?", ]
meta[, Prey_mean := as.numeric(Prey_mean)]

meta
#
meta[is.na(as.numeric(Prey_error)), ]$Prey_error
meta[, Prey_error := str_trim(Prey_error)]
meta[Prey_error == "add if relevant"]$Effect_type
# @@@ RECONSIDER THIS IF WE FIND INTERRUPTED TIME SERIES EFFECT SIZE------------------
# meta[, Prey_error := as.numeric(Prey_error)]
meta

class(meta$Sample_size_overall)

class(meta$Sample_size)


# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Standardized mean difference --------------------------------------------
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6998624/
# ^^^ rec to avoid pre - post comparisons
smd <- meta[Effect_type == "SMD"]

smd[Predator_mean == 0, Predator_PA := "Absent"]
smd[Predator_mean == 1, Predator_PA := "Present"]
smd$Predator_mean <- NULL

smd

unique(smd$Predator_comment)
View(smd[, .(Article, scientificName, Study_rodent,
             Study_type, Year, Site,
             Predator_PA, Predator_error,
             Prey_mean, Prey_error, Sample_size_overall,
             Prey_sample_size, Predator_sample_size
             )])

smd # ok. so we have a couple things here
# Tabak & Rayner et al. can be simply cast wide
# by rat presence/absence 

# Imber et al. can be summarized across years and cast wide
# too bad there wasn't an error bar per year.
# 

# Newton et al., did a before-after control-impact.
# Compare between I suppose? Error is already calculated...

# ---------------- Tabak, Rayner -------------------------------!
smd.1 <- smd[grepl("Tabak", Article) |
               grepl("Rayner", Article)]

View(smd.1[, .(Article, scientificName, Study_rodent,
             Study_type, Year, Site,
             Predator_PA, Predator_error,
             Prey_mean, Prey_error, Sample_size_overall,
             Prey_sample_size, Predator_sample_size
)])
smd.1$Predator_comment <- NA
smd.1.ready <- dcast(smd.1,
                     ... ~ Predator_PA,
                     value.var = c("Prey_mean", "Prey_error",
                                   "Sample_size_overall", "Prey_sample_size",
                                   "Predator_sample_size"))
smd.1.ready

smd <- smd[!Article %in% smd.1$Article]

names(smd.1.ready)

# ---------------- Imber -------------------------------!
# This actually looks like an Interrupted Time Series.
# smd.2 <- smd[grepl("Imber, M.J., West, J.A. and Cooper, W.J., 2003. ", Article), ]
# smd.2$Sample_size_overall
# View(smd.2[, .(Article, scientificName, Study_rodent,
#                Study_type, Year, Site,
#                Predator_PA, Predator_error,
#                Prey_mean, Prey_error, Sample_size_overall,
#                Prey_sample_size, Predator_sample_size
# )])
# smd.2[!is.na(Prey_error)]
# 
# names(smd.2)
# smd.2.sum <- smd.2[, .(Prey_mean = mean(Prey_mean),
#                        Prey_error = sd(Prey_mean),
#                        Prey_error_type = "SD",
#                        Sample_size_overall = uniqueN(Year),
#                        Sample_type = "Number of years"),
#                    by = .(assessmentId, internalTaxonId,
#                           Study_ID, scientificName,
#                           Common_name, redlistCategory, Synonyms_or_previous_lump,
#                           Rodent_attributed_IUCN, Article, Article_secondary_same_data,
#                           Study_type, Study_data_type_useable, Study_rodent,
#                           Study_rodent_species_Rattus_genus, Study_location, 
#                           Latitude, Longitude, Description, Site,
#                           Predator_units, Predator_comment, Prey_units,
#                           Prey_sample_size, Prey_comment,
#                           Source, Data_notes, Effect_type,
#                           Predator_PA
#                           )]
# smd.2.sum


# ---------------- Martin -------------------------------!
# Martin et al., need to be summarized across islands to produce mean and SE
smd.3 <- smd[grepl("Martin", Article)]
unique(smd.3$Article)

smd <- smd[!Article %in% smd.3$Article]

smd.3
View(smd.3[, .(Article, scientificName, Study_rodent,
               Study_type, Year, Site,
               Predator_PA, Predator_error,
               Prey_mean, Prey_error, Sample_size_overall,
               Prey_sample_size, Predator_sample_size)])

smd.3.sum <- smd.3[, .(Prey_mean = mean(Prey_mean),
                       Prey_error = sd(Prey_mean),
                       Prey_error_type = "SD",
                       Sample_size_overall = .N,
                       Sample_type = "Number of islands"),
                   by = .(assessmentId, internalTaxonId,
                          Study_ID, scientificName,
                          Common_name, redlistCategory, Synonyms_or_previous_lump,
                          Rodent_attributed_IUCN, Article, Article_secondary_same_data,
                          Study_type, Study_data_type_useable, Study_rodent,
                          Study_rodent_species_Rattus_genus, Study_location,
                          Latitude, Longitude, Description, Year,
                          Predator_units, Predator_error, Predator_sample_size,
                          #Predator_comment, 
                          Prey_units,
                          Prey_sample_size, Prey_comment,
                          Source, Data_notes, Effect_type,
                          Predator_PA
                          )]
smd.3.sum
View(smd.3.sum[, .(Article, scientificName, Study_rodent,
               Study_type,
               Predator_PA, Predator_error,
               Prey_mean, Prey_error, Sample_size_overall,
               Prey_sample_size, Predator_sample_size)])
smd.3.sum

# Now, cast wide:
smd.3.ready <- dcast(smd.3.sum,
                     ... ~ Predator_PA,
                     value.var = c("Prey_mean", "Prey_error", "Sample_size_overall",
                                   "Prey_sample_size",
                                   "Predator_sample_size"))
smd.3.ready
names(smd.1.ready)

# ---------------- Newton -------------------------------!

smd.4 <- smd[grepl("Newton", Article)]
smd <- smd[!Article %in% smd.4$Article]
unique(smd.4$Article)
smd.4

# So there's pre and post eradication adn then there's island with (before) compard to island without during same time period.
# do we do both comparisons? Or just the lateral between island comparison?
View(smd.4[, .(Article, scientificName, Study_rodent,
                   Study_type, Site, Year,
                   Predator_PA, Predator_error,
                   Prey_mean, Prey_error, Sample_size_overall,
                   Prey_sample_size, Predator_sample_size)])
#
smd.4.spatial <- smd.4[Year %in% c("2001-2002", "1993-2003"), ]
smd.4.spatial

#
smd.4.spatial$Predator_comment <- "Similar control data were collected on Santa Barbara Island from 1993 – 2003 and 2007 – 2009"
smd.4.ready <- dcast(smd.4.spatial[, !c("Year", "Site")],
                     ... ~ Predator_PA,
                     value.var = c("Prey_mean", "Prey_error", "Sample_size_overall",
                                   "Prey_sample_size",
                                    "Predator_sample_size"))
smd.4.ready[, `:=` (Prey_error_Absent = as.numeric(Prey_error_Absent),
                    Prey_error_Present = as.numeric(Prey_error_Present))]
smd.4.ready


nrow(smd)

# @@@ Do we just do the spatial comparison?? ------------------------------

# ------------- Rbind ----------------------!
setdiff(names(smd.1.ready), names(smd.3.ready))
setdiff(names(smd.1.ready), names(smd.4.ready))
setdiff(names(smd.3.ready), names(smd.4.ready))

smd.final <- rbind(smd.1.ready, smd.3.ready, smd.4.ready,
                   fill = TRUE)
smd.final
smd.final[, `:=` (Predator_sample_size_Absent = NULL, 
                  Predator_sample_size_Present = NULL)]


smd.final$Comparison_notes <- "Comparisons between sites with and without rats."

smd.final[grepl("Newton", Article), 
          Comparison_notes := "Comparisons between sites with and without rats. Temporal before/after on Anacapa excluded for now"]

smd.final
setnames(smd.final, names(smd.final), gsub("_Absent", "_Rats_Absent", names(smd.final)))
setnames(smd.final, names(smd.final), gsub("_Present", "_Rats_Present", names(smd.final)))

smd.final <- smd.final[, !c("Predator_HPD_Credible_Intervals_Lower_95",
                            "Predator_HPD_Credible_Intervals_Upper_95")]

fwrite(smd.final, "data/Temp/SMD meta analysis for manual editing.csv")

# Odds ratio ------------------------------------------------------------
# https://www.metafor-project.org/doku.php/tips:assembling_data_or
unique(meta$Effect_type)
meta[Effect_type == "Odds_ratio"]

odds <- meta[Effect_type == "Odds_ratio"]
odds

odds
unique(odds$Predator_mean)
odds[Predator_mean == 0, Predator_PA := "Absent"]
odds[Predator_mean == 1, Predator_PA := "Present"]
odds$Predator_mean <- NULL

length(unique(odds$Article))
odds
# I think we just cast this wide.
unique(odds$Article)

View(odds[Article == "Seitre, J., 1992. Causes of land-bird extinctions in French Polynesia. Oryx, 26(4), pp.215-222.", 
           .(Article, scientificName, Study_rodent,
               Study_type, Site, Year,
               Predator_PA, Predator_error,
               Prey_mean, Prey_error, Sample_size_overall,
               Prey_sample_size, Predator_sample_size)])

#
unique(odds$Prey_mean)
unique(odds$Prey_error)
odds

odds[Data_notes == 'I regareded "abundant" and "rare" as "present" since there is no quantifiable definition for these terms',
     Data_notes := NA]

# I think has to be summarized with a sum

odds.wide <- dcast(odds[, !c("Predator_error", "Prey_error", "Prey_comment",
                             "Predator_comment", "Prey_units", "Predator_units",
                             "Site")],
                   ... ~ Predator_PA,
                   value.var = "Prey_mean",
                   fun.aggregate = sum)
odds.wide
odds.wide[is.na(Absent)]
View(odds.wide)

odds.wide <- odds.wide[, !c("Predator_HPD_Credible_Intervals_Lower_95", "Predator_HPD_Credible_Intervals_Upper_95")]
odds.wide

setnames(odds.wide, c("Absent", "Present"), c("Rats_Absent", "Rats_Present"))
odds.wide$Prey_units <- "Number of islands where prey is present"

fwrite(odds.wide, "data/Temp/Odds ratio for manual checking.csv")


# Correlation ------------------------------------------------------------
corr <- meta[Effect_type == "Correlation", ]
corr


View(corr[, .(Article, Study_type, Site, Year, Predator_mean, Predator_error,
              Prey_mean, Prey_error, Predator_units, Predator_comment, Prey_units)])

# I think these are all good.
fwrite(corr, "data/Temp/Correlation meta analysis for manual editing.csv")

# Interrupted time series (e.g. before-after...)  ------------------------------------------------
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7607479/

# First, let's get that Anacapa before/after data...
anacapa <- smd.4[Site == "Anacapa Island", ]
anacapa[, Study_type := "Temporal"]
anacapa[, Effect_type := "Before-after SMD"]

anacapa[, .(Article, scientificName, Study_rodent,
                               Study_type, Site, Year,
                               Predator_PA, Predator_error,
                               Prey_mean, Prey_error, Sample_size_overall,
            Sample_type,
                               Prey_sample_size, Predator_sample_size)]
unique(anacapa$Sample_size_overall)
#
anacapa[Year == "2001-2002", Number_of_years := 1]
anacapa[Year == "2003-2010", Number_of_years := 7]

anacapa[, Sample_size_overall := as.character(Sample_size_overall)]
anacapa[, Sample_size_overall := "10 sea caves"]
# anacapa[Year == "2003-2010", Sample_size_overall := "7 years, 10 sea caves"]

anacapa

anacapa.cst <- dcast(anacapa[, !c("Year"), with = F],
                     ... ~ Predator_PA,
                     value.var = c("Sample_size_overall", "Prey_mean", "Prey_error",
                                   "Prey_sample_size", "Number_of_years"))

anacapa.cst

# ------------------ Now the others... ---------------------------!
# Going to do this one by one...
unique(meta$Effect_type)

temporals <- meta[Effect_type == "Before_after_interrupted_time_series"]
temporals[, Effect_type := "Before-after SMD"]
temporals

unique(temporals$Study_ID)
unique(temporals$Predator_mean)

temporals[Predator_mean == 0, Predator_PA := "Absent"]
temporals[Predator_mean == 1, Predator_PA := "Present"]
temporals$Predator_mean <- NULL

temporals.1 <- temporals[Study_ID == "study_11",]

temporals.1[, .(Article, scientificName, Study_rodent,
                                    Study_type, Site, Year,
                                    Predator_error,
                                    Prey_mean, Prey_error, Sample_size_overall,
                                    Sample_type, Predator_PA, Predator_error,
                                    Prey_sample_size, Predator_sample_size)]

# Are we summing total number of nests??? I guess so...Weird...
temporals.1.sum <- temporals.1[, .(Prey_mean = mean(Prey_mean),
                                   Sample_size_overall = sum(Sample_size_overall),
                                   Number_of_years = uniqueN(Year)),
                               by = .(assessmentId, internalTaxonId,
                                      Study_ID, scientificName,
                                      Common_name, redlistCategory, Synonyms_or_previous_lump,
                                      Rodent_attributed_IUCN, Article, Article_secondary_same_data,
                                      Study_type, Study_data_type_useable, Study_rodent,
                                      Study_rodent_species_Rattus_genus, Study_location,
                                      Latitude, Longitude, Description, #Year,
                                      Predator_units, Predator_error, Predator_sample_size,
                                      #Predator_comment, 
                                      Prey_units,
                                      Prey_sample_size, Prey_comment,
                                      Source, Data_notes, Effect_type,
                                      Predator_PA
                               )]

# -------------- BIND EM UP! -----------------------------------------!
anacapa.cst
