
rm(list = ls())

library("data.table")
library("ggplot2")
library("tidyr")
library("readxl")
library("stringr")
library("dplyr")
library("metafor")

# From now on any manual changes should be done to the divided datasets
get_cor <- function(x, y){
  return(cor.test(y, x)$estimate)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------

# Calculate effect sizes --------------------------------------------------

# >>> Calculate SMD ----------------------------------------------
smd <- read_excel("data/Working_Databases/SMD-14Dec.xlsx")
smd
# View(smd)
setDT(smd)

# Convert SE to SD:
unique(smd$Prey_error_type)

smd[Prey_error_type == "SE", `:=` (Prey_error_Rats_Absent = Prey_error_Rats_Absent * sqrt(Sample_size_overall_Rats_Absent),
                                   Prey_error_Rats_Present = Prey_error_Rats_Present * sqrt(Sample_size_overall_Rats_Present))]

# Some of these are likely hyper overdispersed.
# Check:
smd[, cv_rats_absent := Prey_error_Rats_Absent / ifelse(Prey_mean_Rats_Absent == 0, 0.001, Prey_mean_Rats_Absent)]
smd[, cv_rats_present := Prey_error_Rats_Present / ifelse(Prey_mean_Rats_Present == 0, 0.001, Prey_mean_Rats_Present)]
smd
# Those are actually ok.

names(smd)

smd <- escalc("SMDH", # H for heteroscestic variance between populations
              m2i = Prey_mean_Rats_Absent,
              m1i = Prey_mean_Rats_Present,
              sd2i = Prey_error_Rats_Absent,
              sd1i = Prey_error_Rats_Present,
              n2i = Sample_size_overall_Rats_Absent,
              n1i = Sample_size_overall_Rats_Present,
              data = smd,
              add = 0.01,
              to = "if0all")
setDT(smd)
smd[, .(Study_ID, Prey_mean_Rats_Absent, Prey_mean_Rats_Present,
        yi, vi)]


ggplot(data = smd, aes(x = 1, y = yi))+
  geom_boxplot()+
  geom_jitter(aes(size = 1/vi))
setnames(smd, c("yi", "vi"), c("effect_size", "sampling_variance"))

smd[, effect_size_type := "SMD"]
smd

# Convert study_1 long term into an odds ratio.
smd.OR <- escalc("D2ORN",
              m2i = Prey_mean_Rats_Absent,
              m1i = Prey_mean_Rats_Present,
              sd2i = Prey_error_Rats_Absent,
              sd1i = Prey_error_Rats_Present,
              n2i = Sample_size_overall_Rats_Absent,
              n1i = Sample_size_overall_Rats_Present,
              data = smd[Effect_size_conversion == "Convert to Odds Ratio for long term analysis", ],
              add = 0.01,
              to = "if0all")
setDT(smd.OR)
smd.OR
smd.OR[, analysis_group := "Long-term abundance"]

setDT(smd.OR)
smd.OR[, `:=` (effect_size = yi, sampling_variance = vi,
               effect_size_type = "SMD converted to OR")]
smd.OR[, `:=` (yi = NULL, vi = NULL)]


smd <- smd[is.na(Effect_size_conversion), ]
smd[, analysis_group := "Before-after eradication abundance"]
smd[, effect_size_type := "SMD"]

smd.final <- rbind(smd,
                   smd.OR, fill = T)

smd.final

unique(smd.final$Effect_type)
unique(smd.final$effect_size_type)

unique(smd.final$analysis_group)

# >>> Odds ratio --------------------------------------------------
odds <- read_excel("data/Working_Databases/Odds Ratio-17Dec.xlsx")
odds

odds <- escalc("OR",
               ai = Rat_Absent.Prey_Neg,
               bi = Rat_Absent.Prey_Pos,
               ci = Rat_Present.Prey_Neg,
               di = Rat_Present.Prey_Pos,
               data = odds)
setDT(odds)
odds

odds[, .(Rat_Absent.Prey_Neg, Rat_Absent.Prey_Pos,
         Rat_Present.Prey_Neg, Rat_Present.Prey_Pos,
         yi, Hypothesis_supported_when)]
setnames(odds, c("yi", "vi"), c("effect_size", "sampling_variance"))
odds[, effect_size_type := "OR"]

# I think this is correct....
odds[is.na(Long_term), Long_term := 0]
odds

smd

unique(odds$Question)
unique(smd.final$analysis_group)
odds[Question == "Long term rat and bird presence vs absence",
     analysis_group := "Long-term abundance"]
odds[Question == "Short term bird reproduction before vs after rat eradication",
     analysis_group := "Before-after eradication reproduction"]

odds$Effect_type

unique(odds$analysis_group)
unique(smd.final$analysis_group)

# >>> Correlation ---------------------------------------------------------

corr <- read_excel("data/Working_Databases/Correlation-18Dec.xlsx")
corr
setDT(corr)

#' 
names(corr)
corr[is.na(as.numeric(Predator_mean)), .(Study_ID, Predator_mean)]

corr[, `:=` (Predator_mean = as.numeric(Predator_mean),
             Prey_mean = as.numeric(Prey_mean))]

corr[Article == "Radley, P.M., Davis, R.A. and Doherty, T.S., 2021. Impacts of invasive rats and tourism on a threatened island bird: the Palau Micronesian Scrubfowl. Bird Conservation International, 31(2), pp.206-218."]
unique(corr$Study_type)
corr[, Long_term := 0]

# Ugh....This is such a pain in the ass to figure out.
corr[Study_type == "Spatiotemporal"]

unique(corr[Study_type == "Spatiotemporal"]$Study_ID)

# corr$correlation_id <- NULL
corr[Study_type == "Spatiotemporal", 
     correlation_id := paste(Study_ID, .GRP),
      by = .(scientificName, Article, Study_ID, Study_rodent,
             Latitude, Longitude, Site, # Keep site in for Spatiotemporal
             Predator_units, Prey_units, Abundance_reproduction)]
#
corr[Study_type == "Temporal"]
corr[Study_type == "Temporal", 
     correlation_id :=  paste(Study_ID, .GRP),
     by = .(scientificName, Article, Study_ID, Study_rodent,
            Study_location, Latitude, Longitude, Site,
            Predator_units, Prey_units, Abundance_reproduction)]
corr[Study_type == "Temporal", .(n = .N), by = .(Study_type, correlation_id)]
#

#' [paste site names together and make sure same number of rows]
corr[Study_type == "Spatial"]
corr[Study_type == "Spatial"]
corr[Study_type == "Spatial", 
     `:=` (correlation_id =  paste(Study_ID, .GRP),
           Site = paste(unique(Site), collapse = "; "),
           Study_location = paste(unique(Study_location), collapse = "; "),
           Latitude = mean(Latitude),
           Longitude = mean(Longitude)),
     by = .(scientificName, Article, Study_ID, Study_rodent,
            #Study_location, Latitude, Longitude, Site,
            Predator_units, Prey_units, Abundance_reproduction)]
corr[, .(n = .N), by = .(Study_type, correlation_id)]

#
corr[, n := .N, by = .(correlation_id)]
range(corr$n)

# corr[n > 4, .(r = get_cor(x = Predator_mean, y = Prey_mean)),
#      by=.(correlation_id)]

corr
corr[n < 4, ]
corr <- corr[n >= 4, ]
# 
# ids <- unique(corr$correlation_id)
# x <- ids[1]
# rs <- lapply(ids, FUN = function(x){
#   get_cor(x = corr[correlation_id == x, ]$Predator_mean,
#           y = corr[correlation_id == x, ]$Prey_mean)
# })
# 
# rs <- unlist(rs)
# one of these is not working and it's because of sample size...

corr <- corr[, .(r = get_cor(x = Predator_mean, y = Prey_mean),
                 Latitude = mean(Latitude), 
                 Longitude = mean(Longitude)),
             by = .(Study_ID,
                    scientificName, Common_name, redlistCategory,
                    Synonyms_or_previous_lump,
                    Rodent_attributed_IUCN, Article,
                    Article_secondary_same_data,
                    Study_type, Study_rodent,
                    Study_location, Site,
                    Predator_units, Prey_units, Abundance_reproduction,
                    Hypothesis_supported_when, Long_term,
                    correlation_id, n)]
corr[duplicated(correlation_id)]
corr

corr[is.na(r), ]

corr[Study_ID == "study_28-29", ]


corr <- escalc("ZCOR",
               ri = r,
               ni = n,
               data = corr)
corr
corr <- corr[!is.na(vi), ]
setDT(corr)
setnames(corr, c("yi", "vi"), c("effect_size", "sampling_variance"))
corr[, effect_size_type := "ZCOR"]

unique(corr$Hypothesis_supported_when)

sort(unique(corr$Study_ID))

corr$Long_term
corr$Long_term <- 0

corr$Effect_type <- "ZCOR"
corr$effect_size_type <- "ZCOR"

#
unique(corr$Abundance)
corr[Abundance_reproduction == "Abundance",
     analysis_group := "Short-term abundance"]
corr[Abundance_reproduction == "Reproduction",
     analysis_group := "Short-term reproduction"]

corr[analysis_group == "Short-term reproduction"]


# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Bind together effect sizes ----------------------------------------------

setDT(smd)
setDT(odds)
setDT(corr)

meta.final <- data.table::rbindlist(list(smd.final, odds, corr), 
                                  use.names = TRUE,
                                  fill = T)

meta.final$effect_size_type


meta.final[, effect_size_id := seq(1:.N)]


unique(meta.final$Effect_type)
meta.final

setnames(meta.final, "Effect_type", "Original_study_design_effect_type")

meta.final[, .(n = .N, refs = uniqueN(Article), effect_sizes = paste(unique(effect_size_type), collapse = "; ")),
           by = .(analysis_group)]

fwrite(meta.final, "builds/meta_analysis/compiled_ready_to_analyze.csv")
meta.final

