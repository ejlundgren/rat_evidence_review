
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

# From now on any manual changes should be done to the divided datasets
get_cor <- function(x, y){
  return(cor.test(y, x)$estimate)
}

# Load original meta-analysis:
meta <- read_excel("data/Raw/Meta-analysis-8-Dec.xlsx")
setDT(meta)
meta$Study_ID


# TESTING -----------------------------------------------------------------
# according to https://onlinelibrary.wiley.com/doi/full/10.4073/cmpn.2016.3
# Odds ratio goes from 0 to infinity with 1 being no effect.
# Is that true!?!?!?!
# If so this should be an OR of 1:
escalc("OR",
       ai = 5, #Rat_Absent.Prey_Neg,
       bi = 5, #Rat_Absent.Prey_Pos,
       ci = 5, #Rat_Present.Prey_Neg,
       di = 5 #Rat_Present.Prey_Pos,
       )
# log odds of 0
exp(0)
# which is an odds ratio of 1


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------

# Calculate effect sizes --------------------------------------------------

# >>> Calculate SMD ----------------------------------------------
smd <- read_excel("data/Working_Databases/SMD-14Dec.xlsx")
smd
# View(smd)
setDT(smd)

# this one is 0 to 0:
unique(meta[Study_ID == "study_53"]$Article)
meta[Study_ID == "study_53" & scientificName == "Ptychoramphus aleuticus" &
       Site == "Sea Caves"]

x <- "study_1"
smd[Study_ID == x]
unique(smd$Study_ID)

# Convert SE to SD:
unique(smd$Prey_error_type)

smd[Prey_error_type == "SE", `:=` (Prey_error_Rats_Absent = Prey_error_Rats_Absent * sqrt(Sample_size_overall_Rats_Absent),
                                   Prey_error_Rats_Present = Prey_error_Rats_Present * sqrt(Sample_size_overall_Rats_Present))]



ggplot()+
  geom_histogram(data = smd, aes(x = Prey_error_Rats_Absent),
                 fill = "red", alpha = .5)+
  geom_histogram(data = smd, aes(x = Prey_error_Rats_Present),
                 fill = "blue", alpha = .5)

smd[Prey_error_Rats_Absent > 750, ]

meta[Study_ID %in% smd[Prey_error_Rats_Absent > 750, ]$Study_ID]
#
smd

# I guess that should be kept huh?
names(smd)

smd <- escalc("SMD", # H for heteroscestic variance between populations
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
smd[Study_ID == x]


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

# >>> Odds or risk ratio --------------------------------------------------
odds <- read_excel("data/Working_Databases/Odds Ratio-17Dec.xlsx")
odds

# Risk ratio or odds ratio?
# Risk is number killed / all nests
# odds is number killed / number survived
# But it looks like metafor takes the same inputs

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

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------


# DEPRECATED: -------------------------------------------------------------


# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------

# Add categorical analysis_group --------------------------------------
#' 
#' meta.final[, .(Long_term, effect_size_type, Abundance_reproduction)]
#' 
#' meta.final[Long_term == 1, .(effect_size_type, Abundance_reproduction)]
#' 
#' 
#' meta.final[Long_term == 0 & Abundance_reproduction == "Presence-absence", .(effect_size_type, Abundance_reproduction)]
#' 
#' meta.final[Long_term == 0 & Abundance_reproduction == "Abundance", .(effect_size_type, Abundance_reproduction)]
#' # OK...so these should be interconverted huh?
#' meta.final[Long_term == 0 & Abundance_reproduction == "Reproduction", .(effect_size_type, Abundance_reproduction)]
#' # Ah but SMD's are eradication studies...
#' meta.final[effect_size_type == "SMD"]
#' # Confusing shit.
#' #
#' # We want the following models: Abundance, Reproduction, Presence-absence, and Eradication abundance.
#' meta.final[grepl("eradic", Description)]
#' meta.final[, analysis_group := ifelse(grepl("eradic", Description),
#'                                       "Before-after eradication abundance", NA)]
#' 
#' meta.final[Long_term == 1 & Abundance_reproduction %in% c("Presence-absence", "Abundance"), ]$analysis_group
#' meta.final[Long_term == 1 & Abundance_reproduction %in% c("Presence-absence", "Abundance"), 
#'            analysis_group := "Long-term abundance"]
#' 
#' meta.final[Long_term == 0 & Abundance_reproduction == "Abundance" &
#'              !grepl("eradic", Description),]$effect_size_type
#' meta.final[Long_term == 0 & Abundance_reproduction == "Abundance" &
#'              !grepl("eradic", Description) &
#'              effect_size_type == "SMD",]$Description
#' 
#' meta.final[Long_term == 0 & Abundance_reproduction == "Abundance" &
#'              !grepl("eradic", Description), analysis_group := "Short-term abundance"]
#' 
#' meta.final[Long_term == 0 & Abundance_reproduction == "Reproduction" &
#'              is.na(analysis_group), analysis_group := "Short-term reproduction"]
#' 
#' #' [Look at question column!!!]
#' 
#' #

#' 
#' 
#' meta.final[effect_size_type == "SMD" & Long_term == 1, ]
#' smd.final
#' 
#' 
#' meta.final[Study_ID == "study_1"]
#' # ok good. already converted.
#' 
#' meta.final[is.na(analysis_group), .(Study_ID, Long_term, Abundance_reproduction, Effect_type, effect_size_type)]

# >>> Convert to shared effect size ---------------------------------------
# From Introduction to Meta‐Analysis - 2009 - Borenstein
# In Resources/References/
# 
# meta.sub <- meta.final[analysis_group == "Short-term reproduction"]
# 
# unique(meta.sub$effect_size_type)
# 
# # Looks like need to convert these to SMD.
# meta.sub[effect_size_type == "OR", smd := effect_size * sqrt(3) / pi]
# 
# meta.sub[effect_size_type == "ZCOR", smd := 2 * r / sqrt(1 - r^2)]
# # meta.sub[effect_size_type == "ZCOR", v := 1 / (n - 3)]

# 
# # Now variance:
# meta.sub[, .(min_v = min(sampling_variance), max_v = max(sampling_variance)), by = .(effect_size_type)]
# # these are roughly equivalent.
# 
# # Calculate variance with 1 / n-3 just like with zcor for consistency.
# # meta.sub[effect_size_type == "ZCOR", v := 1 / (n - 3)]
# # range(meta.sub[effect_size_type == "ZCOR",]$n, na.rm = T)
# # 
# # meta.sub[effect_size_type == "OR", .(Rat_Absent.Prey_Neg, Rat_Absent.Prey_Pos, Rat_Present.Prey_Pos, Rat_Present.Prey_Neg)]
# # 
# # meta.sub[effect_size_type == "OR", n := Rat_Absent.Prey_Neg + Rat_Absent.Prey_Pos + Rat_Present.Prey_Pos + Rat_Present.Prey_Neg]
# # meta.sub[effect_size_type == "OR", ]$n
# # 
# # meta.sub[effect_size_type == "OR", v := 1 / (n - 3)]
# # I think these will be too grossly different.
# 
# # Instead:
# 
# #
# meta.sub[effect_size_type == "OR", v := sampling_variance * 3 / pi^2]
# 
# # Also, let's calculate sampling variance based on Borenstein formula instad of 
# # using the zcor sampling-variance
# meta.sub[effect_size_type == "ZCOR", sampling_variance := (1 - r^2)^2 / (n - 1)]
# meta.sub[effect_size_type == "ZCOR",]$sampling_variance
# 
# meta.sub[effect_size_type == "ZCOR", .(effect_size, r)]
# 
# meta.sub[effect_size_type == "ZCOR", v := (4 * sampling_variance) / ((1 - r^2)^3)]
# 
# meta.sub[, .(min_v = min(v), max_v = max(v)), by = .(effect_size_type)]
# 
# range(meta.final$sampling_variance, na.rm = T)
# # ok...
# 
# # Hmmmmm. V should never be greater than 1 should it??
# meta.sub[v > 1, .(r, n, sampling_variance)]
# 
# # (4 * .5) / (1-0.4756945^2)^3
# 
# meta.sub[, effect_size_type := paste(effect_size_type, "converted to SMD")]
# meta.sub[, `:=` (sampling_variance = v,
#                  effect_size = smd)]
# 
# meta.sub[, `:=` (v = NULL,
#                  smd = NULL)]
# 
# meta.final <- rbind(meta.final[!effect_size_id %in% meta.sub$effect_size_id],
#                     meta.sub)
# 
# meta.final
# 
# meta.final <- meta.final[!is.na(effect_size)]
# 
# unique(meta.final$Original_study_design_effect_type)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Save --------------------------------------------------------------------
# unique(meta.final$effect_size_type)
#

# fwrite(meta.final, "builds/meta_analysis/compiled_ready_to_analyze.csv")
