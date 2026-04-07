#
#
#
# With great trepidation, let's take a look at this dataset.
#
#
# DO NOT USE THIS SCRIPT AGAIN. THIS WAS TO CLEAN ORIGINAL DATA
# Prepare environment -----------------------------------------------------
# 

rerun <- F
if(rerun){
  
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
  
  meta <- read_excel("data/Raw/Meta-analysis-5-Dec.xlsx")
  meta
  
  setDT(meta)
  names(meta)
  
  setnames(meta, names(meta), gsub("-", "_", names(meta)))
  setnames(meta, names(meta), gsub("%", "", names(meta)))
  
  unique(meta$Predator_mean)
  meta <- meta[Predator_mean != "no data"]
  
  meta$Abundance_reproduction
  
  meta[Study_ID == "study_11"]
  
  # Test odds ratio ---------------------------------------------------------
  ?escalc
  
  escalc(measure = "OR",
         ai = 1, bi = 3, ci = 3, di = 1)
  
  escalc(measure = "OR",
         ai = 10, bi = 30, ci = 30, di = 10)
  # OK, so yi is identical but vi is significantly smaller, with higher sample size...
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------
  
  # Odds ratio studies ----------------------------------------------------
  # Unlike previous attempt, I think most of our studies are actually odds ratios...#
  meta
  sort(unique(meta$Prey_units))
  unique(meta$Predator_units)
  unique(meta$Study_type)
  unique(meta$Effect_type)
  
  # >>> Odds ratio with binary prey response ----------------------------------------------------------
  sort(unique(meta$Prey_units))
  
  unique(meta$Effect_type)
  odds.binary <- meta[Effect_type %in% "Binary Odds Ratio", ]
  odds.binary
  
  unique(odds.binary$Article)
  
  # View(odds.binary[, .(Article, Study_type, scientificName,
  # Site, Year, Predator_mean, Predator_error,
  # Prey_mean, Prey_error, Prey_sample_size,
  # Sample_size_overall,
  # Sample_type)])
  # So this needs to be cast into 4 columns.
  
  odds.binary[, Predator_PA := ifelse(Predator_mean == 0,
                                      "Rat_Absent", "Rat_Present")]
  
  unique(odds.binary$Prey_mean)
  odds.binary[, Prey_PA := ifelse(Prey_mean == 0,
                                  "Prey_Absent", "Prey_Present")]
  unique(odds.binary[, .(Prey_mean, Prey_PA)])
  unique(odds.binary[, .(Predator_mean, Predator_PA)])
  
  odds.binary[, to_cast := paste(Predator_PA, Prey_PA, sep = ".")]
  
  odds.binary
  
  unique(odds.binary$Study_type)
  unique(odds.binary$Sample_size_overall)
  unique(odds.binary$Data_notes)
  #
  
  
  # View(odds.binary[, .(Study_ID, scientificName, Study_rodent,
  # Article, Study_type, Latitude, Longitude,
  # Site,
  # Prey_mean, Predator_PA, Data_notes)])
  setorder(odds.binary, Article, Study_location)
  # View(odds.binary[, .(Site, Study_location, Predator_PA, Prey_mean)])
  
  #
  
  odds.binary[, Prey_mean := as.numeric(Prey_mean)]
  names(odds.binary)
  unique(odds.binary$Description)
  unique(odds.binary$Prey_comment)
  unique(odds.binary$Predator_comment)
  unique(odds.binary$Data_notes)
  
  odds.binary[, `:=` (Latitude = mean(Latitude), 
                      Longitude = mean(Longitude)),
              by = .(Article, scientificName, Study_rodent)]
  
  
  # setdiff(names(odds.cont.1.final), names(odds.binary.final))
  odds.binary$Predator_error
  odds.binary$Predator_units
  odds.binary$Prey_sample_size
  odds.binary$Prey_comment
  unique(odds.binary[Prey_mean == 1 & Predator_mean == 1, ]$to_cast)
  unique(odds.binary[Prey_mean == 0 & Predator_mean == 1, ]$to_cast)
  unique(odds.binary[Prey_mean == 0 & Predator_mean == 0, ]$to_cast)
  unique(odds.binary[Prey_mean == 1 & Predator_mean == 0, ]$to_cast)
  
  odds.binary.sum <- odds.binary[, .(val = .N,
                                     Prey_units = "Presence/Absence"),
                                 by = .(assessmentId, internalTaxonId,
                                        Study_ID, scientificName, Common_name,
                                        redlistCategory, Synonyms_or_previous_lump, 
                                        Study_rodent,Rodent_attributed_IUCN,
                                        Article, Article_secondary_same_data, 
                                        Study_type,
                                        Study_data_type_useable,
                                        Study_rodent_species_Rattus_genus,
                                        #Study_location, 
                                        Latitude, Longitude,
                                        #Description,
                                        #Source, 
                                        Predator_error,
                                        Effect_type, to_cast,
                                        Abundance_reproduction,
                                        Hypothesis_supported_when#, #Prey_comment, Predator_comment,
                                 )]
  odds.binary.sum
  
  
  odds.binary.final <- dcast(odds.binary.sum, 
                             ... ~ to_cast,
                             value.var = "val",
                             fill = 0)
  odds.binary.final
  
  odds.binary.final[, .(scientificName, Study_rodent,
                        Latitude, Longitude, 
                        Rat_Absent.Prey_Absent, Rat_Absent.Prey_Present,
                        Rat_Present.Prey_Absent, Rat_Present.Prey_Present)]
  
  odds.binary[scientificName == "Puffinus yelkouan", .(to_cast, Prey_PA,
                                                       Predator_PA,
                                                       Prey_mean)]
  odds.binary[scientificName == "Puffinus yelkouan" &
                Prey_mean == 1, .(to_cast, Prey_PA,Prey_mean)]
  
  # Test:
  escalc(data = odds.binary.final, 
         measure = "OR",
         ai = Rat_Present.Prey_Present,
         bi = Rat_Present.Prey_Absent,
         ci = Rat_Absent.Prey_Present,
         di = Rat_Absent.Prey_Absent)
  
  
  odds.binary.final[, Bird_Response := "Presence/absence"]
  
  setnames(odds.binary.final, 
           c("Rat_Present.Prey_Present", "Rat_Present.Prey_Absent",
             "Rat_Absent.Prey_Present", "Rat_Absent.Prey_Absent"),
           c("Rat_Present.Prey_Pos", "Rat_Present.Prey_Neg",
             "Rat_Absent.Prey_Pos", "Rat_Absent.Prey_Neg"))
  
  
  odds.binary.final
  
  odds.binary.final[grepl("ultramarine", Common_name, ignore.case = T), ]
  
  # >>> Odds ratio with continuous prey response ----------------------------------------------------------
  unique(meta$Effect_type)
  
  odds.cont <- meta[Effect_type == "Continuous Odds Ratio" & !Article %in% odds.binary.final$Article, ]
  
  unique(odds.cont$Article)
  unique(odds.cont$Prey_units)
  unique(odds.cont$Prey_sample_size)
  # Gah but this approach doesn't capture effort doesn't it???? FUCK. GAH.
  
  # Gonna do these manually, one study at a time...
  
  # ---------------- Pierce 2002 -----------------------------------!
  odds.cont.1 <- odds.cont[Article == "Pierce, R.J., 2002. Kiore (Rattus exulans) impact on breeding success of Pycroft's petrels and little shearwaters (p. 24). Wellington: Department of Conservation."]
  odds.cont.1
  odds.cont.1[, Predator_PA := ifelse(Predator_mean == 0,
                                      "Rat_Absent", "Rat_Present")]
  odds.cont.1[, Prey_mean := as.numeric(Prey_mean)]
  
  odds.cont.1$Sample_size_overall
  
  odds.cont.1$Prey_units
  odds.cont.1$Prey_comment
  odds.cont.1$Data_notes
  odds.cont.1$Predator_comment
  
  odds.cont.1[, Number_nests_successful := round(Prey_mean * as.numeric(Sample_size_overall))]
  
  odds.cont.1[, Number_nests_failed := as.numeric(Sample_size_overall) - Number_nests_successful]
  
  odds.cont.1[, .(Number_nests_failed, Number_nests_successful, Sample_size_overall)]
  
  odds.cont.1.final <- dcast(odds.cont.1[, !c("Predator_mean", "Prey_mean", 
                                              "Year", "Sample_size_overall",
                                              "Predator_comment"), 
                                         with = F],
                             ... ~ Predator_PA,
                             value.var = c("Number_nests_successful",
                                           "Number_nests_failed"),
                             fun.aggregate = sum)
  odds.cont.1.final
  
  setnames(odds.cont.1.final, 
           c("Number_nests_successful_Rat_Present", "Number_nests_failed_Rat_Present",
             "Number_nests_successful_Rat_Absent", "Number_nests_failed_Rat_Absent"),
           c("Rat_Present.Prey_Pos", "Rat_Present.Prey_Neg",
             "Rat_Absent.Prey_Pos", "Rat_Absent.Prey_Neg"))
  
  odds.cont.1.final
  
  # ---------------------- Imber ------------------------------------!
  
  odds.cont.2 <- odds.cont[Article == "Imber, M.J., West, J.A. and Cooper, W.J., 2003. Cook's petrel (Pterodroma cookii): historic distribution, breeding biology and effects of predators. Notornis, 50(4), pp.221-230.", ]
  odds.cont.2[, .(Year, Site)]
  
  # We need total number of monitored nests?
  odds.cont.2[, Predator_PA := ifelse(Predator_mean == 0,
                                      "Rat_Absent", "Rat_Present")]
  odds.cont.2[, Prey_mean := as.numeric(Prey_mean)]
  
  odds.cont.2$Sample_size_overall
  odds.cont.2$Prey_mean
  odds.cont.2$Prey_units
  
  odds.cont.2$Prey_units
  odds.cont.2$Prey_comment
  odds.cont.2$Data_notes
  odds.cont.2$Predator_comment
  
  odds.cont.2[, Number_nests_successful := round(Prey_mean * as.numeric(Sample_size_overall))]
  
  odds.cont.2[, Number_nests_failed := as.numeric(Sample_size_overall) - Number_nests_successful]
  
  odds.cont.2[, .(Number_nests_failed, Number_nests_successful, Sample_size_overall)]
  
  odds.cont.2.final <- dcast(odds.cont.2[, !c("Predator_mean", "Prey_mean", 
                                              "Year", "Sample_size_overall"), 
                                         with = F],
                             ... ~ Predator_PA,
                             value.var = c("Number_nests_successful",
                                           "Number_nests_failed"),
                             fun.aggregate = sum)
  odds.cont.2.final
  
  setnames(odds.cont.2.final, 
           c("Number_nests_successful_Rat_Present", "Number_nests_failed_Rat_Present",
             "Number_nests_successful_Rat_Absent", "Number_nests_failed_Rat_Absent"),
           c("Rat_Present.Prey_Pos", "Rat_Present.Prey_Neg",
             "Rat_Absent.Prey_Pos", "Rat_Absent.Prey_Neg"))
  
  # ------------------------ Newton -------------------------!
  unique(odds.cont$Article)
  odds.cont.3 <- odds.cont[Article == "Newton, K.M., McKown, M., Wolf, C., Gellerman, H., Coonan, T., Richards, D., Harvey, A.L., Holmes, N., Howald, G., Faulkner, K. and Tershy, B.R., 2016. Response of native species 10 years after rat eradication on Anacapa Island, California. Journal of Fish and Wildlife Management, 7(1), pp.72-85.", ]
  odds.cont.3
  
  odds.cont.3[, Predator_PA := ifelse(Predator_mean == 0,
                                      "Rat_Absent", "Rat_Present")]
  odds.cont.3[, `:=` (Prey_mean = as.numeric(Prey_mean),
                      Sample_size_overall = as.numeric(Sample_size_overall))]
  # odds.cont.3[Prey_sample_size ]
  odds.cont.3
  odds.cont.3$Prey_units
  
  
  odds.cont.3$Prey_units
  odds.cont.3$Prey_comment
  odds.cont.3$Data_notes
  odds.cont.3$Predator_comment
  
  
  odds.cont.3[Prey_units %in% c("Percent nests occupied",
                                "Percent breeding success"), 
              Number_successful := round(Prey_mean * as.numeric(Sample_size_overall))]
  odds.cont.3[Prey_units %in% c("Percent nests occupied",
                                "Percent breeding success"), 
              Number_failed := as.numeric(Sample_size_overall) - Number_successful]
  # Now opposite for the % depredated
  odds.cont.3[Prey_units %in% "Percent eggs depredated",
              Number_failed := round(Prey_mean * Sample_size_overall)]
  odds.cont.3[Prey_units %in% "Percent eggs depredated",
              Number_successful := as.numeric(Sample_size_overall) - Number_failed]
  # View(odds.cont.3[, .(Prey_units, Prey_mean, Sample_size_overall, Number_successful,
  #                     Number_failed)]
  #
  odds.cont.3.final <- dcast(odds.cont.3[, !c("Predator_mean", "Prey_mean", 
                                              "Year", "Sample_size_overall",
                                              "Predator_comment", "Sampling_quote",
                                              "Sample_type", "Prey_error",
                                              "Site"), 
                                         with = F],
                             ... ~ Predator_PA,
                             value.var = c("Number_successful",
                                           "Number_failed"),
                             fun.aggregate = sum)
  odds.cont.3.final
  odds.cont.3.final[, Hypothesis_supported_when := "Negative"]
  odds.cont.3.final[, Abundance_reproduction := "Reproduction"]
  
  odds.cont.3.final
  setnames(odds.cont.3.final, 
           c("Number_successful_Rat_Present", "Number_failed_Rat_Present",
             "Number_successful_Rat_Absent", "Number_failed_Rat_Absent"),
           c("Rat_Present.Prey_Pos", "Rat_Present.Prey_Neg",
             "Rat_Absent.Prey_Pos", "Rat_Absent.Prey_Neg"))
  
  # ------------------- Rayner ------------------------------!
  unique(odds.cont$Article)
  
  odds.cont.4 <- odds.cont[Article == "Rayner, M.J., Hauber, M.E., Imber, M.J., Stamp, R.K. and Clout, M.N., 2007. Spatial heterogeneity of mesopredator release within an oceanic island system. Proceedings of the National Academy of Sciences, 104(52), pp.20862-20865.", ]
  odds.cont.4[, .(Year, Site)]
  
  # We need total number of monitored nests?
  odds.cont.4[, Predator_PA := ifelse(Predator_mean == 0,
                                      "Rat_Absent", "Rat_Present")]
  odds.cont.4[, Prey_mean := as.numeric(Prey_mean)]
  
  odds.cont.4$Sample_size_overall
  odds.cont.4$Prey_mean
  odds.cont.4$Prey_units
  
  odds.cont.4[, Number_nests_successful := round(Prey_mean * as.numeric(Sample_size_overall))]
  
  odds.cont.4[, Number_nests_failed := as.numeric(Sample_size_overall) - Number_nests_successful]
  
  odds.cont.4[, .(Prey_mean, Number_nests_failed, Number_nests_successful, Sample_size_overall)]
  
  odds.cont.4.final <- dcast(odds.cont.4[, !c("Predator_mean", "Prey_mean", 
                                              "Year", "Sample_size_overall",
                                              "Data_notes", "Prey_comment"), 
                                         with = F],
                             ... ~ Predator_PA,
                             value.var = c("Number_nests_successful",
                                           "Number_nests_failed"),
                             fun.aggregate = sum)
  odds.cont.4.final
  
  setnames(odds.cont.4.final, 
           c("Number_nests_successful_Rat_Present", "Number_nests_failed_Rat_Present",
             "Number_nests_successful_Rat_Absent", "Number_nests_failed_Rat_Absent"),
           c("Rat_Present.Prey_Pos", "Rat_Present.Prey_Neg",
             "Rat_Absent.Prey_Pos", "Rat_Absent.Prey_Neg"))
  odds.cont.4.final
  
  # >>> Combine odds ratio --------------------------------------------------
  odds.binary.final
  
  setdiff(names(odds.binary.final), names(odds.cont.1.final))
  setdiff(names(odds.cont.1.final), names(odds.binary.final))
  odds.binary.final[, Abundance_reproduction := "Presence-absence"]
  
  
  odds.final <- rbind(odds.binary.final[, !c("Bird_Response"), with = F],
                      odds.cont.1.final,
                      odds.cont.2.final,
                      odds.cont.3.final,
                      odds.cont.4.final,
                      fill = T)
  odds.final
  # Goddamn
  odds.final$Prey_comment
  odds.final$Sample_type
  odds.final$Data_notes
  
  odds.final <- odds.final[, !c("Predator_units", "Prey_sample_size",
                                "Prey_comment", "Sample_type",
                                "Predator_HPD_Credible_Intervals_Lower_95", 
                                "Predator_HPD_Credible_Intervals_Upper_95")]
  
  odds.final
  odds.final$Abundance_reproduction
  
  
  fwrite(odds.final, "data/Working_Databases/Odds Ratio for Manual Checking.csv")
  
  # >>> Checking ------------------------------------------------------------
  
  odds.final[grepl("ultramarine", Common_name, ignore.case = T), .(Article,
                                                                   Common_name,
                                                                   Rat_Absent.Prey_Neg, Rat_Absent.Prey_Pos,
                                                                   Rat_Present.Prey_Neg, Rat_Present.Prey_Pos)]
  odds.binary[grepl("ultramarine", Common_name, ignore.case = T), ]
  meta[grepl("ultramarine", Common_name, ignore.case = T), ]
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
  # SMD ---------------------------------------------------------------------
  unique(meta$Effect_type)
  
  smd <- meta[Effect_type == "SMD"]
  
  smd[Predator_mean == 0, Predator_PA := "Absent"]
  smd[Predator_mean == 1, Predator_PA := "Present"]
  # smd$Predator_mean <- NULL
  
  smd
  
  unique(smd$Predator_comment)
  # View(smd[, .(Article, scientificName, Study_rodent,
  #              Study_type, Year, Site,
  #              Predator_PA, Predator_error,
  #              Prey_mean, Prey_error, Sample_size_overall,
  #              Prey_sample_size, Predator_sample_size
  # )])
  
  smd # ok. so we have a couple things here
  # Tabak & Rayner et al. can be simply cast wide
  # by rat presence/absence 
  
  # Imber et al. can be summarized across years and cast wide
  # too bad there wasn't an error bar per year.
  # 
  
  # Newton et al., did a before-after control-impact.
  # Compare between I suppose? Error is already calculated...
  
  # ---------------- Tabak -------------------------------!
  smd.1 <- smd[grepl("Tabak", Article), ]
  
  # View(smd.1[, .(Article, scientificName, Study_rodent,
  #                Study_type, Year, Site,
  #                Predator_PA, Predator_error,
  #                Prey_mean, Prey_error, Sample_size_overall,
  #                Prey_sample_size, Predator_sample_size
  # )])
  smd.1$Predator_comment <- NA
  smd.1[, !c("Predator_mean"), with = F]
  smd.1.ready <- dcast(smd.1[, !c("Predator_mean",
                                  "Predator_HPD_Credible_Intervals_Lower_95", 
                                  "Predator_HPD_Credible_Intervals_Upper_95"), 
                             with = F],
                       ... ~ Predator_PA,
                       value.var = c("Prey_mean", "Prey_error",
                                     "Sample_size_overall", "Prey_sample_size",
                                     "Predator_sample_size"))
  smd.1.ready
  
  smd <- smd[!Article %in% smd.1$Article]
  
  names(smd.1.ready)
  
  # ---------------- Martin -------------------------------!
  # Martin et al., need to be summarized across islands to produce mean and SE
  smd.3 <- smd[grepl("Martin", Article)]
  unique(smd.3$Article)
  
  smd <- smd[!Article %in% smd.3$Article]
  
  smd.3
  # View(smd.3[, .(Article, scientificName, Study_rodent,
  # Study_type, Year, Site,
  # Predator_PA, Predator_error,
  # Prey_units,
  # Prey_mean, Prey_error, Sample_size_overall,
  # Prey_sample_size, Predator_sample_size)])
  smd.3[, Prey_mean := as.numeric(Prey_mean)]
  
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
  
  # View(smd.3.sum[, .(Article, scientificName, Study_rodent,
  #                    Study_type,
  #                    Predator_PA, Predator_error,
  #                    Prey_mean, Prey_error, Sample_size_overall,
  #                    Prey_sample_size, Predator_sample_size)])
  # smd.3.sum
  
  # Now, cast wide:
  smd.3.ready <- dcast(smd.3.sum,
                       ... ~ Predator_PA,
                       value.var = c("Prey_mean", "Prey_error", "Sample_size_overall",
                                     "Prey_sample_size",
                                     "Predator_sample_size"))
  smd.3.ready
  names(smd.3.ready)
  smd <- smd[!Article %in% smd.3.ready$Article]
  
  # --------------- Bourgeous ---------------------------!
  smd.4 <- smd[grepl("Bourgeois", Article), ]
  smd.4
  # View(smd.4[, .(Article, scientificName, Study_rodent,
  #                Study_type, Year, Site,
  #                Predator_PA, Predator_error,
  #                Prey_mean, Prey_error, Sample_size_overall,
  #                Prey_sample_size, Predator_sample_size
  # )])
  smd.4[, Prey_mean := as.numeric(Prey_mean)]
  smd.4$Prey_sample_size
  
  smd.4.sum <- smd.4[, .(Prey_mean = mean(Prey_mean),
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
                            Latitude, Longitude, Description, Site, #Year,
                            Predator_units, Predator_error, Predator_sample_size,
                            #Predator_comment, 
                            Prey_units,
                            Prey_sample_size, Prey_comment,
                            Source, Data_notes, Effect_type,
                            Predator_PA
                     )]
  smd.4.sum$Prey_sample_size
  
  # smd.4.sum$Predator_comment <- NA
  smd.4.ready <- dcast(smd.4.sum,
                       ... ~ Predator_PA,
                       value.var = c("Prey_mean", "Prey_error",
                                     "Sample_size_overall", "Prey_sample_size",
                                     "Predator_sample_size"))
  smd.4.ready
  
  smd <- smd[!Article %in% smd.4.ready$Article]
  
  # --------------Whitworth ------------------------------!
  smd.5 <- smd[grepl("Whitworth", Article, ignore.case = T), ]
  smd.5
  
  # View(smd.5[, .(Article, scientificName, Study_rodent,
  #                Study_type, Year, Site,
  #                Predator_PA, Predator_mean, Predator_error,
  #                Prey_mean, Prey_error, Sample_size_overall,
  #                Prey_sample_size, Predator_sample_size
  # )])
  
  smd.5[, Prey_mean := as.numeric(Prey_mean)]
  smd.5.sum <- smd.5[, .(Prey_mean = mean(Prey_mean),
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
                            Latitude, Longitude, Description, Site, #Year,
                            Predator_units, Predator_error, Predator_sample_size,
                            #Predator_comment, 
                            Prey_units,
                            Prey_sample_size, Prey_comment,
                            Source, Data_notes, Effect_type,
                            Predator_PA
                     )]
  smd.5.sum
  
  # smd.4.sum$Predator_comment <- NA
  smd.5.ready <- dcast(smd.5.sum,
                       ... ~ Predator_PA,
                       value.var = c("Prey_mean", "Prey_error",
                                     "Sample_size_overall", "Prey_sample_size",
                                     "Predator_sample_size"))
  smd.5.ready
  
  smd <- smd[!Article %in% smd.5.ready$Article]
  
  smd$Article
  
  # >>> Rbind & save ----------------------
  setdiff(names(smd.1.ready), names(smd.3.ready))
  setdiff(names(smd.1.ready), names(smd.4.ready))
  setdiff(names(smd.3.ready), names(smd.4.ready))
  
  smd.final <- rbind(smd.1.ready, smd.3.ready, smd.4.ready,
                     smd.5.ready,
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
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
  # Correlation ---------------------------------------------------------------------
  unique(meta$Effect_type)
  meta[Effect_type %in% c("Correlation",
                          "Correlation or Odds Ratio, if we known number of fledglings monitored",
                          "Correlation or Odds Ratio, if we known number of pairs monitored"), ]$Effect_type
  
  corr <- meta[Effect_type %in% c("Correlation",
                                  "Correlation or Odds Ratio, if we known number of fledglings monitored",
                                  "Correlation or Odds Ratio, if we known number of pairs monitored"), ]
  corr
  corr[, Effect_type := "Correlation"]
  
  View(corr[, .(Article, Effect_type, Study_type, Site, Year, Predator_mean, Predator_error,
                Prey_mean, Prey_error, Prey_sample_size, Sample_size_overall,
                Predator_units, Predator_comment, Prey_units)])
  
  # I think these are all good.
  corr[Article == "Radley, P.M., Davis, R.A. and Doherty, T.S., 2021. Impacts of invasive rats and tourism on a threatened island bird: the Palau Micronesian Scrubfowl. Bird Conservation International, 31(2), pp.206-218.",
       .(Prey_mean, Predator_mean)]
  
  corr[is.na(as.numeric(Predator_mean)), .(Predator_mean)]
  
  corr[Predator_mean == "present"] #
  #
  corr[Study_ID == "study_15"]
  
  corr[, `:=` (Predator_mean = str_trim(Predator_mean),
               Prey_mean = str_trim(Prey_mean))]
  corr[is.na(as.numeric(Predator_mean)), .(Predator_mean)]
  
  corr[is.na(as.numeric(Prey_mean)), .(Study_ID, Prey_mean)]
  corr[Study_ID %in% c("study_27", "study_30"), ]
  #
  
  #
  
  corr[Predator_mean == "present", Predator_mean := NA]
  
  corr[Study_ID %in% c("study_27", "study_30"), .(Article, Study_rodent)]
  
  nrow(corr)
  
  nrow(corr[!(Study_ID %in% c("study_27", "study_30") &
                Study_rodent == "Mus musculus")])
  corr <- corr[!(Study_ID %in% c("study_27", "study_30") &
                   Study_rodent == "Mus musculus")]
  
  
  
  corr[is.na(as.numeric(Prey_mean)), .(Study_ID, Prey_mean)]
  
  corr[, `:=` (Predator_mean = as.numeric(Predator_mean),
               Prey_mean = as.numeric(Prey_mean))]
  
  corr <- corr[!is.na(Prey_mean), ]
  corr[is.na(Predator_mean)]
  
  fwrite(corr, "data/Working_Databases/Correlation meta analysis for manual editing.csv")
  #
  
}
