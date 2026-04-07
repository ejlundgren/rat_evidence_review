
rm(list = ls())

library("groundhog")

library("data.table")
library("ggplot2")
library("tidyr")
library("readxl")
library("stringr")
library("dplyr")
library("metafor")
library("tidyr")
library("broom")
library("rotl")
library("ape")
library("orchaRd")

theme_lundy <- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

# Load original meta-analysis:
original <- read_excel("data/Raw/Meta-analysis-8-Dec.xlsx")
setDT(original)
original$Study_ID

meta <- fread("builds/meta_analysis/compiled_ready_to_analyze.csv")

# Data fixes --------------------------------------------------------------
unique(meta$Abundance_reproduction)
setDT(meta)
# meta[Abundance_reproduction == "" &
#        Prey_units == "Abundance", 
#      Abundance_reproduction := "Abundance"]
# meta[Abundance_reproduction == "" &
#        Prey_units %in% c("Number of nests", "Number of nests per site"), 
#      Abundance_reproduction := "Reproduction"]
# unique(meta$Abundance_reproduction)
# 
# meta[, Abundance_reproduction_simple := Abundance_reproduction]
# meta[Abundance_reproduction == "Reproduction (opposite)"]$Hypothesis_supported
# meta[Abundance_reproduction == "Reproduction (opposite)"]$Study_ID
# meta[Abundance_reproduction == "Reproduction (opposite)"]
# meta[Abundance_reproduction_simple == "Reproduction (opposite)",
#      Abundance_reproduction_simple := "Reproduction"]
# unique(meta$Abundance_reproduction_simple)

unique(meta$Hypothesis_supported_when)
meta[Hypothesis_supported_when == "", ]
meta[Hypothesis_supported_when == "", Hypothesis_supported_when := "Negative"]

# These were already inverted in script 0
# meta[Abundance_reproduction == "Reproduction (opposite)", `:=` (yi_RR = -yi_RR,
#                                                                 yi_OR = -yi_OR)]
# meta[Abundance_reproduction_simple == "Presence-absence", Abundance_reproduction_simple := "Abundance"]

# # meta[, Effect_type_simple := Effect_type]
# unique(meta$Effect_type_simple)
# meta[Effect_type_simple %in% c("Binary Odds Ratio", "Continuous Odds Ratio"),
#      Effect_type_simple := "Odds Ratio"]


# >>> Data exclusions (from errors) ---------------------------------------
# unique(sort(meta$Study_ID))
# meta[Study_ID %in% c("study_31", "study_32", "study_33"), ]$Study_ID
# meta <- meta[!Study_ID %in% c("study_31", "study_32", "study_33"), ]

unique(meta$analysis_group)

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Merge in traits / timing ------------------------------------------------
traits <- read_excel("builds/meta_analysis/Traits_for_meta_analysis_aw.xlsx")
traits
setDT(traits)
unique(meta[, .(scientificName, Study_ID)])
names(traits)
(traits[, .(NestSite_ground, Nest_site_height_meters)])

traits
unique(traits[, .(NestSite_ground, NestSite_underground,
                  NestSite_tree, NestSite_cliff_bank,
                  NestSite_waterbody, NestSite_termite_ant)])
traits[, NestSite := ifelse(NestSite_ground == 1 |
                              NestSite_underground == 1 |
                              NestSite_cliff_bank == 1,
                              "Terrestrial", NA)]
traits[is.na(NestSite), NestSite := ifelse(NestSite_tree == 1 |
                                             NestSite_nontree == 1,
                                           "Vegetation", NestSite)]


traits <- traits[, .(scientificName, Mass, Clutch_size_mean, Longevity_years,
                     Incubation_days, 
                     NestSite,
                     Bird_Type)]
traits[duplicated(scientificName)]
traits[, Mass := as.numeric(Mass)]
traits[, Longevity_years := as.numeric(Longevity_years)]
traits[, Clutch_size_mean := as.numeric(Clutch_size_mean)]
traits[, Incubation_days := as.numeric(Incubation_days)]
traits

meta.m1 <- merge(meta,
                 traits,
                 by = "scientificName",
                 all.x = T)
meta.m1

timing <- read_excel("builds/meta_analysis/Timing for meta-analysis.xlsx")
timing

setDT(timing)
timing
#
timing$Study_ID
nrow(meta.m1[duplicated(Study_ID), ]) # should be 0 rows?
nrow(meta[duplicated(Study_ID), ]) 
meta.m1[Study_ID == "study_47"] # Separate correlations in the same study
meta.m1[duplicated(Study_ID), ]$Effect_type_simple

# well it shouldn't matter for the timing...
timing[, `:=` (Timeline_minimum = as.numeric(Timeline_minimum),
               Timeline_maximum = as.numeric(Timeline_maximum))]
timing.sum <- timing[, .(min_time_since = min(Timeline_minimum, na.rm = T),
                         max_time_since = max(Timeline_maximum, na.rm = T)),
                     by = .(Study_ID, scientificName)]
timing.sum[duplicated(Study_ID), ]
# ok good.

meta.m2 <- merge(meta.m1,
                 timing.sum[, !"scientificName", with = F],
                 by = "Study_ID",
                 all.x = T)
meta.m2[is.na(min_time_since), ]

timing[Study_ID == "study_50", ]

#
meta <- copy(meta.m2)

unique(meta$analysis_group)

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Format IDs and variables for random effects --------------------------------
# meta[, Obs_ID := paste0("Obs_", seq(1:.N))]
#effect_size_id

meta[, Article_ID := paste0("Article_", 
                            as.numeric(as.factor(Article)))]

meta[, .(n = uniqueN(Article_ID)), by = .(Article)][n > 1, ]
# we should control for rodent species...won't be easy to do though...
# perhaps at the very least we can standardize the rodent species column?
paste(sort(unlist(str_split("Rattus, Mus", pattern = ", "))),
      collapse = ", ")

unique(meta$Study_rodent)
# Oh, I guess it doesn't matter huh.
meta$Study_rodent

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Prepare phylogeny -------------------------------------------------------

# November 27: getting an HTTP 500 error...Going to try again later.
# Next time, put this in an if() so I don't need internet
# to rerun code.
rerun <- F 
if(rerun){
  nms <- unique(meta$scientificName)
  
  (nms_res <- tnrs_match_names(nms))
  
  tnrs_match_names("Apteryx")
  tnrs_match_names("Apteryx mantelli")
  
  setDT(nms_res)
  nms_res
  nms_res[search_string == "chasiempis ibidis"]
  nms_res[search_string == "apteryx mantelli"]
  # Hm. 
  
  meta[, phylo_Species := scientificName]
  meta[phylo_Species == "Chasiempis ibidis", phylo_Species := "Chasiempis sandwichensis"]
  meta[phylo_Species == "Apteryx mantelli", phylo_Species := "Apteryx"] # australis
  
  meta[phylo_Species == "Pomarea spp.", phylo_Species := "Pomarea nigra"] # australis
  
  nms <- unique(meta$phylo_Species)
  (nms_res <- tnrs_match_names(nms))
  nms_res
  
  
  #
  tree <- tol_induced_subtree(ott_ids = ott_id(nms_res))
  tree
  plot(tree)
  tree$tip.label
  # Need to match phylo_Species with those names...
  
  setDT(nms_res)
  nms_res[, label := paste0(gsub(" ", "_", unique_name),
                            "_ott", ott_id)]
  nms_res
  
  sort(tree$tip.label)
  setdiff(nms_res$label, tree$tip.label)
  
  
  setdiff(tree$tip.label, nms_res$label)
  
  nms_res[, search_string := str_to_sentence(search_string)]
  unique(nms_res$search_string)
  setdiff(nms_res$search_string, meta$phylo_Species)
  setdiff(meta$phylo_Species, nms_res$search_string)
  
  saveRDS(tree, "builds/meta_analysis/phylogeny/tree.Rds")
  saveRDS(nms_res, "builds/meta_analysis/phylogeny/name_key.Rds")
  
}else{
  
  tree <- readRDS("builds/meta_analysis/phylogeny/tree.Rds")
  nms_res <- readRDS("builds/meta_analysis/phylogeny/name_key.Rds")
  
  meta[, phylo_Species := scientificName]
  meta[phylo_Species == "Chasiempis ibidis", phylo_Species := "Chasiempis sandwichensis"]
  meta[phylo_Species == "Apteryx mantelli", phylo_Species := "Apteryx"] # australis
  meta[phylo_Species == "Pomarea spp.", phylo_Species := "Pomarea nigra"] # australis
  
  setdiff(nms_res$search_string, meta$phylo_Species)
  setdiff(meta$phylo_Species, nms_res$search_string)
  
  
  meta.m1 <- merge(meta,
                   nms_res[, .(search_string, label)],
                   by.x = "phylo_Species",
                   by.y = "search_string",
                   all.x = T)
  meta.m1
  setnames(meta.m1, "label", "phylo_species_tiplabel")
  
  meta.m1
  setdiff(meta.m1$phylo_species_tiplabel, tree$tip.label)
  setdiff(tree$tip.label, meta.m1$phylo_species_tiplabel)
  
  meta <- copy(meta.m1)
  
}

createPhyloCorr <- function(spp_list, tree){
  tree.filt <- keep.tip(tree, spp_list)
  tree.br <- compute.brlen(tree.filt)
  tree.corr <- vcv(tree.br, corr=T)
  return(tree.corr)
}

meta[is.na(phylo_species_tiplabel)] # should be 0 rows

createPhyloCorr(spp_list = meta[!is.na(phylo_species_tiplabel)]$phylo_species_tiplabel,
                tree)

meta.final <- copy(meta)

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Create model guide ------------------------------------------------------


#
guide <- CJ(analysis_group = unique(meta.final[!is.na(analysis_group), ]$analysis_group),
            #exclude_converted_effect_sizes = c("yes", "no"),
            phylogeny = c("yes", "no"),
            rat_id = c("yes", "no"),
            non_phylo_bird = c("yes", "no"),
            moderators = c("1", "min_time_since", "max_time_since", 
                           "Clutch_size_mean", "Longevity_years",
                           "NestSite"))

# Not enough N for this, I already know:
guide <- guide[!(phylogeny == "yes" & non_phylo_bird == "yes"), ]

# Only applies to one: Drop the OR conversion'
meta.final[grepl("convert", effect_size_type), ]
# can't get this to work...:
# guide[!(exclude_converted_effect_sizes == "yes" & 
#           non_phylo_bird == "Long-term abundance"), ]
# 
# guide <- guide[!(exclude_converted_effect_sizes == "yes" & 
#                    non_phylo_bird != "Long-term abundance"), ]
# unique(guide[, .(exclude_converted_effect_sizes,
#                  analysis_group)])
guide <- rbind(guide,
               guide[analysis_group == "Long-term abundance"] %>%
                 mutate(exclude_converted_effect_sizes = "yes"),
               fill = T)
guide[is.na(exclude_converted_effect_sizes), exclude_converted_effect_sizes := "no"]

# Create exclusion formula:
guide[, exclusion := paste0("analysis_group == ", "'", analysis_group, "'")]
guide[exclude_converted_effect_sizes == "yes", 
      exclusion := paste0(exclusion, " & !grepl('convert', effect_size_type)")]

guide[moderators != "1", exclusion := paste0(exclusion, " & complete.cases(", moderators, ")")]
unique(guide$exclusion)


# Create fixed effects formula:
guide[, formula := paste0("effect_size ~ ", moderators)]
guide[moderators == "NestSite", formula := paste0(formula, " - 1")]

# Formulate random effects:
guide[, random_formula := "list(~1 | Article_ID / effect_size_id"]
guide[rat_id == "yes", random_formula := paste0(random_formula,
                                                ", ~1 | Study_rodent")]
guide

guide[non_phylo_bird == "yes", random_formula := paste0(random_formula,
                                                        ", ~1 | scientificName")]


guide[phylogeny == "yes", random_formula := paste0(random_formula,
                                                        ", ~1 | phylo_species_tiplabel")]

unique(guide$random_formula)
guide[, random_formula := paste0(random_formula, ")")]

guide

# >>> Add a null model for comparisons.... --------------------------------

guide[, model_comparison_id := paste0("model_series_", seq(1:.N))]
guide[moderators != "1", null_formula := "effect_size ~ 1"]
guide

guide.mlt <- melt(guide,
                  measure.vars = c("formula", "null_formula"),
                  value.name = "formula",
                  variable.name = "model_type")
guide.mlt[, model_type := ifelse(model_type == "formula",
                                 "base_model", "intercept_only")]
guide.mlt

guide.mlt[, model_id := paste0("model_", seq(1:.N))]
guide.mlt

# guide.final <- copy(guide.mlt)

guide.mlt <- guide.mlt[!is.na(formula), ]

# >>> Testing / sample sizes -------------------------------------------------------------

for(i in 1:nrow(guide.mlt)){
  as.formula(guide.mlt[i, ]$formula)
} # if this runs through then formulas are correct.

Ns <- list()
out <- c()

for(i in 1:nrow(guide.mlt)){
  
  out <- meta.final[eval(parse(text = guide.mlt[i, ]$exclusion)), ]
  Ns[[i]] <- out[, .(N = .N,
                     n_articles = uniqueN(Article),
                     n_Study_rodent = uniqueN(Study_rodent),
                     n_scientificName = uniqueN(scientificName),
                     n_nestsite = uniqueN(NestSite))]

}

names(Ns) <- guide.mlt$model_id
Ns <- rbindlist(Ns, idcol = "model_id")
Ns#

guide.mlt[model_id == "model_1"]

guide.mlt.mrg <- merge(guide.mlt,
                       Ns,
                       by = 'model_id',
                       all.x = T)
guide.mlt.mrg[exclude_converted_effect_sizes == "yes"]


unique(guide.mlt.mrg$analysis_group)

# >>> Filter out low sample sizes -----------------------------------------

# maybe we leave them all in...
# guide.mlt.mrg.filt <- guide.mlt.mrg[n_articles > 1, ]
# guide.mlt.mrg.filt

range(guide.mlt.mrg$n_articles)

# guide.mlt.mrg.filt <- guide.mlt.mrg[n_articles >= 3, ]

guide.mlt.mrg.filt <- guide.mlt.mrg[!(rat_id == "yes" & n_Study_rodent < 3), ]

guide.mlt.mrg.filt <- guide.mlt.mrg.filt[!(non_phylo_bird == "yes" & n_scientificName < 3), ]

guide.mlt.mrg.filt <- guide.mlt.mrg.filt[!(phylogeny == "yes" & n_scientificName < 3), ]

guide.mlt.mrg.filt <- guide.mlt.mrg.filt[!(moderators == "NestSite" & n_nestsite < 2), ]

guide.mlt.mrg.filt

guide.final <- copy(guide.mlt.mrg.filt)

# >>> Add column to identify numeric moderators ---------------------------
unique(guide.final$moderators)
unique(guide.final$model_type)
guide.final[moderators %in% c("max_time_since", "min_time_since", "Clutch_size_mean",
                              "Longevity_years")  &
              model_type == "base_model", ]

#
guide.final[, moderator_type := ifelse(moderators %in% c("max_time_since", "min_time_since", "Clutch_size_mean",
                                                         "Longevity_years") &
                                         model_type == "base_model",
                                       "numeric", "categorical_or_intercept")]

guide.final[moderator_type == "numeric",
            formula := paste0(formula, "_scaled")]
guide.final

guide.final[moderator_type == "numeric", ]$formula

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Run models --------------------------------------------------------------

guide.final
meta.final[is.na(effect_size), ]
meta.final <- meta.final[!is.na(effect_size)]
sub.dat <- c()
m <- c()
i <- 4
var <- c()
errors <- c()

file.remove(list.files("builds/meta_analysis/models", full.names = T))

for(i in 1:nrow(guide.final)){
  
  sub.dat <- meta.final[eval(parse(text = guide.final[i, ]$exclusion))]
  
  if(guide.final[i, ]$moderator_type == "numeric"){
    
    var <- sub.dat[[guide.final[i, ]$moderators]]
    sub.dat[, num_scaled := scale(var)]
    
    setnames(sub.dat, 
             "num_scaled",
             paste0(guide.final[i, ]$moderators, "_scaled"))
  }
  
  if(guide.final[i, ]$phylogeny == "no"){
    tryCatch(
      expr = {
        m <- rma.mv(as.formula(guide.final[i, ]$formula),
                    V = sampling_variance,
                    random = eval(parse(text = guide.final[i, ]$random)),
                    method = "ML",
                    test = "t",
                    data = sub.dat)
        saveRDS(m, paste0("builds/meta_analysis/models/", guide.final[i, ]$model_id, ".Rds"))
    },
    error = function(e){
      print(e)
    })#,
    # error = function(e){
    #   errors[i] <- e
    # }
    
  }else{
    
    tryCatch(
      expr = {
        m <- rma.mv(as.formula(guide.final[i, ]$formula),
                    V = sampling_variance,
                    random = eval(parse(text = guide.final[i, ]$random)),
                    R = list(phylo_species_tiplabel = createPhyloCorr(sub.dat$phylo_species_tiplabel,
                                                                      tree)),
                    method = "ML",
                    test = "t",
                    data = sub.dat)
        saveRDS(m, paste0("builds/meta_analysis/models/", guide.final[i, ]$model_id, ".Rds"))
      },
      error = function(e){
        print(e)
      })#,
      # error = function(e){
      #   errors[i] <- e
      # }
  }
  
  m <- c()
  
  cat(i, "/", nrow(guide.final), "\r")
}
i
guide.final[i, ] # this seems unlikely too... it's an intercept only...
guide.final

# errors
guide.final[, model_path := paste0("builds/meta_analysis/models/", model_id, ".Rds")]
guide.final[!(file.exists(model_path)), ] # hopefully 0 rows but here 2
unique(guide.final[file.exists(model_path)]$analysis_group)

guide.final[file.exists(model_path) & analysis_group == "Short-term reproduction" &
              moderators == "1"]
# That's not good.

sub.guide <- guide.final[analysis_group == "Short-term reproduction" &
              moderators == "1" & 
              phylogeny == "no" &
              rat_id == "no" &
              non_phylo_bird == "no"]

m <- rma.mv(as.formula(sub.guide$formula),
            V = sampling_variance,
            random = list(~1 | Article_ID),#eval(parse(text = sub.guide$random)),
            method = "ML",
            test = "t",
            data = meta.final[eval(parse(text = sub.guide$exclusion)), ])
m

guide.final[analysis_group == "Short-term reproduction" &
              moderators == "1" & 
              phylogeny == "no" &
              rat_id == "no" &
              non_phylo_bird == "no",
            random := "~1|Article_ID"]

saveRDS(m, paste0("builds/meta_analysis/models/", sub.guide$model_id, ".Rds"))


# saveRDS(m, paste0("builds/meta_analysis/models/", guide.final[i, ]$model_id, ".Rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Compare intercept versus alternative  ----------------------------------------------------------
# Going to use LRT tests on this...Start by casting wide.
guide.final[model_comparison_id == "model_series_2", ]


guide.final.filter <- guide.final[file.exists(model_path), ]
guide.final.filter

guide.final.wide <- dcast(data = guide.final.filter[, !"moderator_type", with = F],
                              ... ~ model_type,
                              value.var = c("model_id", "model_path",
                                            "formula"))
guide.final.wide

guide.final.wide <- guide.final.wide[!is.na(formula_base_model), ]
guide.final.wide
# A lot of these did not run...

# why didn't these intercept only models run?????

m_int <- c()
m_base <- c()
i <- 1
out <- c()

for(i in 1:nrow(guide.final.wide)){
  #
  
  m_base <- readRDS(guide.final.wide[i, ]$model_path_base_model)
  guide.final.wide[i, I2_base := i2_ml(m_base)[1]]
  guide.final.wide[i, BIC_base := BIC(m_base)[1]]
  
  #
  if(!is.na(guide.final.wide[i, ]$model_id_intercept_only)){
    m_int <- readRDS(guide.final.wide[i, ]$model_path_intercept_only)
    guide.final.wide[i, I2_intercept := i2_ml(m_int)[1]]
    guide.final.wide[i, BIC_intercept := BIC(m_int)] 
    
    out <- anova(m_base, m_int)
    guide.final.wide[i, LRT := out$LRT]
    guide.final.wide[i, LRT_pval := out$pval]
    
  }
  m_base <- c()
  m_int <- c()
  cat(i, "/", nrow(guide.final.wide), "\r")

}

guide.final.wide

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Compare random effects & flag overfitting----------------------------------------------------------

# flag_overfitting <- function(x, na.rm = TRUE) {
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
#   H <- 1.5 * IQR(x, na.rm = na.rm)
#   x < (qnt[1] - H)
#   # I think overfitting only happens on the LOW end for sigma and I2. So ignore high end overfitting
#   # e.g., x > qnt[2] + H # https://stackoverflow.com/questions/44089894/identifying-the-outliers-in-a-data-set-in-r
#   return((x < (qnt[1] - H)))
#   # again, 71.41451 is an outlier...evne though it is identical. Huh.
# }
guide.final.wide

guide.final.wide[, random_effects_comparison_id := paste0("random_model_series_", .GRP),
            by = .(analysis_group, moderators, exclusion, exclude_converted_effect_sizes)]
guide.final.wide

guide.final.wide[random_effects_comparison_id == "random_model_series_1", ]

guide.final.wide[, best_model_random := ifelse(model_id_base_model == .SD[which.min(BIC_base)]$model_id_base_model,
                                "best_model", "not_best"),
                 by = .(random_effects_comparison_id)]
guide.final.wide
guide.final.wide[random_effects_comparison_id == "random_model_series_1", ]
guide.final.wide[random_effects_comparison_id == "random_model_series_2", ]
guide.final.wide[random_effects_comparison_id == "random_model_series_10", ]
guide.final.wide[, delta_BIC_random := BIC_base - .SD[best_model_random == "best_model"]$BIC_base,
                 by = .(random_effects_comparison_id)]
guide.final.wide

range(guide.final.wide[best_model_random == "not_best"]$delta_BIC_random)
# Good. All > 2. 

guide.final.wide[, delta_I2_random := round(I2_base - .SD[best_model_random == "best_model"]$I2_base),
                 by = .(random_effects_comparison_id)]

range(guide.final.wide[best_model_random == "not_best"]$delta_I2_random)

guide.final.wide



# >>> Report i2 of phylogeny ----------------------------------------------

ms <- lapply(guide.final.wide[grepl("phylo_species_tiplabel", random_formula) &
                                !is.na(model_path_intercept_only)]$model_path_intercept_only,
             readRDS)
ms

out <- lapply(ms,
              i2_ml)

out <- lapply(out,
              round,
              2)
out

lapply(out, 
       "[[", "I2_phylo_species_tiplabel")


# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Which moderators improve model quality? ----------------------------------------------------------

guide.final.wide[LRT_pval < 0.05 & best_model_random == "best_model", ]

# long-term presence absence and clutch size, longevity, max/min time since
guide.final.wide[analysis_group == "Short-term reproduction"]

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Save --------------------------------------------------------------------
saveRDS(meta.final,
        "builds/meta_analysis/final_meta_analytic_dataset_post_modeling.Rds")

saveRDS(guide.final.wide,
        "builds/meta_analysis/final_model_guide.Rds")

