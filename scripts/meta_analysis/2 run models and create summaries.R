
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

theme_RATS<- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

meta <- fread("builds/meta_analysis/compiled_ready_to_analyze.csv")

# Data fixes --------------------------------------------------------------
unique(meta$Abundance_reproduction)
setDT(meta)


unique(meta$Hypothesis_supported_when)
meta[Hypothesis_supported_when == "", ]
meta[Hypothesis_supported_when == "", Hypothesis_supported_when := "Negative"]

# >>> Data exclusions (from errors) ---------------------------------------

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

#' *Based on a reviewer comment, we'll consider the mixed groups to be Rattus rattus*
meta[, phylo_rodent := Study_rodent]
meta[grepl(", ", Study_rodent), phylo_rodent := "Rattus rattus"]
# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Prepare phylogenies -------------------------------------------------------
# In an if statement in case I'm out of service..

rerun <- F 
if(rerun){
# >>> Bird phylogeny ------------------------------------------------------
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
  
  # I could have done this more efficiently but oh well
  saveRDS(tree, "builds/meta_analysis/phylogeny/bird_tree.Rds")
  saveRDS(nms_res, "builds/meta_analysis/phylogeny/bird_name_key.Rds")

# >>> Rodent phylogeny -------------------------------------------------------

  nms <- unique(meta$phylo_rodent)
  
  (nms_res <- tnrs_match_names(nms))
  
  tree <- tol_induced_subtree(ott_ids = nms_res$ott_id)
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
  
  saveRDS(tree, "builds/meta_analysis/phylogeny/rat_tree.Rds")
  saveRDS(nms_res, "builds/meta_analysis/phylogeny/rat_name_key.Rds")
  
  
  
}else{
  
  bird_tree <- readRDS("builds/meta_analysis/phylogeny/bird_tree.Rds")
  nms_res <- readRDS("builds/meta_analysis/phylogeny/bird_name_key.Rds")
  
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
  setdiff(meta.m1$phylo_species_tiplabel, bird_tree$tip.label)
  setdiff(bird_tree$tip.label, meta.m1$phylo_species_tiplabel)
  
  # Now rats:
  rat_tree <- readRDS("builds/meta_analysis/phylogeny/rat_tree.Rds")
  nms_res <- readRDS("builds/meta_analysis/phylogeny/rat_name_key.Rds")
  
  setdiff(nms_res$search_string, meta$phylo_rodent)
  setdiff(meta$phylo_Species, nms_res$search_string)
  setnames(nms_res, "search_string", "phylo_rodent")
  setnames(nms_res, "label", "rodent_label")
  
  meta.m2 <- merge(meta.m1,
                   nms_res[, .(phylo_rodent, rodent_label)],
                   by = "phylo_rodent",
                   all.x = T)
  meta.m2
  
  meta <- copy(meta.m2)
  
}

meta <- meta[!is.na(effect_size)]

createPhyloCorr <- function(spp_list, tree){
  tree.filt <- keep.tip(tree, spp_list)
  tree.br <- compute.brlen(tree.filt)
  tree.corr <- vcv(tree.br, corr=T)
  return(tree.corr)
}

meta[is.na(phylo_species_tiplabel)] # should be 0 rows
meta[is.na(rodent_label)] # should be 0 rows

# Test function:
meta[is.na(phylo_species_tiplabel)]
createPhyloCorr(spp_list = meta$phylo_species_tiplabel,
                bird_tree)

meta <- copy(meta)

createPhyloCorr(spp_list = meta$rodent_label,
                rat_tree)

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Create model guide ------------------------------------------------------

#
guide <- CJ(analysis_group = unique(meta[!is.na(analysis_group), ]$analysis_group),
            #exclude_converted_effect_sizes = c("yes", "no"),
            phylogeny = c("yes", "no"),
            non_phylo_bird = c("yes", "no"),
            rat_phylogeny = c("yes", "no"),
            rat_id = c("yes", "no"),
            moderators = c("1"))

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
                                                ", ~1 | phylo_rodent")]
guide

guide[rat_phylogeny == "yes", random_formula := paste0(random_formula,
                                                ", ~1 | rodent_label")]

#
guide[non_phylo_bird == "yes", random_formula := paste0(random_formula,
                                                        ", ~1 | scientificName")]


guide[phylogeny == "yes", random_formula := paste0(random_formula,
                                                        ", ~1 | phylo_species_tiplabel")]

unique(guide$random_formula)
guide[, random_formula := paste0(random_formula, ")")]

guide


# >>> Filter out comparisons with insufficient n-------------------

for(i in 1:nrow(guide)){
  sub.dat <- meta[eval(parse(text = guide[i, ]$exclusion)), ]
  guide[i, n_rodents := length(unique(sub.dat$phylo_rodent))]
  guide[i, n_articles := length(unique(sub.dat$Article_ID))]
  guide[i, n_observations := length(unique(sub.dat$effect_size_id))]
  guide[i, n_birds := length(unique(sub.dat$scientificName))]
  
  
}

# Going to keep n_articles to 2 to be extremely generous
guide <- guide[n_articles >= 2, ]
# but no need for rat phylogeny or rat-id with 1 rat. 
guide <- guide[!(rat_phylogeny == "yes" & n_rodents == 1), ]
guide <- guide[!(rat_id == "yes" & n_rodents == 1), ]

guide

# >>> Create comparison id ------------------------------------------------
# We can't compare the converted effect sizes sensitivity to others
guide[, model_id := paste0("model_", 1:.N)]

guide[, model_comparison_id := paste0("model_comp_", .GRP),
      by = .(analysis_group, exclude_converted_effect_sizes)]
guide

# >>> Create entire rma.mv call in table --------------------------------------
# Because the hard syntax differs with 'R'
# To make things simpler, let's formulate the R call here:
guide[, R_call := fcase(phylogeny == "yes" & rat_phylogeny == "no",
                            "R=list(phylo_species_tiplabel = createPhyloCorr(sub.dat$phylo_species_tiplabel, bird_tree)), ",
                            phylogeny == "yes" & rat_phylogeny == "yes",
                            "R=list(phylo_species_tiplabel = createPhyloCorr(sub.dat$phylo_species_tiplabel, bird_tree), rodent_label = createPhyloCorr(sub.dat$rodent_label, rat_tree)), ",
                            phylogeny == "no" & rat_phylogeny == "yes",
                            "R=list(rodent_label = createPhyloCorr(sub.dat$rodent_label, rat_tree)), ",
                            phylogeny == "no" & rat_phylogeny == "no",
                            "")]
unique(guide$R_call)

#
guide[, call := paste0("rma.mv(yi = effect_size, V = sampling_variance, random=", 
                        random_formula, ", ",
                        R_call,
                        "method = 'ML', test = 't', dfs = 'contain', control=list(iter.max=100000, eval.max=100000, rel.tol=1e-9), data = sub.dat)")]
sub.dat <- meta[eval(parse(text = guide$exclusion[1]))]
m <- eval(parse(text = guide$call[1]))

guide$call[1]
# 
#' [Most complex models won't converge. Not enough data. But worth trying.]

guide$call[41]

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Run models --------------------------------------------------------------
guide[analysis_group == "Short-term reproduction"]

guide
sub.dat <- c()
m <- c()
errors <- c()
summary_list <- list()

file.remove(list.files("builds/meta_analysis/models", full.names = T))
i <- 41

for(i in 1:nrow(guide)){
  
  sub.dat <- meta[eval(parse(text = guide[i, ]$exclusion))]

  m <- try({
      eval(parse(text = guide[i, ]$call))
  })
  
  if(inherits(m, "try-error")){
    summary_list[[i]] <- data.table(guide[i, .( model_id, model_comparison_id, 
                                             analysis_group, phylogeny, non_phylo_bird, rat_phylogeny,
                                             rat_id, moderators, exclude_converted_effect_sizes, 
                                             exclusion, random_formula, n_rodents, n_birds, n_articles, n_observations)],
                                 message = as.character(m))
  }else{
    
    I2 <- i2_ml(m) |> t() |> as.data.frame()  |> setDT()
    
    summary_list[[i]] <- data.table(guide[i, .( model_id, model_comparison_id, 
                                             analysis_group, phylogeny, non_phylo_bird, rat_phylogeny,
                                             rat_id, moderators, exclude_converted_effect_sizes, 
                                             exclusion, random_formula, n_rodents, n_birds, n_articles, n_observations)],
                                 AICc = AIC(m, correct = TRUE),
                                 min_sigma = min(m$sigma2),
                                 I2,
                                 predict(m) |> as.data.frame() |> setDT(),
                                 pval = m$pval,
                                 t = m$zval,
                                 df = m$ddf)
    
    saveRDS(m, paste0("builds/meta_analysis/models/", guide[i, ]$model_id, ".Rds"))
  }

  m <- c()
  
  cat(i, "/", nrow(guide), "\r")
}
i

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Model selection  ----------------------------------------------------------
summaries <- rbindlist(summary_list, fill = TRUE)
summaries[!is.na(message)]

summaries[analysis_group == "Short-term reproduction"]

# summaries <- summaries[is.na(message)][, message := NULL]

# Exclude overfit models:
# summaries <- summaries[min_sigma > 0, ]

# Select models with lowest AIC
summaries[analysis_group == "Short-term reproduction"]

summaries[, min_aicc := min(AICc), by = .(model_comparison_id)]
best_models <- summaries[AICc == min_aicc]
best_models

best_models[duplicated(model_comparison_id), ]

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Save --------------------------------------------------------------------
saveRDS(meta,
        "builds/meta_analysis/final_meta_analytic_dataset_post_modeling.Rds")

saveRDS(best_models,
        "builds/meta_analysis/model_summaries.Rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------
# Publication bias --------------------------------------------------------

bias_guide <- copy(best_models)
bias_guide <- bias_guide[exclude_converted_effect_sizes == "no", .(model_id, analysis_group, exclusion,
                                                                   random_formula)]

bias_guide[, bias_test := "effect_size ~ bias_test"] # this sqrt(effectiveN) for lnOR and SMD and sqrt(vi) for Zr
bias_guide[, correction := "effect_size ~ correction"] # This is effectiveN or just vi
bias_guide[, decline_effect := "effect_size ~ pub_year"] # Decline effect

bias_guide <- melt(bias_guide,
                   measure.vars = c("bias_test", "correction", "decline_effect"),
                   value.name = "formula",
                   variable.name = "test_type")
bias_guide

# >>> Calculate predictors (effective N and sqrt(effective N)) -----------------------------------
unique(meta$effect_size_type)
#
meta[effect_size_type %in% c("OR"),
     `:=` (n2 =  Rat_Absent.Prey_Neg + Rat_Absent.Prey_Pos,
           n1 = Rat_Present.Prey_Neg + Rat_Present.Prey_Pos)]
#
meta[effect_size_type %in% c("SMD converted to OR", "SMD"),]
meta[effect_size_type %in% c("SMD converted to OR", "SMD"),
      `:=` (n2 = Sample_size_overall_Rats_Absent, n1 = Sample_size_overall_Rats_Present)]
#
meta[effect_size_type %in% c("SMD converted to OR", "SMD", "OR"),
     correction := (4*n1*n2)/(n1+n2)]
meta[effect_size_type %in% c("SMD converted to OR", "SMD", "OR"),
     bias_test := sqrt(correction)]

#
meta[effect_size_type %in% c("ZCOR"),
     correction := sampling_variance]
meta[effect_size_type %in% c("ZCOR"),
     bias_test := sqrt(sampling_variance)]

meta[is.na(bias_test)]
meta[is.na(correction)]

#' [This feels very futile with so few studies.]

# Publication year
meta[, pub_year := str_extract(Article,
                               "\\d+")]

meta[, pub_year := as.numeric(pub_year)]
meta[, pub_year := scale(pub_year)]

# >>> Run models ----------------------------------------------------------

models <- list()
summary_list <- list()

bias_guide
sub.dat <- c()
m <- c()
i <- 1

for(i in 1:nrow(bias_guide)){
  
  sub.dat <- meta[eval(parse(text = bias_guide[i, ]$exclusion))]
  
  models[[i]] <- try({
    rma.mv(as.formula(bias_guide[i, ]$formula),
           V = sampling_variance,
           random = eval(parse(text = bias_guide[i, ]$random_formula)),
           data = sub.dat,
           method = 'ML', test = 't', dfs = 'contain', 
           control=list(iter.max=100000, eval.max=100000, rel.tol=1e-9))
  })
  
  if(inherits(models[[i]], "try-error")){
    summary_list[[i]] <- data.table(bias_guide[i, ],
                                    message = as.character(m))
  }else{

    summary_list[[i]] <- tidy(models[[i]]) %>%
      setDT() %>%
      data.table(bias_guide[i, ],
                 df = models[[i]]$ddf[2])
    
  }
  
  cat(i, "/", nrow(bias_guide), "\r")
}
i

bias_summaries <- rbindlist(summary_list)
bias_summaries

# >>> Test for bias -------------------------------------------------------

bias_summaries[p.value < 0.05 & term == "bias_test", ]
# No evidence.
bias_summaries[term == "bias_test", .(min_est = min(estimate),
                                      max_est = max(estimate),
                                      mindf = min(df),
                                      maxdf = max(df),
                                      min_statistic = min(statistic),
                                      max_statistics = max(statistic),
                                      min_p = min(p.value),
                                      max_p = max(p.value))]


bias_summaries[p.value < 0.05 & term == "pub_year", ]
bias_summaries[term == "pub_year", .(min_est = min(estimate),
                                      max_est = max(estimate),
                                      mindf = min(df),
                                      maxdf = max(df),
                                      min_statistic = min(statistic),
                                      max_statistics = max(statistic),
                                      min_p = min(p.value),
                                      max_p = max(p.value))]
# >>> Table ---------------------------------------------------------------
bias_summaries

unique(meta[, .(analysis_group, effect_size_type)])


model.gt <- bias_summaries %>%
  filter(test_type %in% c("bias_test", "decline_effect")) %>%
  mutate(test_type = case_when(test_type == "bias_test" ~ "Small-sample bias",
                               test_type == "decline_effect" ~ "Decline effect")) %>%
  select(test_type, term, estimate, std.error, statistic, df, p.value, 
         analysis_group) %>%
  # mutate(term = case_when(term == "bias_test" ~ "sqrt( effective N )",
  #                         term == "pub_year" ~ "Publication year",
  #                         term == "intercept" ~ "Intercept")) %>%
  mutate(term = case_when(term == "bias_test" & analysis_group %in% c("Long-term abundance",
                                                                      "Before-after eradication reproduction",
                                                                      "Long-term abundance",
                                                                      "Before-after eradication abundance") ~
                                  "$\\sqrt{N_{0}}$",
                          term == "bias_test" & analysis_group %in% c("Short-term reproduction",
                                                                      "Short-term abundance") ~
                                  "$\\sqrt{vi}$",
                          term == "pub_year" ~ "Publication year",
                          term == "intercept" ~ "Intercept")) %>%
  dplyr::rename("Test" = "test_type",
         "Term" = "term",
         "Estimate" = "estimate",
         "t" = "statistic",
         "p" = "p.value",
         "SE" = "std.error") %>%
  group_by(analysis_group) %>%
  gt() %>%
  fmt_number(
    columns = everything(), 
    decimals = 2         
  ) %>%
  fmt_markdown(columns = Term) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) %>%
  opt_table_font(
    size = 12
  )

print(model.gt)
gtsave(model.gt, filename = "figures/meta_analysis/publication bias table.pdf")
