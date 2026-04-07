
rm(list = ls())

pacman::p_load("data.table", "ggplot2", "tidyr", "readxl",
               "stringr", "dplyr", "metafor", "tidyr", "broom",
               "gt")
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


# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Prepare moderators ----------------------------------------------------
unique(meta$Article)

meta[, pub_year := str_extract(Article,
                               "\\d+")]

meta[, pub_year := as.numeric(pub_year)]

# >>> Effective N ---------------------------------------------------------
unique(meta[, .(effect_size_type, analysis_group)])


meta[analysis_group %in% c("Long-term abundance", 
                           "Before-after eradication reproduction",
                           "Before-after eradication reproduction") &
       effect_size_type %in% c("OR"),
      `:=` (n1 = Rat_Absent.Prey_Neg + Rat_Absent.Prey_Pos,
            n2 = Rat_Present.Prey_Neg + Rat_Present.Prey_Pos)]

meta[analysis_group %in% c("Long-term abundance", 
                           "Before-after eradication reproduction",
                           "Before-after eradication reproduction") &
       effect_size_type %in% c("SMD"),
     `:=` (n1 = Sample_size_overall_Rats_Absent,
           n2 = Sample_size_overall_Rats_Present)]


meta[analysis_group %in% c("Long-term abundance", 
                           "Before-after eradication reproduction",
                           "Before-after eradication reproduction"),
     effective_N := (4*n1*n2)/(n1+n2)]

meta[analysis_group %in% c("Long-term abundance", 
                           "Before-after eradication reproduction",
                           "Before-after eradication reproduction"), 
     correction := 1/effective_N]

meta[analysis_group %in% c("Long-term abundance", 
                           "Before-after eradication reproduction",
                           "Before-after eradication reproduction"), 
     bias_test := sqrt(1/effective_N)]

meta[effect_size_type == "ZCOR",
     correction := sampling_variance]
meta[effect_size_type == "ZCOR",
     bias_test := sqrt(sampling_variance)]

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Format IDs and variables for random effects --------------------------------
# meta[, Obs_ID := paste0("Obs_", seq(1:.N))]
#effect_size_id

meta[, Article_ID := paste0("Article_", 
                            as.numeric(as.factor(Article)))]


# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Create model guide ------------------------------------------------------

guide <- CJ(analysis_group = unique(meta[!is.na(analysis_group), ]$analysis_group),
            #exclude_converted_effect_sizes = c("yes", "no"),
            moderators = c("pub_year", "bias_test", "correction"))

# Create exclusion formula:
guide[, exclusion := paste0("analysis_group == ", "'", analysis_group, "'")]


# Create fixed effects formula:
guide[, formula := paste0("~ ", moderators)]

# Formulate random effects:
guide[, random_formula := "list(~1 | Article_ID / effect_size_id)"]

# Add an ID:
guide[, model_id := paste(analysis_group, moderators)]

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Run models --------------------------------------------------------------

guide
meta.final <- meta[!is.na(effect_size)]
sub.dat <- c()
model_list <- list()
tidy_models <- list()
i <- 4

for(i in 1:nrow(guide)){
  
  sub.dat <- meta.final[eval(parse(text = guide[i, ]$exclusion))]
  
    tryCatch(
      expr = {
        model_list[[i]] <- rma.mv(effect_size,
                    V = sampling_variance,
                    mods = as.formula(guide[i, ]$formula),
                    random = eval(parse(text = guide[i, ]$random)),
                    method = "ML",
                    test = "t",
                    data = sub.dat)
        tidy_models[[i]] <- model_list[[i]] %>%
          tidy() %>%
          mutate(lower_ci = model_list[[i]]$ci.lb,
                 upper_ci = model_list[[i]]$ci.ub) %>%
          bind_cols(guide[i, ]) %>% 
          setDT()
        
        names(model_list)[i] <- guide[i, ]$model_id
        
      },
      error = function(e){
        print(e)
      })
  cat(i, "/", nrow(guide), "\r")
}
i
guide



# Publication bias --------------------------------------------------------

tidy_models <- rbindlist(tidy_models)
tidy_models[moderators == "bias_test" & p.value < 0.05, ]
tidy_models[model_id == "Long-term abundance correction", ]


# Publication year --------------------------------------------------------
tidy_models[moderators == "pub_year" & p.value < 0.05, ]
tidy_models[p.value < 0.05, ]


# Table -------------------------------------------------------------------

model.gt <- tidy_models %>%
  mutate(moderators = case_when(moderators == "bias_test" ~ "Testing for bias",
                                moderators == "correction" ~ "Bias-corrected estimate",
                                moderators == "year" ~ "Year")) %>%
  select(moderators, term, estimate, std.error, statistic, p.value, 
         lower_ci, upper_ci,
         analysis_group) %>%
  group_by(analysis_group) %>%
  gt() %>%
  fmt_number(
    columns = everything(), 
    decimals = 2         
  )
class(model.gt)

print(model.gt)
gtsave(model.gt, "figures/meta_analysis/publication bias models.rtf")


