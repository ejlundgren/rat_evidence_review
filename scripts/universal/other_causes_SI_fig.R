
rm(list = ls())

library("groundhog")

date <- "2024-07-15"
ubuntu <- FALSE
if(ubuntu == TRUE){
  date <- "2023-04-15"
}
pcks <- c("data.table", "ggplot2", "tidyr", "readxl",
          "stringr", "dplyr")
groundhog.library(pcks, date)

theme_lundy <- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

dat <- fread("data/Working_Databases/Other_Causes.csv")
dat

dat[Primary_causes == "Habitat loss"]

names(dat)[grepl("habitat", names(dat), ignore.case = T)]
names(dat)[grepl("cat", names(dat), ignore.case = T)]
dat[Primary_causes == "Rats, cats, human hunting"]$Cat_introduced
dat[Primary_causes == "Rats, cats, human hunting"]

# 
dat[Rodent_only_primary == 1, No_other_cause := 1]
# dat[Unknown_cause == 1, ]

dat[Primary_causes == "Unknown", ]

dat[Primary_causes == "Unknown", Unknown_cause := 1]

dat.long <- melt(dat[, !c("assessmentId", "internalTaxonId")], 
                 id.vars = c("scientificName", "Common_name", "Synonyms_or_previous_lump",
                             "Rodent_attributed_IUCN", "Primary_causes", "Rodent_primary",
                             "Rodent_only_primary", "redlistCategory"),
                 variable.name = "Other_cause"
                 )

dat.long

dat.long <- dat.long[!is.na(value) & value == 1, ]

dat.long

dat[scientificName %in% setdiff(dat$scientificName, dat.long$scientificName)]

dat.long

dat.long[Other_cause == "Unknown_cause"]
dat[Unknown_cause == 1, ]


# >>> Load hypothesis classifications -------------------------------------
hyp.classes <- read_excel("data/Raw/Hypothesis-categories.xlsx")
hyp.classes
setDT(hyp.classes)

hyp.classes[, Hypothesis := gsub(" ", "_", Hypothesis)]
hyp.classes[, Hypothesis := gsub("-", "_", Hypothesis)]

setdiff(dat.long$Other_cause,
        hyp.classes$Hypothesis)
unique(hyp.classes$Hypothesis)

dat.long.mrg <- merge(dat.long,
                      hyp.classes,
                      all.x = T,
                      by.x = "Other_cause",
                      by.y = "Hypothesis")

dat.long.mrg[is.na(Category), ]$Other_cause

# >>> Number of species by cause ------------------------------------------

spp.sum <- dat.long.mrg[, .(number_spp = uniqueN(scientificName)),
                    by = .(Other_cause, Category)]

spp.sum

# Order this. 
sort(unique(spp.sum$Category))
spp.sum$Category <- factor(spp.sum$Category,
                           levels = rev(c("Human_activity", "Karma", "Introduced_predators",
                                      "Introduced_other", "Native_species", 
                                      "Disease_toxin")))
spp.sum[, cat_order := as.numeric(Category)]
spp.sum

setorder(spp.sum, cat_order, number_spp)
spp.sum[, order_seq := 1:.N]
#
sort(unique(spp.sum$Other_cause))
spp.sum[grepl("introduced", Other_cause), cause_clean := gsub("_introduced", "", Other_cause)]
spp.sum[grepl("native", Other_cause), cause_clean := gsub("_native", "", cause_clean)]
spp.sum[is.na(cause_clean), cause_clean := Other_cause]

sort(unique(spp.sum$cause_clean))

spp.sum[cause_clean == "Bird_other", cause_clean := "Other birds"]
spp.sum[cause_clean == "Predation_bird", cause_clean := "Bird predation"]
spp.sum[cause_clean == "Predation_mammal", cause_clean := "Mammal predation"]
spp.sum[cause_clean == "Donkey_horse", cause_clean := "Donkeys or horses"]
spp.sum[cause_clean == "Competition_bird", cause_clean := "Bird competition"]
spp.sum[cause_clean == "Competition_mammal", cause_clean := "Mammal competition"]
spp.sum[cause_clean == "Bird_other", cause_clean := "Other birds"]
spp.sum[cause_clean == "Dog_domestic", cause_clean := "Domestic dogs"]
spp.sum[cause_clean == "Herbivores_other_or_unspecified", cause_clean := "Other herbivores"]
spp.sum[cause_clean == "Human_hunting_harvesting", cause_clean := "Human hunting/harvesting"]
spp.sum[cause_clean == "Insects_invertebrates", cause_clean := "Insects/invertebrates"]
spp.sum[cause_clean == "Mona_Monkey", cause_clean := "Mona monkey"]
spp.sum[cause_clean == "Prey_base_currents_temp_weather", cause_clean := "Weather induced prey decline"]
spp.sum[cause_clean == "Seal_habitat_effects", cause_clean := "Impacts of seals on habitat"]
spp.sum[cause_clean == "Toxins_pesticides_pollution_oil_spills_radioactive_exposure_heavy_metals", cause_clean := "Toxins/pesticides/radiation/pollution"]
spp.sum[cause_clean == "Weather_extreme_unfavourable_sea_temp_cyclone", cause_clean := "Extreme weather/unfavorable sea temperature"]
spp.sum[cause_clean == "Deforestation_logging", cause_clean := "Deforestation/logging"]
spp.sum[cause_clean == "Civets", cause_clean := "Civet"]
spp.sum[cause_clean == "Rabbits", cause_clean := "Rabbit"]

sort(unique(spp.sum$cause_clean))

spp.sum[, cause_clean := gsub("_", " ", cause_clean)]
sort(unique(spp.sum$cause_clean))

labs <- unique(spp.sum[!is.na(Category), .(Other_cause, cause_clean)])
labs

labs.vec <- labs$cause_clean
names(labs.vec) <- labs$Other_cause

#
spp.sum$Other_cause <- factor(spp.sum$Other_cause,
                              levels = unique(spp.sum$Other_cause))

#
panel_A <- ggplot(data = spp.sum[!is.na(Category)], 
       aes(y = Other_cause, x = number_spp,fill = Category))+
  geom_col()+
  scale_y_discrete(labels = labs.vec)+
  scale_fill_discrete(labels = c("Human_activity" = "Human activity",
                                 "Karma" = "Environment",
                                 "Introduced_predators" = "Introduced predators",
                                 "Introduced_other" = "Other introduced",
                                 "Native_species" = "Native species",
                                 "Disease_toxin" = "Diseases/toxins"))+
  guides(name = NULL,
         fill = guide_legend(reverse = TRUE))+
  ylab(NULL)+
  xlab("Number of bird species")+
  theme_lundy+
  theme(legend.position = c(.75, .85))
panel_A

# >>> Number of hypotheses per species ------------------------------------
unique(dat.long.mrg$redlistCategory)
dat.long.mrg[, simple_status := ifelse(redlistCategory %in% c("Extinct", "Extinct in the Wild"),
                                       "Extinct", "Extant")]

#
hyp.sum <- dat.long.mrg[, .(number_hyp = uniqueN(.SD[Other_cause != "No_other_cause"]$Other_cause)),
                    by = .(scientificName, simple_status)]

hyp.sum

# Split this into two columns:
setorder(hyp.sum, simple_status,  -number_hyp)
nrow(hyp.sum) / 3
hyp.sum[1:113, col := 1]
hyp.sum[114:227, col := 2]
hyp.sum[228:nrow(hyp.sum), col := 3]

hyp.sum$scientificName <- factor(hyp.sum$scientificName,
                                 levels = rev(hyp.sum$scientificName))
hyp.sum[duplicated(scientificName)]

panel_B <- ggplot(data = hyp.sum, aes(y = scientificName, x = number_hyp,
                                      fill = simple_status))+
  geom_col()+
  facet_wrap(~col, scales = "free_y")+
  scale_fill_grey(name = NULL)+
  xlab("Number of alternative hypotheses")+
  ylab(NULL)+
  theme_lundy+
  theme(strip.text = element_blank(),
        legend.position = c(.95, .95))
panel_B

# >>> Patchwork -----------------------------------------------------------
library("patchwork")
panel_A + panel_B + plot_layout(widths = c(.25, .75))+
  plot_annotation(tag_levels = "A")
ggsave("figures/systematic_review/november_2024/other_causes.pdf", width = 16, height = 12)
