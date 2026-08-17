
# Prepare environment -----------------------------------------------------

rm(list = ls())

# library("pacmac")
pacman::p_load("data.table", "ggplot2", "tidyr", "readxl",
               "stringr", "dplyr", "patchwork", "writexl")

theme_lundy <- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Load attribution and studies  --------------------------------------------------
# data/Working_Databases/Systematic_review_attribution_and_evidence.csv
attrib <- fread("data/Working_Databases/Attribution.csv")

studies <- fread("data/Working_Databases/Studies.csv")

studies[scientificName == "Thalassarche chlororhynchos"]
attrib[scientificName == "Thalassarche chlororhynchos"]

# >>> Filter out some studies ---------------------------------------------
unique(studies$Evidence_category)


# studies <- studies[!Study_type %in% c("Diet overlap", "Spatial (artificial)",
#                                       "Spatiotemporal (artificial)", "Evolution")]

studies[Study_rodent_final == "",]

# >>> Merge together ------------------------------------------------------
attrib[duplicated(key)]  # should be 0 rows
# attrib[key == "Chasiempis ibidis_Rattus rattus"]
# studies[key == "Chasiempis ibidis_Rattus rattus"]
# attrib[key == "Chasiempis ibidis_Rattus rattus",
#        has_study := "Yes"]
# attrib <- unique(attrib)

#
studies[duplicated(key)] # should be > 0 rows

dat <- merge(attrib,
             studies[, !"scientificName"],
             by = "key",
             all.x = T,
             all.y = F)
#
dat

names(dat)
dat <- dat[, .(scientificName, Common_name,
               Original_rodent_attributed_IUCN, 
               Rodent_attribution_source,
               Reported_study_rodent,
               Study_notes,
               Article_secondary_same_data,
               redlistCategory,
               Synonyms_or_previous_lump, 
               Rodent_attributed_final, 
               Study_rodent_final,
               Rodent_primary, Rodent_only_primary,
               Article, Study_ID,
               Evidence_category, Evidence_effect, Evidence_method,
               Predation_type, Hypothesis_supported,
               Percent_nests_or_individuals_predated,
               Predator_and_prey_population_data, Experiment_control_site_or_spatial_variation,
               Sample_size_greater_than_1,
               Note_experiment_sample_size, Experiment_confounding_variables_included,
               Experiment_BACI, Study_location,
               Latitude, Longitude)]
dat

dat[Rodent_attributed_final != Study_rodent_final & !is.na(Study_rodent_final), ]
# Should be 0 rows.

dat[scientificName == "Thalassarche chlororhynchos"]
unique(dat$Evidence_effect)
unique(dat$Evidence_category)

# >>> Merge in traits/orders ----------------------------------------------
taxa <- fread("data/Working_Databases/Provisional_Trait_Taxonomy.csv")
taxa

unique(taxa$Order)
unique(taxa$Primary.Lifestyle)
taxa[duplicated(scientificName)]
taxa <- unique(taxa)
taxa[duplicated(scientificName)]

#
all(dat$scientificName %in% taxa$scientificName)
#

# Categorize nest sites. Since species are not exclusive let's do this 
# based on hierarchies...
taxa[NestSite_waterbody == 1, ]
taxa[NestSite_termite_ant == 1, ]

taxa[NestSite_tree == 1 | NestSite_nontree == 1,
      Nest_Site_Cat := "Vegetation"]

taxa[NestSite_waterbody == 1 |
       NestSite_termite_ant == 1 |
       NestSite_ground,
     Nest_Site_Cat := "Ground"]

taxa[NestSite_cliff_bank == 1,
     Nest_Site_Cat := "Cliff-face"]

taxa[NestSite_underground == 1,
     Nest_Site_Cat := "Burrow"]

taxa[is.na(Nest_Site_Cat), ]
taxa[is.na(Nest_Site_Cat) & (Mound == 1 | Parasite == 1), ]

#
dat.mrg <- merge(dat,
                 taxa[, .(scientificName, Order, Mass, Bird_Type_Coarse,
                          Nest_Site_Cat)],
                 by = "scientificName",
                 all.x = T)
dat.mrg
nrow(dat.mrg) == nrow(dat) # MUST BE TRUE

dat <- copy(dat.mrg)

fwrite(dat, "data/Working_Databases/Systematic_Review_Merged.csv")


# >>> Simplify study type? ------------------------------------------------

# Now get frequency of support...
dat[, value := ifelse(Hypothesis_supported == 0, -1, 1)]
dat

dat[, Rodent_display := Rodent_attributed_final]
unique(dat$Rodent_attributed_final)

dat[!Rodent_display %in% c("Rattus norvegicus", "Rattus rattus",
                                    "Rattus exulans", "Mus musculus"),
             Rodent_display := "Other rodents"]
#
unique(dat$Evidence_category)
# dat[, Study_type_simple := Study_type]
# dat[grepl("Lethal", Study_type), Study_type_simple := "Lethal control"]

# 
# dat[grepl("Spati", Study_type) | grepl("Temporal", Study_type), ##| grepl("Timing", Study_type), 
#     Study_type_simple := "Spatial or temporal correlation"]
# unique(dat$Study_type_simple)
# 
# # Even simpler:
# dat[, Study_type_simplest := Study_type_simple]
# dat[Study_type_simple %in% c("Timing", "Spatial or temporal correlation"),
#     Study_type_simplest := "Population"]

dat <- dat[Rodent_display != "Other rodents"]

# >>> Create a continuous quality column ----------------------------------
unique(dat[, .(Evidence_category, Evidence_effect, Evidence_method)])

dat[Evidence_category == "Predation", Evidence_quality := 1]
unique(dat[is.na(Evidence_quality), .(Evidence_category, Evidence_effect, Evidence_method)])
dat[Evidence_category == "Lethal program" &
      Evidence_effect == "Reproductive success", Evidence_quality := 2]

dat[Evidence_category == "Lethal program" &
      Evidence_effect == "Abundance", Evidence_quality := 3]


dat[Evidence_category == "Population" &
      Evidence_effect == "Reproductive success", Evidence_quality := 4]


dat[Evidence_category == "Population" &
      Evidence_effect == "Abundance", Evidence_quality := 5]
unique(dat[is.na(Evidence_quality), .(Evidence_category, Evidence_effect, Evidence_method)])

dat[scientificName == "Thalassarche chlororhynchos"]
dat[scientificName == "Thalassarche chlororhynchos"]$Rodent_primary

# Now add a highlight column:
unique(dat[, .(Predator_and_prey_population_data, Experiment_control_site_or_spatial_variation,
        Sample_size_greater_than_1, Experiment_confounding_variables_included)])
dat[Predator_and_prey_population_data == 1 &
      Experiment_control_site_or_spatial_variation == 1 &
      Sample_size_greater_than_1 == 1 &
      Experiment_confounding_variables_included == 1,]

dat[Predator_and_prey_population_data == 1 |
      Experiment_control_site_or_spatial_variation == 1 |
      Sample_size_greater_than_1 == 1 |
      Experiment_confounding_variables_included == 1,]

dat[Predator_and_prey_population_data == 1 &
      Experiment_control_site_or_spatial_variation == 1 &
      Sample_size_greater_than_1 == 1 &
      Experiment_confounding_variables_included == 1,
    Quality_Study_Highlight := "Highlight"]
dat[is.na(Quality_Study_Highlight), Quality_Study_Highlight := "No"]

dat[scientificName == "Pomarea nigra"]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------------

# Evidence frequency by species -------------------------------------------

# @@@ DEPREC Focus only on studies where rodent is primary -----------------------
# unique(dat$Rodent_primary)

# dat <- dat[Rodent_primary == 1, ]
dat[, synth_evidence := ifelse(Evidence_category == "Population",
                             paste(Evidence_category, Predator_and_prey_population_data, Quality_Study_Highlight),
                             Evidence_category)]
unique(dat$synth_evidence)

studies.freq <- dat[, .(n_studies = sum(value, na.rm = T),
                        n_articles = uniqueN(Article)),
                        by = .(scientificName, Bird_Type_Coarse, 
                               Evidence_category, Evidence_effect,
                               synth_evidence,
                               Evidence_quality, Quality_Study_Highlight,
                               Rodent_display,
                               Hypothesis_supported)]

studies.freq[scientificName == "Pomarea nigra"]


studies.freq[, n_articles := ifelse(Hypothesis_supported == 0, -n_articles, n_articles)]
studies.freq
studies.freq[is.na(n_articles), n_articles := 0]

unique(studies.freq$Hypothesis_supported)

# studies.freq[n_articles != evidence, ]
range(studies.freq$n_articles)

# >>> Supplementary plots ----------------------------------------------------------------
length(unique(studies.freq[Rodent_display == "Mus musculus"]$scientificName))

studies.freq
# studies.freq <- studies.freq[]
#
#
unique(studies.freq$synth_evidence)

pal <- c("Population 1 Highlight" = "dodgerblue2",
          "Population 1 No" = "dodgerblue2",
          "Population 0 No" = "dodgerblue4",
          "Lethal program" = "indianred4",
          "Predation" = "grey50")

col_pal <- c("Population 1 Highlight" = "gold",
             "Population 1 No" = "transparent",
             "Population 0 No" = "transparent",
             "Lethal program" = "transparent",
             "Predation" = "transparent")

labs <- c("Population 1 Highlight" = "Population with all qualities",
             "Population 1 No" = "Population with data",
             "Population 0 No" = "Population without data",
             "Lethal program" = "Lethal program",
             "Predation" = "Predation")
#
# Hmmmm. Testing....Perhaps rectangles behind axis text? Or just squares by name?
studies.freq[is.na(synth_evidence) & n_articles > 0, ]


ggplot()+
  geom_vline(xintercept = 0)+
  geom_col(data = studies.freq[!is.na(n_articles), ], aes(x = n_studies, y = scientificName, 
                                    fill = synth_evidence,
                                    color = synth_evidence),
           linewidth = .5)+
  # geom_point(data = studies.freq, aes(x = -5, y = scientificName,
  #                color = Bird_Type_Coarse), shape = 15)+
  ylab("Bird species")+
  xlab("Number of studies")+
  scale_color_manual(name = NULL,
                     values = col_pal,
                     labels = labs,
                     na.translate = F)+
  scale_fill_manual(name = NULL,
                     values = pal,
                    labels = labs,
                    na.translate = F)+
  facet_wrap(~Rodent_display, scales = "free_y")+
  theme_lundy+
  theme(axis.ticks = element_blank())

# Let's export individually
# studies.freq[is.na(col), col := 2]
# Let's start with rats. Unfortunately, think I need to split dataset up to do the plotting...
# Or we write a function huh?

unique(studies.freq$synth_evidence)

studies.freq[is.na(Evidence_quality), Evidence_quality := 0]

studies.freq[, synth_evidence := gsub("0", "no_data", synth_evidence)]
studies.freq[, synth_evidence := gsub("1", "data", synth_evidence)]

unique(studies.freq$synth_evidence)
studies.freq$synth_evidence <- factor(studies.freq$synth_evidence,
                                      levels = (c("Predation", 
                                                     "Lethal program",
                                                     "Population no_data No", 
                                                     "Population data No",
                                                     "Population data Highlight")))

pal <- c("Population data Highlight" = "dodgerblue2",
         "Population data No" = "dodgerblue2",
         "Population no_data No" = "dodgerblue4",
         "Lethal program" = "indianred4",
         "Predation" = "grey50")

col_pal <- c("Population data Highlight" = "gold",
             "Population data No" = "transparent",
             "Population no_data No" = "transparent",
             "Lethal program" = "transparent",
             "Predation" = "transparent")

labs <- c("Population data Highlight" = "Population with all qualities",
          "Population data No" = "Population with data",
          "Population no_data No" = "Population without data",
          "Lethal program" = "Lethal program",
          "Predation" = "Predation")


# Sort order of plotting:


studies.freq[, synth_evidence_direction := paste(synth_evidence, Hypothesis_supported)]

order_tab <- dcast(studies.freq,
                   scientificName + Rodent_display ~ synth_evidence_direction,
                   value.var = c("n_studies"),
                   fun.aggregate = sum)
order_tab
setnames(order_tab, names(order_tab), gsub(" ", "_", names(order_tab)))
order_tab

# View(order_tab[Rodent_display == "Rattus rattus"])
# Try to order by quality (and quantity within quality level...)
setorder(order_tab, 
         Rodent_display, 
         -Population_data_Highlight_1,
         -Population_data_No_1, 
         -Population_no_data_No_1,
         -Lethal_program_1,
         -Predation_1,
         #
         Population_data_Highlight_0,
         Population_data_No_0, 
         Population_no_data_No_0,
         Lethal_program_0,
         Predation_0,
         
         NA_NA)

# View(order_tab[Rodent_display == "Rattus rattus"])
order_tab[, order_seq := 1:.N]
order_tab

order_tab[, key := paste(scientificName, Rodent_display)]
studies.freq[, key := paste(scientificName, Rodent_display)]

studies.freq.mrg <- merge(studies.freq,
                          order_tab[, .(order_seq, key)],
                          all.x = T,
                          by = "key")
studies.freq.mrg

unique(studies.freq.mrg$synth_evidence)

# --------------- Rattus rattus --------------------------------------!
# Sorting into the correct order is very challenging...

rat.freq <- studies.freq.mrg[Rodent_display == "Rattus rattus", ]

rat.freq$scientificName <- factor(rat.freq$scientificName,
                                  levels = order_tab[Rodent_display == "Rattus rattus", ]$scientificName)
# setorder(rat.freq, order_bins, -total_positive_studies, -total_negative_studies)
# nrow(rat.freq)

setorder(rat.freq, order_seq)
# divide into columns:
N <- nrow(rat.freq)
rat.freq[1:round(N/3), col := 1]
rat.freq[(ceiling(N/3)+1):(round(N/3)*2) , col := 2]
rat.freq[(round(N/3)*2):nrow(rat.freq) , col := 3]
rat.freq[, .(n = .N), by = col]


library("scales")
# View(rat.freq[, .(scientificName, synth_evidence_direction, order_seq, col)])

# setorder(rat.freq, order)
#
# calculate x limits
xlim <- rat.freq[, .(n = sum(n_articles)),
                 by = .(scientificName, Hypothesis_supported)]
xlim <- range(xlim$n)
###
col1 <- ggplot()+
  geom_vline(xintercept = 0)+
  geom_col(data = rat.freq[!is.na(n_articles) &
                             col == 1, ], 
           aes(x = n_studies, y = scientificName, 
                fill = synth_evidence,
                color = synth_evidence),
           linewidth = .75)+
  annotate(geom = "text", x = -4, y = 71, label = "Not in support",
           size = 3)+
  annotate(geom = "text", x = 2.5, y = 71, label = "In support",
           size = 3)+
  coord_cartesian(clip = 'off')+
  scale_x_continuous(limits = xlim, breaks = breaks_pretty())+
  scale_y_discrete(limits = rev)+
  # geom_point(data = studies.freq, aes(x = -5, y = scientificName,
  #                color = Bird_Type_Coarse), shape = 15)+
  ylab("Bird species")+
  ggtitle("Black rat")+
  xlab(NULL)+
  # scale_x_continuous()+
  scale_color_manual(name = NULL,
                     values = col_pal,
                     labels = labs,
                     na.translate = F)+
  scale_fill_manual(name = NULL,
                    values = pal,
                    labels = labs,
                    na.translate = F)+
  # facet_wrap(~col, scales = "free_y")+
  theme_lundy+
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 20),
        strip.text = element_blank(),
        legend.position = "bottom")
col1


col2 <- ggplot()+
  geom_vline(xintercept = 0)+
  geom_col(data = rat.freq[!is.na(n_articles)  &
                             col == 2, ], 
           aes(x = n_studies, y = scientificName, 
               fill = synth_evidence,
               color = synth_evidence),
           linewidth = .75)+
  # geom_point(data = studies.freq, aes(x = -5, y = scientificName,
  #                color = Bird_Type_Coarse), shape = 15)+
  ylab(NULL)+
  xlab("Number of studies")+
  scale_color_manual(name = NULL,
                     values = col_pal,
                     labels = labs,
                     na.translate = F)+
  scale_fill_manual(name = NULL,
                    values = pal,
                    labels = labs,
                    na.translate = F)+
  scale_x_continuous(limits = xlim, breaks = breaks_pretty())+
  scale_y_discrete(limits = rev)+
  # facet_wrap(~col, scales = "free_y")+
  theme_lundy+
  theme(axis.ticks = element_blank(),
        # plot.title = element_text(hjust = 0.5, size = 20),
        strip.text = element_blank(),
        legend.position = "bottom")
col2
# almost there...gahhh


#
col3 <- ggplot()+
  geom_vline(xintercept = 0)+
  geom_col(data = rat.freq[!is.na(n_articles) &
                             col == 3, ], 
           aes(x = n_studies, y = scientificName, 
               fill = synth_evidence,
               color = synth_evidence),
           linewidth = .75)+
  # geom_point(data = studies.freq, aes(x = -5, y = scientificName,
  #                color = Bird_Type_Coarse), shape = 15)+
  ylab(NULL)+
  xlab(NULL)+
  scale_x_continuous(limits = xlim, breaks = breaks_pretty())+
  scale_color_manual(name = NULL,
                     values = col_pal,
                     labels = labs,
                     na.translate = F)+
  scale_fill_manual(name = NULL,
                    values = pal,
                    labels = labs,
                    na.translate = F)+
  # scale_y_discrete(limits = rev)+
  # facet_wrap(~col, scales = "free_y")+
  theme_lundy+
  theme(axis.ticks = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom")
col3

col1 + col2 + col3 +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

#
ggsave("figures/systematic_review/november_2024/black rats all species.png", width = 14, height = 14)
ggsave("figures/systematic_review/november_2024/black rats all species.pdf", width = 14, height = 14)

# --------------- Rattus norvegicus --------------------------------------!
#
rat.freq <- studies.freq.mrg[Rodent_display == "Rattus norvegicus", ]

rat.freq$scientificName <- factor(rat.freq$scientificName,
                                  levels = order_tab[Rodent_display == "Rattus norvegicus", ]$scientificName)
# setorder(rat.freq, order_bins, -total_positive_studies, -total_negative_studies)
# nrow(rat.freq)

setorder(rat.freq, order_seq)

# divide into columns:
n <- nrow(rat.freq)
rat.freq[1:round(n/3), col := 1]
rat.freq[round(n/3):(ceiling(n/3) * 2), col := 2]
rat.freq[(ceiling(n/3) * 2):n , col := 3]

# rat.freq[(round(326/3)*2):(floor(326/3)*3) , col := 3]
rat.freq[, .(n = .N), by = col]

xlim <- rat.freq[, .(n = sum(n_articles)),
                 by = .(scientificName, Hypothesis_supported)]
xlim <- range(xlim$n)
#

col1 <- ggplot()+
  geom_vline(xintercept = 0)+
  geom_col(data = rat.freq[!is.na(n_articles) &
                             col == 1, ], 
           aes(x = n_studies, y = scientificName, 
               fill = synth_evidence,
               color = synth_evidence),
           linewidth = .75)+
  annotate(geom = "text", x = -1.5, y = 50, label = "Not in support",
           size = 3)+
  annotate(geom = "text", x = 2, y = 50, label = "In support",
           size = 3)+
  coord_cartesian(clip = 'off')+
  scale_x_continuous(limits = xlim)+
  scale_y_discrete(limits = rev)+
  # geom_point(data = studies.freq, aes(x = -5, y = scientificName,
  #                color = Bird_Type_Coarse), shape = 15)+
  ylab("Bird species")+
  ggtitle("Brown rat")+
  xlab(NULL)+
  scale_color_manual(name = NULL,
                     values = col_pal,
                     labels = labs,
                     na.translate = F)+
  scale_fill_manual(name = NULL,
                    values = pal,
                    labels = labs,
                    na.translate = F)+
  # facet_wrap(~col, scales = "free_y")+
  theme_lundy+
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 20),
        strip.text = element_blank(),
        legend.position = "bottom")
col1


col2 <- ggplot()+
  geom_vline(xintercept = 0)+
  geom_col(data = rat.freq[!is.na(n_articles)  &
                             col == 2, ], 
           aes(x = n_studies, y = scientificName, 
               fill = synth_evidence,
               color = synth_evidence),
           linewidth = .75)+
  # geom_point(data = studies.freq, aes(x = -5, y = scientificName,
  #                color = Bird_Type_Coarse), shape = 15)+
  ylab(NULL)+
  xlab("Number of studies")+
  scale_color_manual(name = NULL,
                     values = col_pal,
                     labels = labs,
                     na.translate = F)+
  scale_fill_manual(name = NULL,
                    values = pal,
                    labels = labs,
                    na.translate = F)+
  scale_x_continuous(limits = xlim)+
  scale_y_discrete(limits = rev)+
  # facet_wrap(~col, scales = "free_y")+
  theme_lundy+
  theme(axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_blank(),
        legend.position = "bottom")
col2
# almost there...gahhh


#
col3 <- ggplot()+
  geom_vline(xintercept = 0)+
  geom_col(data = rat.freq[!is.na(n_articles) &
                             col == 3, ], 
           aes(x = n_studies, y = scientificName, 
               fill = synth_evidence,
               color = synth_evidence),
           linewidth = .75)+
  # geom_point(data = studies.freq, aes(x = -5, y = scientificName,
  #                color = Bird_Type_Coarse), shape = 15)+
  ylab(NULL)+
  xlab(NULL)+
  scale_x_continuous(limits = xlim)+
  scale_color_manual(name = NULL,
                     values = col_pal,
                     labels = labs,
                     na.translate = F)+
  scale_fill_manual(name = NULL,
                    values = pal,
                    labels = labs,
                    na.translate = F)+
  # scale_y_discrete(limits = rev)+
  # facet_wrap(~col, scales = "free_y")+
  theme_lundy+
  theme(axis.ticks = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom")
col3

col1 + col2 + col3 +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("figures/systematic_review/november_2024/brown rats all species.png", width = 14, height = 14)
ggsave("figures/systematic_review/november_2024/brown rats all species.pdf", width = 14, height = 14)

# --------------- Rattus exulans --------------------------------------!
#
rat.freq <- studies.freq.mrg[Rodent_display == "Rattus exulans", ]

rat.freq$scientificName <- factor(rat.freq$scientificName,
                                  levels = order_tab[Rodent_display == "Rattus exulans", ]$scientificName)
# setorder(rat.freq, order_bins, -total_positive_studies, -total_negative_studies)
# nrow(rat.freq)

setorder(rat.freq, order_seq)

# divide into columns:
n <- nrow(rat.freq)
rat.freq[1:round(n/2), col := 1]
rat.freq[(ceiling(n/2)+1):n , col := 2]
# rat.freq[(round(326/3)*2):(floor(326/3)*3) , col := 3]
rat.freq[, .(n = .N), by = col]
#
xlim <- rat.freq[, .(n = sum(n_articles)),
                 by = .(scientificName, Hypothesis_supported)]
xlim <- range(xlim$n)

col1 <- ggplot()+
  geom_vline(xintercept = 0)+
  geom_col(data = rat.freq[!is.na(n_articles) &
                             col == 1, ], 
           aes(x = n_studies, y = scientificName, 
               fill = synth_evidence,
               color = synth_evidence),
           linewidth = .75)+
  annotate(geom = "text", x = -1.5, y = 52, label = "Not in support",
           size = 3)+
  annotate(geom = "text", x = 2, y = 52, label = "In support",
           size = 3)+
  coord_cartesian(clip = 'off')+
  scale_y_discrete(limits = rev)+
  scale_x_continuous(limits = xlim)+
  # geom_point(data = studies.freq, aes(x = -5, y = scientificName,
  #                color = Bird_Type_Coarse), shape = 15)+
  ylab("Bird species")+
  ggtitle("Pacific rats")+
  xlab(NULL)+
  scale_color_manual(name = NULL,
                     values = col_pal,
                     labels = labs,
                     na.translate = F)+
  scale_fill_manual(name = NULL,
                    values = pal,
                    labels = labs,
                    na.translate = F)+
  # facet_wrap(~col, scales = "free_y")+
  theme_lundy+
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 20),
        strip.text = element_blank(),
        legend.position = "bottom")
col1


col2 <- ggplot()+
  geom_vline(xintercept = 0)+
  geom_col(data = rat.freq[!is.na(n_articles)  &
                             col == 2, ], 
           aes(x = n_studies, y = scientificName, 
               fill = synth_evidence,
               color = synth_evidence),
           linewidth = .75)+
  # geom_point(data = studies.freq, aes(x = -5, y = scientificName,
  #                color = Bird_Type_Coarse), shape = 15)+
  ylab(NULL)+
  xlab("Number of studies")+
  scale_x_continuous(limits = xlim)+
  scale_color_manual(name = NULL,
                     values = col_pal,
                     labels = labs,
                     na.translate = F)+
  scale_fill_manual(name = NULL,
                    values = pal,
                    labels = labs,
                    na.translate = F)+
  scale_y_discrete(limits = rev)+
  # facet_wrap(~col, scales = "free_y")+
  theme_lundy+
  theme(axis.ticks = element_blank(),
        strip.text = element_blank(),
        legend.position = "bottom")
col2
# almost there...gahhh

col1 + col2  +
  plot_layout(ncol = 3, guides = "collect") &
  theme(legend.position = "bottom")

ggsave("figures/systematic_review/november_2024/pacific rats all species.png", width = 12, height = 13)
ggsave("figures/systematic_review/november_2024/pacific rats all species.pdf", width = 12, height = 13)


# --------------- Mus musculus --------------------------------------!
#

rat.freq <- studies.freq.mrg[Rodent_display == "Mus musculus", ]

rat.freq$scientificName <- factor(rat.freq$scientificName,
                                  levels = order_tab[Rodent_display == "Mus musculus", ]$scientificName)
# setorder(rat.freq, order_bins, -total_positive_studies, -total_negative_studies)
# nrow(rat.freq)

setorder(rat.freq, order_seq)

# Rat plot
ggplot()+
  geom_vline(xintercept = 0)+
  geom_col(data = rat.freq[!is.na(n_articles), ], 
           aes(x = n_studies, y = scientificName, 
               fill = synth_evidence,
               color = synth_evidence),
           linewidth = .75)+
  annotate(geom = "text", x = -1.5, y = 34, label = "Not in support",
           size = 3)+
  annotate(geom = "text", x = 2, y = 34, label = "In support",
           size = 3)+
  # geom_point(data = studies.freq, aes(x = -5, y = scientificName,
  #                color = Bird_Type_Coarse), shape = 15)+
  ylab("Bird species")+
  xlab("Number of studies")+
  scale_y_discrete(limits = rev)+
  scale_color_manual(name = NULL,
                     values = col_pal,
                     labels = labs,
                     na.translate = F)+
  scale_fill_manual(name = NULL,
                    values = pal,
                    labels = labs,
                    na.translate = F)+
  ggtitle("House mouse")+
  coord_cartesian(clip = "off")+
  # facet_wrap(~col, scales = "free_y")+
  theme_lundy+
  theme(axis.ticks = element_blank(),
        strip.text = element_blank(),
        plot.title = element_text(size = 15),
        legend.position = "bottom")

ggsave("figures/systematic_review/november_2024/mouse all species.png", width = 12, height = 12)
ggsave("figures/systematic_review/november_2024/mouse all species.pdf", width = 12, height = 12)

# ~~~~~~~~~~~~~~~ ---------------------------------------------------------
# Summaries ---------------------------------------------------------------
# Filling in Summary stats.xlsx.
dat[, Evidence_method := gsub(" ", "_", Evidence_method)]
dat[, Evidence_effect := gsub(" ", "_", Evidence_effect)]
dat[, Evidence_category := gsub(" ", "_", Evidence_category)]
dat[is.na(Evidence_method), Evidence_method := "NONE"]
dat[is.na(Evidence_effect), Evidence_effect := "NONE"]
dat[is.na(Evidence_category), Evidence_category := "NONE"]

dat[Article == "", ]
dat[is.na(Article), ]
dat[is.na(Study_ID), ]

# >>> Total number of articles/studies: ------------------------------------------------------!
sort(unique(dat$Article))
length(unique(dat[!is.na(Article), ]$Article))
length(unique(dat[!is.na(Article) & Rodent_primary == 1, ]$Article))

length(unique(dat[!is.na(Article), ]$Study_ID))
length(unique(dat[!is.na(Article) & Rodent_primary == 1, ]$Study_ID))

# Mean articles/studies per bird species
x <- dat[, .(n_articles = uniqueN(.SD[!is.na(Article)]$Article),
              n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(scientificName)][, .(m_articles = mean(n_articles), 
                                m_studies = mean(n_studies))]
x

x <- dat[Rodent_primary == 1, .(n_articles = uniqueN(.SD[!is.na(Article)]$Article),
             n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
         by = .(scientificName)][, .(m_articles = mean(n_articles), 
                                     m_studies = mean(n_studies))]
x
#
# >>> Studies dedicated to: ------------------------------------------------------
length(unique(dat[Study_rodent_final == "Rattus rattus", ]$Study_ID))
length(unique(dat[Study_rodent_final == "Rattus rattus" &
                    Rodent_primary == 1, ]$Study_ID))

length(unique(dat[Study_rodent_final == "Rattus norvegicus", ]$Study_ID))
length(unique(dat[Study_rodent_final == "Rattus norvegicus" &
                    Rodent_primary == 1, ]$Study_ID))

length(unique(dat[Study_rodent_final == "Rattus exulans", ]$Study_ID))
length(unique(dat[Study_rodent_final == "Rattus exulans" &
                    Rodent_primary == 1, ]$Study_ID))


length(unique(dat[Study_rodent_final == "Mus musculus", ]$Study_ID))
length(unique(dat[Study_rodent_final == "Mus musculus" &
                    Rodent_primary == 1, ]$Study_ID))

# >>> Studies are about: ------------------------------------------------------
# ----------------- By evidence_method ------------------------------------------------------!
unique(dat$Evidence_category)
unique(dat$Evidence_effect)
unique(dat$Evidence_method)
dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_method)]
#
dat[Rodent_primary == 1, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_method)]
#
#
studies.freq <- dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Evidence_method)]
studies.freq

dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary ~ #+
                  # Predator_and_prey_population_data + Quality_Study_Highlight 
                 Evidence_method,
                  value.var = "n_studies",
                  fun.aggregate = sum)
dat.wide
#
mean(dat.wide$Predation)
mean(dat.wide[Rodent_primary == 1, ]$Predation)

# ----------------- By evidence_category ------------------------------------------------------!

dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category)]
#
dat[Rodent_primary == 1, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category)]
#
#
studies.freq <- dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Evidence_category)]
studies.freq
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary ~ #+
                    # Predator_and_prey_population_data + Quality_Study_Highlight 
                    Evidence_category,
                  value.var = "n_studies",
                  fun.aggregate = sum)
dat.wide

mean(dat.wide$Lethal_program)
mean(dat.wide[Rodent_primary == 1, ]$Lethal_program)

mean(dat.wide$Population)
mean(dat.wide[Rodent_primary == 1, ]$Population)

# --------- evidence_category * effect -----------------------!
dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Evidence_effect)]
#
dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Evidence_effect, Rodent_primary)][Rodent_primary == 1, ]
#
#
studies.freq <- dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Evidence_effect,
                           Evidence_category)]
studies.freq
#
studies.freq[, cat_effect := paste(Evidence_category, Evidence_effect, sep = "_")]

#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary ~ #+
                    # Predator_and_prey_population_data + Quality_Study_Highlight 
                    cat_effect,
                  value.var = "n_studies",
                  fun.aggregate = sum,
                  fill = 0)
dat.wide
#
dat.wide

#
mean(dat.wide$Lethal_program_Reproductive_success)
mean(dat.wide[Rodent_primary == 1, ]$Lethal_program_Reproductive_success)
#

mean(dat.wide$Lethal_program_Abundance)
mean(dat.wide[Rodent_primary == 1, ]$Lethal_program_Abundance)

#
mean(dat.wide$Population_Reproductive_success)
mean(dat.wide[Rodent_primary == 1, ]$Population_Reproductive_success)

#
mean(dat.wide$Population_Abundance)
mean(dat.wide[Rodent_primary == 1, ]$Population_Abundance)


# --------- evidence_category + data -----------------------!
dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, 
           Predator_and_prey_population_data)][Predator_and_prey_population_data == 1, ]

#
dat[Rodent_primary == 1, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Predator_and_prey_population_data)][Predator_and_prey_population_data == 1, ]
#
#
studies.freq <- dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Evidence_category,
                           Predator_and_prey_population_data)]
studies.freq

#
studies.freq[, cat_effect := paste(Evidence_category, 
                                   Predator_and_prey_population_data,
                                   sep = "_")]

#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary ~ #+
                    # Predator_and_prey_population_data + Quality_Study_Highlight 
                    cat_effect,
                  value.var = "n_studies",
                  fun.aggregate = sum,
                  fill = 0)
dat.wide
#
dat.wide

#
mean(dat.wide$Population_1)
mean(dat.wide[Rodent_primary == 1, ]$Population_1)

# --------- evidence_category * effect + data -----------------------!
dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Evidence_effect, 
           Predator_and_prey_population_data)][Evidence_category == "Population" &
                                                 Predator_and_prey_population_data == 1]
#
dat[Rodent_primary == 1, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Evidence_effect, 
           Predator_and_prey_population_data)][Evidence_category == "Population" &
                          Predator_and_prey_population_data == 1 , ]



studies.freq <- dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Evidence_effect,
                           Predator_and_prey_population_data,
                           Evidence_category)]
studies.freq

#
studies.freq[, cat_effect := paste(Evidence_category, Evidence_effect, 
                                   Predator_and_prey_population_data,
                                   sep = "_")]

#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary ~ #+
                    # Predator_and_prey_population_data + Quality_Study_Highlight 
                    cat_effect,
                  value.var = "n_studies",
                  fun.aggregate = sum,
                  fill = 0)
dat.wide
#
dat.wide

#
mean(dat.wide$Population_Reproductive_success_1)
mean(dat.wide[Rodent_primary == 1, ]$Population_Reproductive_success_1)

#
mean(dat.wide$Population_Abundance_1)
mean(dat.wide[Rodent_primary == 1, ]$Population_Abundance_1)


# --------- evidence_category * effect + quality -----------------------!
#' [Next time can get rid of this section and add the appropriate columns together below...probably at least]
dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, 
           Quality_Study_Highlight,
           Predator_and_prey_population_data)][Evidence_category == "Population" &
                                                 Predator_and_prey_population_data == 1 &
                                                 Quality_Study_Highlight == "Highlight", ]
#
dat[Rodent_primary == 1, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, 
           Quality_Study_Highlight,
           Predator_and_prey_population_data)][Evidence_category == "Population" &
                                                 Predator_and_prey_population_data == 1 &
                                                 Quality_Study_Highlight == "Highlight", ]

studies.freq <- dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Quality_Study_Highlight,
                           Predator_and_prey_population_data,
                           Evidence_category)]
studies.freq

#
studies.freq[, cat_effect := paste(Evidence_category, Quality_Study_Highlight, 
                                   Predator_and_prey_population_data,
                                   sep = "_")]

#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary ~ #+
                    # Predator_and_prey_population_data + Quality_Study_Highlight 
                    cat_effect,
                  value.var = "n_studies",
                  fun.aggregate = sum,
                  fill = 0)
dat.wide

#
mean(dat.wide$Population_Highlight_1)
mean(dat.wide[Rodent_primary == 1, ]$Population_Highlight_1)

# --------- evidence_category * effect + data + quality -----------------------!
dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Evidence_effect, 
           Quality_Study_Highlight,
           Predator_and_prey_population_data)][Evidence_category == "Population" &
                                                 Predator_and_prey_population_data == 1 &
                                                 Quality_Study_Highlight == "Highlight", ]
#
dat[Rodent_primary == 1, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Evidence_effect, 
           Quality_Study_Highlight,
           Predator_and_prey_population_data)][Evidence_category == "Population" &
                                                 Predator_and_prey_population_data == 1 &
                                                 Quality_Study_Highlight == "Highlight", ]


studies.freq <- dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Evidence_effect,
                           Predator_and_prey_population_data,
                           Evidence_category,
                           Quality_Study_Highlight)]
studies.freq

#
studies.freq[, cat_effect := paste(Evidence_category, Evidence_effect, 
                                   Predator_and_prey_population_data,
                                   Quality_Study_Highlight,
                                   sep = "_")]

#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary ~ #+
                    # Predator_and_prey_population_data + Quality_Study_Highlight 
                    cat_effect,
                  value.var = "n_studies",
                  fun.aggregate = sum,
                  fill = 0)
dat.wide
#
dat.wide

#
mean(dat.wide$Population_Reproductive_success_1_Highlight)
mean(dat.wide[Rodent_primary == 1, ]$Population_Reproductive_success_1_Highlight)

#
mean(dat.wide$Population_Abundance_1_Highlight)
mean(dat.wide[Rodent_primary == 1, ]$Population_Abundance_1_Highlight)
# >>> Studies NOT in support ------------------------------------------------------
#' * This section needs to be % of studies of birds that have studies in that category *

# ------------------------- Evidence category * Support --------------------------!
dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Hypothesis_supported)][Hypothesis_supported == 0, ]

#
dat[Rodent_primary == 1, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Hypothesis_supported)][Hypothesis_supported == 0, ]


studies.freq <- dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Evidence_category, Hypothesis_supported)]
studies.freq

#
studies.freq

studies.freq[, cat_effect := paste(Evidence_category, Hypothesis_supported,
                                   sep = "_")]

#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary ~ #+
                    # Predator_and_prey_population_data + Quality_Study_Highlight 
                    cat_effect,
                  fun.aggregate = sum,
                  value.var = "n_studies",
                  fill = 0)
dat.wide


#
dat.wide[, total := Predation_1 + Predation_0]
dat.wide[total > 0 , ]
mean(dat.wide[total > 0 , ]$Predation_0, na.rm = T)
mean(dat.wide[total > 0 &
                Rodent_primary == 1, ]$Predation_0, na.rm = T)

#
dat.wide[, total := Lethal_program_0 + Lethal_program_1]
mean(dat.wide[total > 0, ]$Lethal_program_0, na.rm = T)
mean(dat.wide[total > 0 &
                Rodent_primary == 1, ]$Lethal_program_0, na.rm = T)

#
dat.wide[, total := Population_0 + Population_1]
mean(dat.wide[total > 0, ]$Population_0, na.rm = T)
mean(dat.wide[total > 0 &
                Rodent_primary == 1, ]$Population_0, na.rm = T)

# ------------------------- Evidence category * Support * data --------------------------!
dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Hypothesis_supported, 
           Predator_and_prey_population_data)][Hypothesis_supported == 0 &
                                                 Predator_and_prey_population_data == 1, ]

#
dat[Rodent_primary == 1, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Hypothesis_supported, 
           Predator_and_prey_population_data)][Hypothesis_supported == 0 &
                                                 Predator_and_prey_population_data == 1, ]


studies.freq <- dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Evidence_category, Hypothesis_supported,
                           Predator_and_prey_population_data)]
studies.freq

#
studies.freq[, cat_effect := paste(Evidence_category, Hypothesis_supported,
                                   Predator_and_prey_population_data,
                                   sep = "_")]

#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary ~ #+
                    # Predator_and_prey_population_data + Quality_Study_Highlight 
                    cat_effect,
                  value.var = "n_studies",
                  fun.aggregate = sum,
                  fill = 0)
dat.wide
#
dat.wide[, total := Population_0_1 + Population_1_1]
mean(dat.wide[total > 0, ]$Population_0_1, na.rm = T)
mean(dat.wide[total > 0 &
                Rodent_primary == 1, ]$Population_0_1, na.rm = T)


# ------------------------- Evidence category * Support * ALL QUALITIES --------------------------!

dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Hypothesis_supported, 
           Quality_Study_Highlight)][Hypothesis_supported == 0 &
                                       Quality_Study_Highlight == "Highlight", ]

#
dat[Rodent_primary == 1, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Hypothesis_supported, 
           Quality_Study_Highlight)][Hypothesis_supported == 0 &
                                       Quality_Study_Highlight == "Highlight", ]


studies.freq <- dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Evidence_category, Hypothesis_supported,
                           Quality_Study_Highlight)]
studies.freq

#
studies.freq[, cat_effect := paste(Evidence_category, Hypothesis_supported,
                                   Quality_Study_Highlight,
                                   sep = "_")]

#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary ~ #+
                    # Predator_and_prey_population_data + Quality_Study_Highlight 
                    cat_effect,
                  value.var = "n_studies",
                  fun.aggregate = sum,
                  fill = 0)
dat.wide
#
dat.wide[, total := Population_0_Highlight + Population_1_Highlight]
mean(dat.wide[total > 0, ]$Population_0_Highlight, na.rm = T)
mean(dat.wide[total > 0 &
                Rodent_primary == 1, ]$Population_0_Highlight, na.rm = T)

# ------------------------- Evidence category * Support * ALL QUALITIES * Abundance --------------------------!
#
dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category, Evidence_effect,
           Hypothesis_supported, 
           Quality_Study_Highlight)][Hypothesis_supported == 0 &
                                       Quality_Study_Highlight == "Highlight" &
                                       Evidence_effect == "Abundance", ]

#
dat[Rodent_primary == 1, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
    by = .(Evidence_category,Evidence_effect,
           Hypothesis_supported, 
           Quality_Study_Highlight)][Hypothesis_supported == 0 &
                                       Quality_Study_Highlight == "Highlight" &
                                       Evidence_effect == "Abundance", ]


studies.freq <- dat[, .(n_studies = uniqueN(.SD[!is.na(Study_ID)]$Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Evidence_category, Evidence_effect,
                           Hypothesis_supported,
                           Quality_Study_Highlight)]
studies.freq

#
studies.freq[, cat_effect := paste(Evidence_category, Evidence_effect,
                                   Hypothesis_supported,
                                   Quality_Study_Highlight,
                                   sep = "_")]

#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary ~ #+
                    # Predator_and_prey_population_data + Quality_Study_Highlight 
                    cat_effect,
                  value.var = "n_studies",
                  fun.aggregate = sum,
                  fill = 0)
dat.wide
#
#
dat.wide[, total := Population_Abundance_0_Highlight + Population_Abundance_1_Highlight  ]
mean(dat.wide[total > 0, ]$Population_Abundance_0_Highlight, na.rm = T)
mean(dat.wide[total > 0 &
                Rodent_primary == 1, ]$Population_Abundance_0_Highlight, na.rm = T)

# >>> Birds with no studies ------------------------------------------------------
dat
#
dat[, any_study := ifelse(nrow(.SD[!is.na(Study_ID), ]) > 0,
                          "yes", "no"),
    by = .(scientificName)]
dat[, .(no_birds = uniqueN(scientificName)),
    by = .(any_study)]

dat[any_study == "no" & !is.na(Study_ID), ] # Must be 0 rows

dat[, .(no_birds = uniqueN(scientificName)),
    by = .(any_study, Rodent_attributed_final)][any_study == "no"]

# Rodent primary:
dat[Rodent_primary == 1, .(no_birds = uniqueN(scientificName)),
    by = .(any_study)]

dat[Rodent_primary == 1, .(no_birds = uniqueN(scientificName)),
    by = .(any_study, Rodent_attributed_final)][any_study == "no"]

# >>> Birds with ONLY predation data in support ----------------------------------
#' [NEW APPROACH: add a column 'matches_criteria' and use that to summarize]

# God what an annoying thing to calculate hahahaha
#This is dumb. Going to do a cast summary.
unique(dat$Evidence_category)
dat[, Evidence_category := gsub(" ", "_", Evidence_category)]

dat[, cat_support := paste(Evidence_category, Hypothesis_supported, sep = "_")]
unique(dat$cat_support)

#
studies.combined.freq <- dat[, .(n_studies = uniqueN(Study_ID)),
                    by = .(scientificName, Rodent_primary,
                            #Rodent_attributed_final,
                           # Predator_and_prey_population_data,
                           # Quality_Study_Highlight,
                           cat_support)]
studies.combined.freq


#
dat.combined.wide <- dcast(data = studies.combined.freq,
                  scientificName + Rodent_primary #+ 
                 # Rodent_attributed_final #+
                    # Predator_and_prey_population_data + Quality_Study_Highlight 
                  ~ cat_support,
                  value.var = "n_studies",
                  fun.aggregate = sum)
dat.combined.wide

dat.combined.wide[, matches_criteria := ifelse(Predation_1 > 0 &
                                        Lethal_program_1 == 0 &
                                        Population_1 == 0,
                                      "yes", "no")]


length(unique(dat.combined.wide[matches_criteria == "yes"]$scientificName))
length(unique(dat.combined.wide[matches_criteria == "yes" &
                         Rodent_primary == 1]$scientificName))
# Now get totals for birds with some kind of evidence...
dat.combined.wide[, total_studies := Lethal_program_0 + Lethal_program_1 + Population_0 + Population_1 +
                    Predation_0 + Predation_1]
dat.combined.wide
length(unique(dat.combined.wide[total_studies > 0]$scientificName))
length(unique(dat.combined.wide[total_studies > 0  &
                                  Rodent_primary == 1]$scientificName))

# Now by rodent species:
studies.freq <- dat[, .(n_studies = uniqueN(Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Rodent_attributed_final,
                           # Predator_and_prey_population_data,
                           # Quality_Study_Highlight,
                           cat_support)]
studies.freq

#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary + 
                   Rodent_attributed_final #+
                  # Predator_and_prey_population_data + Quality_Study_Highlight 
                  ~ cat_support,
                  value.var = "n_studies",
                  fun.aggregate = sum)
dat.wide

#

dat.wide[, matches_criteria := ifelse(Predation_1 > 0 &
                                        Lethal_program_1 == 0 &
                                        Population_1 == 0,
                                      "yes", "no")]
dat.wide[, total_studies := Lethal_program_0 + Lethal_program_1 + Population_0 + Population_1 +
                    Predation_0 + Predation_1]
dat.wide


#
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_attributed_final == "Rattus rattus", ]$scientificName))
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus rattus"]$scientificName))
length(unique(dat.wide[total_studies > 0 &
                         Rodent_attributed_final == "Rattus rattus", ]$scientificName))
length(unique(dat.wide[total_studies > 0 &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus rattus"]$scientificName))

#
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_attributed_final == "Rattus norvegicus", ]$scientificName))
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus norvegicus"]$scientificName))
length(unique(dat.wide[total_studies > 0 &
                         Rodent_attributed_final == "Rattus norvegicus", ]$scientificName))
length(unique(dat.wide[total_studies > 0 &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus norvegicus"]$scientificName))

#
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_attributed_final == "Rattus exulans", ]$scientificName))
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus exulans"]$scientificName))
length(unique(dat.wide[total_studies > 0 &
                         Rodent_attributed_final == "Rattus exulans", ]$scientificName))
length(unique(dat.wide[total_studies > 0 &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus exulans"]$scientificName))


#
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_attributed_final == "Mus musculus", ]$scientificName))
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Mus musculus"]$scientificName))
length(unique(dat.wide[total_studies > 0 &
                         Rodent_attributed_final == "Mus musculus", ]$scientificName))
length(unique(dat.wide[total_studies > 0 &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Mus musculus"]$scientificName))

# >>> Birds with population data in support ----------------------------------
# Based on testing above, we don't need the combined.wide here...But since I 
dat.combined.wide
dat.combined.wide[, matches_criteria := ifelse(Population_1 > 0,
                                      "yes", "no")]
dat.combined.wide[, has_population_study := ifelse(Population_0 + Population_1 > 0,
                                          "yes", "no")]

#
dat.wide[, matches_criteria := ifelse(Population_1 > 0,
                                      "yes", "no")]
dat.wide[, has_population_study := ifelse(Population_0 + Population_1 > 0,
                                          "yes", "no")]

# Overall:
length(unique(dat.combined.wide[matches_criteria == "yes" ]$scientificName))
length(unique(dat.combined.wide[matches_criteria == "yes"  &
                         Rodent_primary == 1]$scientificName))
mean(dat.combined.wide[has_population_study == "yes", ]$Population_1)
mean(dat.combined.wide[has_population_study == "yes" &
                Rodent_primary == 1, ]$Population_1)

#
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_attributed_final == "Rattus rattus", ]$scientificName))
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus rattus"]$scientificName))
mean(dat.wide[has_population_study == "yes" &
                Rodent_attributed_final == "Rattus rattus", ]$Population_1)
mean(dat.wide[has_population_study == "yes" &
                Rodent_primary == 1 &
                Rodent_attributed_final == "Rattus rattus", ]$Population_1)

#
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_attributed_final == "Rattus norvegicus", ]$scientificName))
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus norvegicus"]$scientificName))
mean(dat.wide[has_population_study == "yes" &
                Rodent_attributed_final == "Rattus norvegicus", ]$Population_1)
mean(dat.wide[has_population_study == "yes" &
                Rodent_primary == 1 &
                Rodent_attributed_final == "Rattus norvegicus", ]$Population_1)

#
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_attributed_final == "Rattus exulans", ]$scientificName))
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus exulans"]$scientificName))
mean(dat.wide[has_population_study == "yes" &
                Rodent_attributed_final == "Rattus exulans", ]$Population_1)
mean(dat.wide[has_population_study == "yes" &
                Rodent_primary == 1 &
                Rodent_attributed_final == "Rattus exulans", ]$Population_1)

#
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_attributed_final == "Mus musculus", ]$scientificName))
length(unique(dat.wide[matches_criteria == "yes" &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Mus musculus"]$scientificName))
mean(dat.wide[has_population_study == "yes" &
                Rodent_attributed_final == "Mus musculus", ]$Population_1)
mean(dat.wide[has_population_study == "yes" &
                Rodent_primary == 1 &
                Rodent_attributed_final == "Mus musculus", ]$Population_1)

# >>> Birds with Population studies that provide rodent and bird data ----------------
#

dat[, cat_support := paste(Evidence_category,
                           Hypothesis_supported,
                           ifelse(Predator_and_prey_population_data == 1,
                                  "data", "no_data"), 
                           sep = "_")]
unique(dat$cat_support)
length(unique(dat[cat_support == "Population_1_data"]$scientificName))
unique(dat[cat_support == "Population_1_data"]$scientificName)
#' [why is this producing 18, but down below I get 6???]
#' 
unique(dat[cat_support == "Population_1_data", .(scientificName, 
                                                 Hypothesis_supported,
                                                 Evidence_category,
                                                 Predator_and_prey_population_data)])

studies.freq <- dat[, .(n_studies = uniqueN(Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Rodent_attributed_final,
                           Predator_and_prey_population_data,
                           # Quality_Study_Highlight,
                           cat_support)]
studies.freq

#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary + 
                    Rodent_attributed_final#+ Quality_Study_Highlight
                  ~ cat_support,
                  value.var = "n_studies",
                  fun.aggregate = sum)
dat.wide
#

length(unique(dat.wide[Population_1_data > 0 , ]$scientificName))

# Overall:
dat.combined.wide <- dat.wide[, .(Population_0_data = sum(Population_0_data),
                                  Population_0_no_data = sum(Population_0_no_data),
                                  Population_1_data = sum(Population_1_data),
                                  Population_1_no_data = sum(Population_1_no_data)),
                              by = .(Rodent_primary, scientificName)]
dat.combined.wide
length(unique(dat.combined.wide[Population_1_data > 0 , ]$scientificName))
length(unique(dat.combined.wide[Population_1_data > 0 &
                         Rodent_primary == 1]$scientificName))
dat.combined.wide[, has_study := Population_0_data + Population_1_data]
mean(dat.combined.wide[has_study > 0, ]$Population_1_data)
mean(dat.combined.wide[has_study > 0 & Rodent_primary == 1, ]$Population_1_data)

# Now by rodents: 
dat.wide[, has_study := Population_0_data + Population_1_data]
length(unique(dat.wide[Population_1_data > 0 &
                         Rodent_attributed_final == "Rattus rattus", ]$scientificName))
length(unique(dat.wide[Population_1_data > 0 &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus rattus"]$scientificName))
mean(dat.wide[has_study > 0 &
                Rodent_attributed_final == "Rattus rattus", ]$Population_1_data)
mean(dat.wide[has_study > 0 & Rodent_primary == 1 &
                Rodent_attributed_final == "Rattus rattus", ]$Population_1_data)

#
length(unique(dat.wide[Population_1_data > 0 &
                         Rodent_attributed_final == "Rattus norvegicus", ]$scientificName))
length(unique(dat.wide[Population_1_data > 0 &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus norvegicus"]$scientificName))
mean(dat.wide[has_study > 0 &
                Rodent_attributed_final == "Rattus norvegicus", ]$Population_1_data)
mean(dat.wide[has_study > 0 & Rodent_primary == 1 &
                Rodent_attributed_final == "Rattus norvegicus", ]$Population_1_data)

#
length(unique(dat.wide[Population_1_data > 0 &
                         Rodent_attributed_final == "Rattus exulans", ]$scientificName))
length(unique(dat.wide[Population_1_data > 0 &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus exulans"]$scientificName))
mean(dat.wide[has_study > 0 &
                Rodent_attributed_final == "Rattus exulans", ]$Population_1_data)
mean(dat.wide[has_study > 0 & Rodent_primary == 1 &
                Rodent_attributed_final == "Rattus exulans", ]$Population_1_data)


#
length(unique(dat.wide[Population_1_data > 0 &
                         Rodent_attributed_final == "Mus musculus", ]$scientificName))
length(unique(dat.wide[Population_1_data > 0 &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Mus musculus"]$scientificName))
mean(dat.wide[has_study > 0 &
                Rodent_attributed_final == "Mus musculus", ]$Population_1_data)
mean(dat.wide[has_study > 0 & Rodent_primary == 1 &
                Rodent_attributed_final == "Mus musculus", ]$Population_1_data)


# >>> Birds with population studies that meet all qualities ---------------
dat[Quality_Study_Highlight == "Highlight"]

dat[, cat_support := paste(Evidence_category, Hypothesis_supported,
                           Quality_Study_Highlight, sep = "_")]
unique(dat$cat_support)
studies.freq <- dat[, .(n_studies = uniqueN(Study_ID)),
                    by = .(scientificName, Rodent_primary,
                           Rodent_attributed_final,
                           #Predator_and_prey_population_data,
                           Quality_Study_Highlight,
                           cat_support)]
studies.freq
#
dat.wide <- dcast(data = studies.freq,
                  scientificName + Rodent_primary + 
                    Rodent_attributed_final
                  ~ cat_support,
                  value.var = "n_studies",
                  fun.aggregate = sum)
dat.wide

# All birds:
names(dat.wide)
dat.combined.wide <- dat.wide[, .(Population_0_Highlight = sum(Population_0_Highlight),
                                  Population_0_No = sum(Population_0_No),
                                  Population_1_Highlight = sum(Population_1_Highlight),
                                  Population_1_No = sum(Population_1_No)),
                              by = .(Rodent_primary, scientificName)]
dat.combined.wide[, has_study := Population_0_Highlight + Population_1_Highlight]

length(unique(dat.combined.wide[Population_1_Highlight > 0 , ]$scientificName))
length(unique(dat.combined.wide[Population_1_Highlight > 0 &
                         Rodent_primary == 1]$scientificName))
mean(dat.combined.wide[has_study > 0, ]$Population_1_Highlight)
mean(dat.combined.wide[has_study > 0 &
                         Rodent_primary == 1, ]$Population_1_Highlight)

#
dat.wide[, has_study := Population_0_Highlight + Population_1_Highlight]
length(unique(dat.wide[Population_1_Highlight > 0  &
                         Rodent_attributed_final == "Rattus rattus", ]$scientificName))
length(unique(dat.wide[Population_1_Highlight > 0  &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus rattus"]$scientificName))
mean(dat.wide[has_study > 0 &
                Rodent_attributed_final == "Rattus rattus", ]$Population_1_Highlight)
mean(dat.wide[has_study > 0 &
               Rodent_primary == 1 &
                Rodent_attributed_final == "Rattus rattus", ]$Population_1_Highlight)

#
length(unique(dat.wide[Population_1_Highlight > 0  &
                         Rodent_attributed_final == "Rattus norvegicus", ]$scientificName))
length(unique(dat.wide[Population_1_Highlight > 0  &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus norvegicus"]$scientificName))
mean(dat.wide[has_study > 0 &
                Rodent_attributed_final == "Rattus norvegicus", ]$Population_1_Highlight)
mean(dat.wide[has_study > 0 &
                Rodent_primary == 1 &
                Rodent_attributed_final == "Rattus norvegicus", ]$Population_1_Highlight)

#
length(unique(dat.wide[Population_1_Highlight > 0  &
                         Rodent_attributed_final == "Rattus exulans", ]$scientificName))
length(unique(dat.wide[Population_1_Highlight > 0  &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Rattus exulans"]$scientificName))
mean(dat.wide[has_study > 0 &
                Rodent_attributed_final == "Rattus exulans", ]$Population_1_Highlight)
mean(dat.wide[has_study > 0 &
                Rodent_primary == 1 &
                Rodent_attributed_final == "Rattus exulans", ]$Population_1_Highlight)


#
length(unique(dat.wide[Population_1_Highlight > 0  &
                         Rodent_attributed_final == "Mus musculus", ]$scientificName))
length(unique(dat.wide[Population_1_Highlight > 0  &
                         Rodent_primary == 1 &
                         Rodent_attributed_final == "Mus musculus"]$scientificName))
mean(dat.wide[has_study > 0 &
                Rodent_attributed_final == "Mus musculus", ]$Population_1_Highlight)
mean(dat.wide[has_study > 0 &
                Rodent_primary == 1 &
                Rodent_attributed_final == "Mus musculus", ]$Population_1_Highlight)

# ~~~~~~~~~~~~~~~ ---------------------------------------------------------

# Summary plot ------------------------------------------------------------
# Unfortunately, plotting the summaries is not going to be super straightforward,
# since each needs to be calculated in a different way.
# We want:
#     A. number of bird species per study type
#     B. Number of studies by type in support vs not in support

# >>> Number of bird species per study type -------------------------------

# There's gotta be a way to do this...
dat2 <- copy(dat)
dat2[, Rodent_attributed_final := "All rodents"]
nrow(dat2)
dat2 <- unique(dat2)
nrow(dat2)
dat2$Rodent_display <- "All rodents"
dat2 <- unique(dat2)
nrow(dat2)

#
combined.dat <- rbind(dat,
                      dat2)

combined.dat[scientificName == "Pomarea nigra"]

#
#
#
#

unique(combined.dat$Evidence_category)
combined.dat[is.na(Study_ID), synth_col := "None"]
combined.dat[Evidence_category == "Predation" &
               Hypothesis_supported == 1, synth_col := "Predation_in_support"]
#
combined.dat[Evidence_category == "Lethal_program" &
               Hypothesis_supported == 1, synth_col := "Lethal_program_in_support"]
#
combined.dat[Evidence_category == "Population" &
               Predator_and_prey_population_data == 0 &
               Hypothesis_supported == 1, synth_col := "Population_without_data_in_support"]
#
combined.dat[Evidence_category == "Population" &
               Predator_and_prey_population_data == 1 &
               Hypothesis_supported == 1, synth_col := "Population_with_data_in_support"]
unique(combined.dat[, .(synth_col, Hypothesis_supported)])

combined.dat[is.na(synth_col)]$Evidence_category

#
combined.dat[Evidence_category == "Population" &
               Quality_Study_Highlight == "Highlight" &
               Hypothesis_supported == 1, 
             synth_col := "Population_with_all_qualities_in_support"]
unique(combined.dat[, .(synth_col, Hypothesis_supported)])
combined.dat[scientificName == "Pomarea nigra"]
#

combined.dat[Hypothesis_supported == 0, synth_col := "Not_in_support"]

combined.dat
unique(combined.dat[, .(synth_col, Hypothesis_supported)])

bird_freq <- combined.dat[, .(no_studies = .N),
                          by = .(Rodent_attributed_final, scientificName, 
                                 synth_col)]
bird_freq


bird_freq[scientificName == "Pomarea nigra"]


bird_freq.wide <- dcast(bird_freq,
                        scientificName + Rodent_attributed_final ~ synth_col,
                        value.var = "no_studies",
                        fill = 0)
bird_freq.wide
bird_freq.wide[scientificName == "Pomarea nigra"]

# Now create a summary column:
bird_freq.wide[Lethal_program_in_support == 0 &
                 None > 0 &
                 Not_in_support == 0 &
                 Population_with_all_qualities_in_support == 0 &
                 Population_with_data_in_support == 0 &
                 Population_without_data_in_support == 0 &
                 Predation_in_support == 0,
               synth_col := "No studies"]

#
bird_freq.wide[Lethal_program_in_support == 0 &
                 None >= 0 &
                 Not_in_support >= 0 &
                 Population_with_all_qualities_in_support == 0 &
                 Population_with_data_in_support == 0 &
                 Population_without_data_in_support == 0 &
                 Predation_in_support > 0,
               synth_col := "Only predation in support"]
#
bird_freq.wide[Lethal_program_in_support > 0 &
                 None >= 0 &
                 Not_in_support >= 0 &
                 Population_with_all_qualities_in_support == 0 &
                 Population_with_data_in_support == 0 &
                 Population_without_data_in_support == 0 &
                 Predation_in_support >= 0,
               synth_col := "Lethal program in support"]

#
bird_freq.wide[Lethal_program_in_support >= 0 &
                 None >= 0 &
                 Not_in_support >= 0 &
                 Population_with_all_qualities_in_support == 0 &
                 Population_with_data_in_support == 0 &
                 Population_without_data_in_support > 0 &
                 Predation_in_support >= 0,
               synth_col := "Population study without data in support"]

#
bird_freq.wide[Lethal_program_in_support >= 0 &
                 None >= 0 &
                 Not_in_support >= 0 &
                 Population_with_all_qualities_in_support == 0 &
                 Population_with_data_in_support > 0 &
                 Population_without_data_in_support >= 0 &
                 Predation_in_support >= 0,
               synth_col := "Population study with data in support"]

#
bird_freq.wide[Lethal_program_in_support >= 0 &
                 None >= 0 &
                 Not_in_support >= 0 &
                 Population_with_all_qualities_in_support > 0 &
                 Population_without_data_in_support >= 0 &
                 Predation_in_support >= 0,
               synth_col := "Population study with all qualities in support"]

#
bird_freq.wide[Lethal_program_in_support == 0 &
                 None >= 0 &
                 Not_in_support == 0 &
                 Population_with_all_qualities_in_support == 0 &
                 Population_with_data_in_support == 0 &
                 Population_without_data_in_support == 0 &
                 Predation_in_support == 0,
               synth_col := "No studies found"]


bird_freq.wide[Lethal_program_in_support == 0 &
                 None >= 0 &
                 Not_in_support > 0 &
                 Population_with_all_qualities_in_support == 0 &
                 Population_with_data_in_support == 0 &
                 Population_without_data_in_support == 0 &
                 Predation_in_support == 0,
               synth_col := "All studies are not in support"]

bird_freq.wide
bird_freq.wide[synth_col == "All studies are not in support"]

#
(unique(bird_freq.wide[is.na(synth_col), .(Lethal_program_in_support, None,
                               Not_in_support, Population_with_data_in_support,
                               Population_without_data_in_support,
                               Predation_in_support, synth_col)]))

View(unique(bird_freq.wide[, .(Lethal_program_in_support, None,
                          Not_in_support, Population_with_data_in_support,
                          Population_without_data_in_support,
                          Predation_in_support, synth_col)]))
fwrite(bird_freq.wide[, .(scientificName, Rodent_attributed_final, Lethal_program_in_support, None,
                          Not_in_support, Population_with_data_in_support,
                          Population_without_data_in_support, Population_with_all_qualities_in_support,
                          Predation_in_support, synth_col)],
       "builds/systematic_review/frequency_table_for_checking.csv")

bird_freq.wide[scientificName == "Pomarea nigra"]


bird_freq.wide[None > 0 , ]

bird_freq.wide[is.na(synth_col)]

# ----------- Number of bird species by synthetic column -------------------!
#But before, let's add the single endemic species category + RedList status
single_island <- read_xlsx("data/Working_Databases/single_island_endemic_classification.xlsx")
setDT(single_island)
table(single_island$single_island_endemic)

spp_status <- unique(dat[,.(scientificName, redlistCategory)])
table(spp_status$redlistCategory) #53 extinct species and one species missing the RedList status
spp_status[redlistCategory == "", scientificName] #"Porphyrio paepae"
spp_status[scientificName == "Porphyrio paepae", redlistCategory := "Extinct"]

spp_status <- merge(spp_status, single_island[,.(scientificName, single_island_endemic)],
                    by = "scientificName")

bird_freq.wide <- merge(bird_freq.wide, spp_status,
                        by = "scientificName")

bird_freq <- bird_freq.wide[, .(no_species = uniqueN(scientificName)),
                                by = .(synth_col, Rodent_attributed_final)]
bird_freq

bird_freq[Rodent_attributed_final == "All rodents" & synth_col == "All studies are not in support"]
30 / length(unique(bird_freq.wide$scientificName))


bird_freq[Rodent_attributed_final == "All rodents" & synth_col == "No studies found"]

sum_bird_freq <- bird_freq[, Proportion :=  (no_species / sum(no_species)) * 100, by = Rodent_attributed_final]
setorder(sum_bird_freq, Rodent_attributed_final)
setnames(sum_bird_freq, "Rodent_attributed_final", "Rodent attributed")
setnames(sum_bird_freq, "synth_col", "Evidence")
setnames(sum_bird_freq, "no_species", "Bird species")

unique(dat$Evidence_effect)
nrow(dat[Hypothesis_supported == 0 & Evidence_effect == "Abundance"]) / nrow(dat[Evidence_effect == "Abundance"])
nrow(dat[Hypothesis_supported == 0 & Evidence_effect == "Reproductive_success"]) / nrow(dat[Evidence_effect == "Reproductive_success"])

#Copy of bird freq for sys review for all extant species
bird_freq_2 <- bird_freq.wide[, .(no_species = uniqueN(scientificName)),
                            by = .(synth_col, Rodent_attributed_final, redlistCategory)]

bird_freq_2 <- bird_freq_2[redlistCategory != "Extinct", ]

sum_bird_freq_2 <- bird_freq_2[,.(
  `Bird species`  = sum(no_species)
), by = .(`Rodent attributed` = Rodent_attributed_final, Evidence = synth_col)]

sum_bird_freq_2[, Proportion :=  (`Bird species` / sum(`Bird species`)) * 100, by = `Rodent attributed`]
setorder(sum_bird_freq_2, `Rodent attributed`)

#Copy of bird freq for sys review all extant multi-island species
bird_freq_3 <- bird_freq.wide[, .(no_species = uniqueN(scientificName)),
                              by = .(synth_col, Rodent_attributed_final, redlistCategory, single_island_endemic)]

bird_freq_3 <- bird_freq_3[redlistCategory != "Extinct" & single_island_endemic != "yes", ]

sum_bird_freq_3 <- bird_freq_3[,.(
  `Bird species`  = sum(no_species)
), by = .(`Rodent attributed` = Rodent_attributed_final, Evidence = synth_col)]

sum_bird_freq_3[, Proportion :=  (`Bird species` / sum(`Bird species`)) * 100, by = `Rodent attributed`]
setorder(sum_bird_freq_3, `Rodent attributed`)

#Save sum for sys rev all extant & all extant multi-island species
sys_rev_filt <- list(
  "sys_rev_all_included" = sum_bird_freq,
  "sys_rev_all_extant"  = sum_bird_freq_2,
  "sys_rev_all_extant_multi-island"  = sum_bird_freq_3)

writexl::write_xlsx(sys_rev_filt, "builds/systematic_review/sys_rev_filt.xlsx")

# ----------- Plot ------------------- !

bird_freq$Rodent_attributed_final <- factor(bird_freq$Rodent_attributed_final,
                                            levels = c("All rodents",
                                                       "Rattus rattus", 
                                                       "Rattus norvegicus",
                                                       "Rattus exulans",
                                                       "Mus musculus"))

unique(bird_freq$synth_col)
bird_freq$synth_col <- factor(bird_freq$synth_col,
                                     levels = c("Population study with all qualities in support",
                                                "Population study with data in support",
                                                "Population study without data in support",
                                                "Lethal program in support",
                                                "Only predation in support",
                                                "All studies are not in support",
                                                "No studies found"))

labs <- c("Population study with all qualities in support" = "Population in support (with data & qualities)",
          "Population study with data in support" = "Population in support (with data)",
          "Population study without data in support" = "Population in support (without data)",
          "Lethal program in support" = "Lethal program in support (best)",
          "Only predation in support" = "Predation in support (only)",
          "All studies are not in support" = "All studies not in support",
          "No studies found" = "No studies found")
#
# pal1 <- c("Population studies with data in support" = "#8ACB88",
#           "Other studies in support" = "#FFBF46",
#           "Only that predation occurs" = "#648381",
#           "No studies" = "#020100")
# 
pal2 <- c("Population study with all qualities in support" = "dodgerblue2",
          "Population study with data in support" = "dodgerblue2",
          "Population study without data in support" = "dodgerblue4",
          "Lethal program in support" = "indianred4",
          "Only predation in support" = "grey50",
          "All studies are not in support" = "grey20",
          "No studies found" = "black")

col_pal <- c("Population study with all qualities in support" = "gold",
          "Population study with data in support" = "transparent",
          "Population study without data in support" = "transparent",
          "Lethal program in support" = "transparent",
          "Only predation in support" = "transparent",
          "All studies are not in support" = "transparent",
          "No studies found" = "transparent")
bird_freq

bird_freq[synth_col == "Only predation in support"]
bird_freq[duplicated(paste(synth_col, Rodent_attributed_final))]

#
p_bird_freq <- ggplot(data = bird_freq[!is.na(synth_col), ], 
       aes(x = Rodent_attributed_final,
           y = no_species, fill = synth_col, color = synth_col))+
  geom_col(position = "stack", linewidth = .75)+
  ylab("Number of bird species")+
  xlab(NULL)+
  scale_x_discrete(labels = c(c("All rodents" = "All rodents",
                                "Rattus rattus" = "Black rats", 
                                "Rattus norvegicus" = "Brown rats",
                                "Rattus exulans" = "Pacific rats",
                                "Mus musculus" = "House mouse")))+
  scale_fill_manual(name = NULL,
                    values = pal2,
                    labels = labs)+
  scale_color_manual(name = NULL,
                    values = col_pal,
                    labels = labs)+
  theme_lundy
p_bird_freq

# >>> Total number of studies ---------------------------------------------
#Same as before, first add the single-island category
dat[is.na(redlistCategory), scientificName]
dat[scientificName == "Porphyrio paepae", redlistCategory := "Extinct"]

dat <- merge(dat, single_island[,.(scientificName, single_island_endemic)],
             by = "scientificName")

unique(dat[, .(value, Hypothesis_supported)])
unique(dat$Evidence_category)
unique(dat$Evidence_effect)

dat[, Evidence_category_effect := ifelse(!Evidence_effect %in% c("Predation", "NONE"),
                                          paste(Evidence_category, Evidence_effect),
                                          Evidence_category)]
unique(dat$Evidence_category_effect)

dat[, Evidence_category_effect_data := ifelse(Evidence_category == "Population",
                                              paste(Evidence_category_effect, Predator_and_prey_population_data),
                                              Evidence_category_effect)]

study_freq <- dat[, .(number_studies = sum(value)),
                  by = .(Evidence_category_effect, 
                         Evidence_category_effect_data,
                         Hypothesis_supported,
                         Quality_Study_Highlight)]
study_freq

#Copy of bird freq for sys review for all extant species
study_freq_2 <- dat[redlistCategory != "Extinct", .(number_studies = sum(value)),
                  by = .(Evidence_category_effect, 
                         Evidence_category_effect_data,
                         Hypothesis_supported,
                         Quality_Study_Highlight)]

#Copy of bird freq for sys review all extant multi-island species
study_freq_3 <- dat[redlistCategory != "Extinct" & single_island_endemic != "yes", .(number_studies = sum(value)),
                  by = .(Evidence_category_effect, 
                         Evidence_category_effect_data,
                         Hypothesis_supported,
                         Quality_Study_Highlight)]


# --------------------- Plot --------------------------------------------!

unique(study_freq$Evidence_category_effect)
study_freq$Evidence_category_effect <- factor(study_freq$Evidence_category_effect ,
                                                   levels = c("NONE", "Predation", 
                                                              "Lethal_program Reproductive_success",
                                                              "Lethal_program Abundance",
                                                              "Population Reproductive_success",
                                                              "Population Abundance"))
unique(study_freq$Evidence_category_effect_data)
study_freq$Evidence_category_effect_data <- factor(study_freq$Evidence_category_effect_data ,
                                       levels = c("NONE", "Predation", 
                                                  "Lethal_program Reproductive_success",
                                                  "Lethal_program Abundance",
                                                  "Population Reproductive_success 1",
                                                  "Population Reproductive_success 0",
                                                  "Population Abundance 1",
                                                  "Population Abundance 0"))
study_freq$Quality_Study_Highlight <- factor(study_freq$Quality_Study_Highlight,
                                             levels = rev(c("No", "Highlight")))

p_study_freq <- ggplot(data = study_freq[Evidence_category_effect != "NONE"],
                       aes(x = number_studies,
                           y = Evidence_category_effect,
                           fill = Evidence_category_effect_data,
                           color = Quality_Study_Highlight,
                           linewidth = Quality_Study_Highlight))+
  geom_col(linewidth = 1, width = .5)+
  geom_vline(xintercept = 0)+
  scale_color_manual(values = c("Highlight" = "gold",
                                "No" = "transparent"))+
  annotate(geom = "text", x = -50, y = 5.4,
           label = "Not in support", size = 3.3, hjust = 0,
           color = "grey30")+
  annotate(geom = "text", x = 90, y = 5.4,
           label = "In support", size = 3.3, hjust = 0,
           color = "grey30")+
  # scale_fill_manual(values = c("Predation" = "#7D7E75", 
  #                              "Lethal_program" = "indianred4",
  #                              "Population" = "dodgerblue3"))+
  scale_fill_manual(values = c("Predation" = "grey50",
                              "Lethal_program Reproductive_success" = "indianred4",
                              "Lethal_program Abundance" = "indianred4",
                              "Population Reproductive_success 0" = "dodgerblue4",
                              "Population Abundance 0" = "dodgerblue4",
                              "Population Reproductive_success 1" = "dodgerblue2",
                              "Population Abundance 1" = "dodgerblue2"))+ #
  scale_y_discrete(labels = c("Predation" = "Predation",
                              "Lethal_program Reproductive_success" = "Lethal program\n(reproductive success)",
                              "Lethal_program Abundance" = "Lethal program\n(abundance)",
                              "Population Reproductive_success" = "Population\n(reproductive success)",
                              "Population Abundance" = "Population\n(abundance)"))+
  guides(fill = "none", color = "none")+
  ylab(NULL)+
  xlab("Number of studies")+
  theme_lundy+
  theme(axis.text.y = element_text(hjust=0),
        axis.ticks.y = element_blank())

p_study_freq

# >>> Patchwork -----------------------------------------------------------
library("patchwork")

p_bird_freq + theme(legend.position=c(.75,.75)) +#theme(legend.position = "bottom") + 
  p_study_freq + plot_layout(ncol = 2, widths = c(0.6, 0.4))+
  plot_annotation(tag_levels = "A")

ggsave("figures/systematic_review/november_2024/main_text_evidence_summaries.png",
       width = 12, height = 8, dpi = 500)

ggsave("figures/systematic_review/november_2024/main_text_evidence_summaries.pdf",
       width = 12, height = 8, dpi = 500)

ggsave("figures/systematic_review/sys_rew_all_extant.pdf", plot = last_plot(), device = cairo_pdf, 
       width = 11.46, height = 8.30)

ggsave("figures/systematic_review/sys_rew_all_extant_multi-island.pdf", plot = last_plot(), device = cairo_pdf, 
       width = 11.46, height = 8.30)

# ~~~~~~~~~~~~~~~ ---------------------------------------------------------
# Number of studies by type -----------------------------------------------

dat
unique(dat[, .(Evidence_category, Evidence_effect, Evidence_method,
               Predator_and_prey_population_data, Experiment_control_site_or_spatial_variation,
               Sample_size_greater_than_1,
               Experiment_confounding_variables_included,
               Experiment_BACI)])

# >>> Create synthetic column ---------------------------------------------
# Try doing this with 2 levels...
unique(dat[Evidence_category == "Population", 
           .(Evidence_category, Evidence_effect, Evidence_method,
               Predator_and_prey_population_data, Experiment_control_site_or_spatial_variation,
               Sample_size_greater_than_1,
               Experiment_confounding_variables_included,
               Experiment_BACI)])

dat[Evidence_category == "Population", 
    study_type_synth := paste(Evidence_category,
                              Evidence_method,
                              ifelse(Predator_and_prey_population_data == 1,
                                    "with data", "without data")
                              # ifelse(Experiment_control_site_or_spatial_variation == 1,
                              #        "control/variation", "no_control"),
                              # ifelse(Sample_size_greater_than_1 == 1,
                              #        "n>1", "n==1"),
                              # ifelse(Experiment_confounding_variables_included == 1,
                              #        "confounders_considered", "confounders_ignored")
                              )]
# ^^^ THIS LEVEL OF DETAIL SHOULD BE IN A TABLE.
#
dat[, Evidence_category_data := ifelse(Evidence_category == "Population",
                                       paste(Evidence_category, ifelse(Predator_and_prey_population_data == 1,
                                                                       "with data", "without data")),
                                       Evidence_category)]

#
unique(dat[Evidence_category == "Lethal_program", 
           .(Evidence_category, Evidence_effect, Evidence_method,
             Predator_and_prey_population_data, Experiment_control_site_or_spatial_variation,
             Sample_size_greater_than_1,
             Experiment_confounding_variables_included,
             Experiment_BACI)])

dat[Evidence_category == "Lethal_program", 
    study_type_synth := paste(Evidence_category,
                              Evidence_method)]

unique(dat[Evidence_category == "Predation", 
           .(Evidence_category, Evidence_effect, Evidence_method,
             Predator_and_prey_population_data, Experiment_control_site_or_spatial_variation,
             Sample_size_greater_than_1,
             Experiment_confounding_variables_included,
             Experiment_BACI)])

#
dat[, study_type_synth := gsub(" NA", "", study_type_synth)]

dat[Evidence_category == "Predation", study_type_synth := "Predation"]

study_types <- dat[!is.na(study_type_synth), ]
study_types

study_types <- study_types[, .(n = .N),
                           by = .(study_type_synth, Evidence_category_data,
                                  #Evidence_effect,
                                  Hypothesis_supported,
                                  Quality_Study_Highlight)]

study_types[Hypothesis_supported == 0,
            n := -n]

study_types

unique(study_types$study_type_synth)
# unique(study_types[Evidence_category_data == "Population with data", ]$study_type_synth)
# unique(study_types[Evidence_category_data == "Population without data", ]$study_type_synth)

dat[Evidence_method == "Timing" & Quality_Study_Highlight == "Highlight"]


# >>> Plot ----------------------------------------------------------------
# color by Evidence_category_data
# y axis by study_type_synth...
unique(study_types$Evidence_category_data)
study_types[Quality_Study_Highlight == "Highlight", 
    Evidence_category_data := "Population with all qualities"]

pal <- c("Predation" = "grey50", 
         "Lethal_program" = "indianred4",
          "Lethal_program" = "indianred4",
          "Population without data" = "dodgerblue4",
          "Population with data" = "dodgerblue2",
         "Population with all qualities" = "dodgerblue2")

col_labs <- c("Predation" = "Predation", 
         "Lethal_program" = "Lethal program",
         "Lethal_program" = "Lethal program",
         "Population without data" = "Population without data",
         "Population with data" = "Population with data",
         "Population with all qualities" = "Population with all qualities")

col_pal <- c("Predation" = "transparent", 
         "Lethal_program" = "transparent",
         "Lethal_program" = "transparent",
         "Population without data" = "transparent",
         "Population with data" = "transparent",
         "Population with all qualities" = "gold")

#
unique(study_types$study_type_synth)

#
# study_types[study_type_synth == "Population Timing" &
#               Evidence_category_data == "Population with data"]
# dat[study_type_synth == "Population Timing" &
#               Evidence_category_data == "Population with data"]
# This looks like an error

#
sort(unique(study_types$study_type_synth))

# Ugh.
study_types$study_type_synth <- factor(study_types$study_type_synth,
                                       levels = c(
                                         "Predation",
                                         "Lethal_program Temporal",
                                         "Lethal_program Spatial",
                                         "Population Timing without data",
                                         "Population Temporal without data",
                                         "Population Spatial without data",
                                         "Population Spatiotemporal without data",
                                         
                                         "Population Timing with data",
                                         "Population Temporal with data",
                                         "Population Spatial with data",
                                         "Population Spatiotemporal with data"#,
                                         # "Population Temporal no_control n==1 confounders_ignored",
                                         #""
                                       ))
labs <- c(
  "Predation" = "Predation",
  "Lethal_program Temporal" = "Temporal",
  "Lethal_program Spatial" = "Spatial",
  "Population Timing without data" = "Timing",
  "Population Temporal without data" = "Temporal",
  "Population Spatial without data" = "Spatial",
  "Population Spatiotemporal without data" = "Spatiotemporal",
  
  "Population Timing with data" = "Timing",
  "Population Temporal with data" = "Temporal",
  "Population Spatial with data" = "Spatial",
  "Population Spatiotemporal with data" = "Spatiotemporal")


study_types[study_type_synth == "Population Spatial with data"]

unique(study_types$Evidence_category_data)
study_types$Evidence_category_data <- factor(study_types$Evidence_category_data,
                                             levels = rev(c("Predation", "Lethal_program",
                                                        "Population without data",
                                                        "Population with data",
                                                        "Population with all qualities")))
#
ggplot(data = study_types, 
       aes(x = n, y = (study_type_synth),
           fill = Evidence_category_data,
           color = Evidence_category_data))+
  geom_vline(xintercept = 0)+
  annotate(geom = "text", x = -50, y = 10.5,
           label = "Not in support", size = 3.3, hjust = 0,
           color = "grey30")+
  annotate(geom = "text", x = 50, y = 10.5,
           label = "In support", size = 3.3, hjust = 0,
           color = "grey30")+
  geom_col(linewidth = 1)+
  scale_fill_manual(name = NULL,
                    labels = col_labs,
                    values = pal)+
  scale_y_discrete(labels = labs)+
  scale_color_manual(name = NULL,
                     values = col_pal,
                     labels = col_labs)+
  # guides(color = "none")+
  ylab(NULL)+
  xlab("Number of studies")+
  coord_cartesian(clip = "off")+
  # facet_wrap(~Evidence_effect, ncol = 2)+
  theme_lundy+
  theme(legend.position=c(.85,.85))

ggsave("figures/systematic_review/november_2024/evidence_types.pdf", width = 8, height = 9)

# ~~~~~~~~~~~~~~~ ---------------------------------------------------------
# ~~~~~~~~~~~~~~~ ---------------------------------------------------------
# ~~~~~~~~~~~~~~~ ---------------------------------------------------------
# ~~~~~~~~~~~~~~~ ---------------------------------------------------------
#' 
#' 
#' # TEMP Held in reserve in case new approach doesnt work -------------------
#' 
#' 
#' combined.dat[, any_study := ifelse(nrow(.SD[!is.na(Study_ID), ]) > 0,
#'                                    "yes", "no"),
#'              by = .(scientificName, Rodent_attributed_final)]
#' combined.dat[, .(n = uniqueN(scientificName)),
#'              by = .(Rodent_attributed_final, any_study)]
#' 
#' unique(combined.dat$Evidence_category)
#' combined.dat[, Evidence_category_direction := paste(Evidence_category,
#'                                                     Hypothesis_supported,
#'                                                     sep = "_")]
#' unique(combined.dat$Evidence_category_direction)
#' 
#' # Now only has predation in support:
#' combined.dat[, has_predation_in_support := ifelse(any(.SD$Evidence_category_direction %in% "Predation_1"),
#'                                                   "yes", "no"),
#'              by = .(scientificName, Rodent_attributed_final)]
#' 
#' combined.dat[, .(n = uniqueN(scientificName)), by = .(has_predation_in_support)]
#' unique(combined.dat[Evidence_category_direction == "Predation_1", ]$scientificName)
#' combined.dat[scientificName == "Zosterops tenuirostris", .(Rodent_attributed_final,
#'                                                            has_predation_in_support)]
#' #
#' combined.dat[, has_other_evidence_in_support := ifelse(any(.SD$Evidence_category_direction %in% 
#'                                                              c("Population_1", "Lethal_program_1")),
#'                                                        "yes", "no"),
#'              by = .(scientificName, Rodent_attributed_final)]
#' combined.dat[, .(n = uniqueN(scientificName)), by = .(has_other_evidence_in_support)]
#' 
#' 
#' #
#' combined.dat[, only_predation_in_support := ifelse(has_other_evidence_in_support == "no" &
#'                                                      has_predation_in_support == "yes",
#'                                                    "yes", "no"),
#'              by = .(scientificName, Rodent_attributed_final)]
#' combined.dat[only_predation_in_support == "yes"]
#' combined.dat[, .(n = uniqueN(scientificName)), by = .(only_predation_in_support)]
#' 
#' # Population studies in support column
#' #' * Right now this is based on other studies in support *
#' sort(unique(combined.dat$Evidence_category_direction))
#' combined.dat[, lethal_program_in_support := ifelse(any(.SD$Evidence_category_direction_data %in% c("Population_1")),
#'                                                    "yes", "no"),
#'              by = .(scientificName, Rodent_attributed_final)]
#' combined.dat[, .(n = uniqueN(scientificName)), by = .(lethal_program_in_support)]
#' 
#' # Now, population studies with data in suppport.
#' unique(combined.dat$Evidence_category_direction)
#' combined.dat$Predator_and_prey_population_data
#' combined.dat[, Evidence_category_direction_data := paste(Evidence_category_direction,
#'                                                          Predator_and_prey_population_data,
#'                                                          sep = "_")]
#' unique(combined.dat$Evidence_category_direction_data)
#' combined.dat[, population_studies_with_data_in_support := ifelse(any(.SD$Evidence_category_direction_data %in%
#'                                                                        "Population_1_1"),
#'                                                                  "yes", "no"),
#'              by = .(scientificName, Rodent_attributed_final)]
#' combined.dat[, .(n = uniqueN(scientificName)), by = .(population_studies_with_data_in_support)]
#' 
#' 
#' # Lethal_program in support column
#' #' * Right now this is based on other studies in support *
#' sort(unique(combined.dat$Evidence_category_direction_data))
#' combined.dat[, lethal_program_in_support := ifelse(any(.SD$Evidence_category_direction_data %in% c("Lethal_program_1_0",
#'                                                                                                    "Lethal_program_1_1")) &
#'                                                      !any(.SD$population_studies_with_data_in_support %in% c("yes")) &
#'                                                      !any(.SD$population_studies_in_support %in% "yes"),
#'                                                    "yes", "no"),
#'              by = .(scientificName, Rodent_attributed_final)]
#' combined.dat[, .(n = uniqueN(scientificName)), by = .(lethal_program_in_support)]
#' 
#' 
#' # No studies in support
#' #' * Right now this is based on other studies in support *
#' sort(unique(combined.dat$Evidence_category_direction_data))
#' sort(unique(combined.dat$Evidence_category_direction))
#' (unique(combined.dat$Hypothesis_supported))
#' combined.dat[Evidence_category == "NONE"]$Hypothesis_supported
#' 
#' combined.dat[, no_studies_in_support := ifelse(all(.SD$Hypothesis_supported %in% 
#'                                                      c(0)),
#'                                                "yes", "no"),
#'              by = .(scientificName, Rodent_attributed_final)]
#' combined.dat[, .(n = uniqueN(scientificName)), by = .(no_studies_in_support)]
#' 
#' unique(combined.dat[no_studies_in_support == "yes", .(Evidence_category, Hypothesis_supported)])
#' # combined.dat[no_studies_in_support == "yes" & Evidence_category == "NONE",]
#' # combined.dat[no_studies_in_support == "yes" & Evidence_category == "NONE",
#' #              no_studies_in_support := NA]
#' # unique(combined.dat[no_studies_in_support == "yes", .(Evidence_category, Hypothesis_supported)])
#' 
#' # Now create a synthetic column:
#' combined.dat$synthetic_column <- NULL
#' combined.dat[any_study == "no", 
#'              synthetic_column := "No studies"]
#' unique(combined.dat[synthetic_column == "No studies"]$Article) # should be NA
#' 
#' # check as we go. Should be 0 rows:
#' combined.dat[only_predation_in_support == "yes" & !is.na(synthetic_column),]
#' combined.dat[only_predation_in_support == "yes", 
#'              synthetic_column := "Only that predation occurs"]
#' unique(combined.dat[synthetic_column == "Only that predation occurs", ]$Evidence_category_direction)
#' # Predation_1 and then other 0s should be here.
#' 
#' #
#' combined.dat[population_studies_with_data_in_support == "yes" & 
#'                !is.na(synthetic_column),] # should be 0 rows
#' combined.dat[population_studies_with_data_in_support == "yes", 
#'              synthetic_column := "Population studies with data in support"]
#' 
#' #
#' combined.dat[lethal_program_in_support == "yes" & !is.na(synthetic_column)]
#' combined.dat[lethal_program_in_support == "yes", 
#'              synthetic_column := "Other studies in support"]
#' combined.dat[is.na(synthetic_column)]
#' 
#' unique(combined.dat[is.na(synthetic_column)]$Hypothesis_supported)
#' 
#' 
#' # 
#' combined.dat[no_studies_in_support == "yes" & !is.na(synthetic_column)]
#' combined.dat[no_studies_in_support == "yes", 
#'              synthetic_column := "No studies in support"]
#' combined.dat[is.na(synthetic_column)]
#' 
#' 
#' 
#' # Testing...
#' combined.dat[, key := paste(scientificName, Rodent_attributed_final)]
#' combined.dat[, .(n_lvls = uniqueN(synthetic_column)),
#'              by = .(key)][n_lvls > 1, ] # should be 0 rows.
#' 
#' combined.dat
#' 
#' 
#' # ~~~~~~~~~~~~~~~ ---------------------------------------------------------
#' # OLD ---------------------------------------------------------------------
#' # ~~~~~~~~~~~~~~~ ---------------------------------------------------------
#' # ~~~~~~~~~~~~~~~ ---------------------------------------------------------
#' # ~~~~~~~~~~~~~~~ ---------------------------------------------------------
#' # ~~~~~~~~~~~~~~~ ---------------------------------------------------------
#' # ~~~~~~~~~~~~~~~ ---------------------------------------------------------
#' 
#' # By evidence quality -----------------------------------------------------
#' dat
#' unique(dat$Experiment_BACI)
#' dat[, Experiment_BACI := fcase(Experiment_BACI %in% c("", "0", "?"),
#'                                "No",
#'                                Experiment_BACI == "1",
#'                                "Yes",
#'                                default = NA)]
#' 
#' unique(dat$Experiment_confounding_variables_included)
#' dat[, Experiment_confounding_variables_included := as.character(Experiment_confounding_variables_included)]
#' dat[, Experiment_confounding_variables_included := fcase(
#'                       Experiment_confounding_variables_included %in% c(0), "No",
#'                       Experiment_confounding_variables_included == 1, "Yes",
#'                       default = NA)]
#' unique(dat$Experiment_confounding_variables_included)
#' 
#' unique(dat$Sample_size_greater_than_1)
#' dat[, Sample_size_greater_than_1 := as.character(Sample_size_greater_than_1)]
#' dat[, Sample_size_greater_than_1 := fcase(
#'   Sample_size_greater_than_1 %in% c(0), "No",
#'   Sample_size_greater_than_1 == 1, "Yes",
#'   default = NA)]
#' unique(dat$Sample_size_greater_than_1)
#' 
#' 
#' unique(dat$Experiment_control_site_or_spatial_variation)
#' dat[, Experiment_control_site_or_spatial_variation := fcase(Experiment_control_site_or_spatial_variation %in% c("", "0", "?"),
#'                                "No",
#'                                Experiment_control_site_or_spatial_variation == "1",
#'                                "Yes",
#'                                default = NA)]
#' unique(dat$Experiment_control_site_or_spatial_variation)
#' 
#' dat
#' 
#' #
#' studies.freq <- dat[, .(evidence = sum(value, na.rm = T),
#'                         n_articles = uniqueN(.SD[!is.na(Article)]$Article)),
#'                     by = .(#scientificName, 
#'                            Study_type_simplest, 
#'                            Rodent_display, Bird_Type_Coarse,
#'                            Hypothesis_supported,
#'                            Experiment_control_site_or_spatial_variation,
#'                            Sample_size_greater_than_1,
#'                            Experiment_confounding_variables_included,
#'                            Experiment_BACI)]
#' studies.freq
#' # studies.freq <- studies.freq[n_articles > 0, ]
#' 
#' studies.freq[Hypothesis_supported == 0, n_articles := -n_articles]
#' studies.freq
#' 
#' # Melt:
#' studies.freq.mlt <- melt(studies.freq,
#'                          id.vars = c("Study_type_simplest",
#'                                      "Rodent_display", "Bird_Type_Coarse",
#'                                      "Hypothesis_supported",
#'                                      "n_articles"),
#'                          measure.vars = c("Experiment_control_site_or_spatial_variation",
#'                                           "Sample_size_greater_than_1", "Experiment_confounding_variables_included",
#'                                           "Experiment_BACI")
#'                          )
#' studies.freq.mlt
#' 
#' #
#' unique(studies.freq.mlt$variable)
#' unique(studies.freq$Experiment_control_site_or_spatial_variation)
#' 
#' #
#' ggplot(data = studies.freq.mlt[value == "Yes"],
#'        aes(x = n_articles, y = variable, fill = Study_type_simplest))+
#'   geom_vline(xintercept = 0)+
#'   geom_col()+
#'   ylab(NULL)+
#'   coord_cartesian(xlim = c(-5, 15))+
#'   facet_wrap(~Bird_Type_Coarse)+
#'   theme_lundy
#' 
#' 
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------------
#' # Summarize by bird family ------------------------------------------------
#' unique(dat$Order)
#' dat[Order == "Apterygiformes; Struthioniformes"]
#' dat[Order == "Apterygiformes; Struthioniformes", Order := "Apterygiformes"]
#' 
#' # Get number per species and then take average?
#' studies.freq <- dat[, .(evidence = sum(value, na.rm = T),
#'                         n_articles = uniqueN(.SD[!is.na(Article)]$Article)),
#'                     by = .(scientificName, Study_type_simplest, 
#'                            Rodent_display, Order,
#'                            Hypothesis_supported)]
#' studies.freq
#' 
#' studies.freq[, n_articles := ifelse(Hypothesis_supported == 0, -n_articles, n_articles)]
#' studies.freq
#' studies.freq[is.na(n_articles),]
#' # Get rid of the count for NA:
#' studies.freq[is.na(n_articles), n_articles := 0]
#' 
#' studies.freq
#' 
#' #
#' order.summary <- studies.freq[, .(total_n_articles = sum(n_articles)),
#'                               by = .(Rodent_display, Order,
#'                                      Hypothesis_supported,
#'                                      Study_type_simplest)]
#' order.summary
#' 
#' 
#' # >>> Plot ----------------------------------------------------------------
#' 
#' #
#' ggplot(data = order.summary, 
#'        aes(x = total_n_articles, y = Order, 
#'            fill = Study_type_simplest))+
#'   geom_vline(xintercept = 0)+
#'   ylab("Bird Order")+
#'   xlab("Total number of articles")+
#'   geom_col(position = "dodge")+
#'   # geom_errorbarh(aes(xmin = mean_n_articles - sd_n_articles, xmax = mean_n_articles + sd_n_articles,
#'   #                    y = Order,
#'   #                    group = Rodent_display),
#'   #                position = "dodge")+
#'   facet_wrap(~Rodent_display,
#'              scales = "free_y",
#'              ncol = 3)+
#'   theme_lundy
#' 
#' ggsave("figures/systematic_review/prelim_sept_2024/evidence by order.png", width = 12, height = 12)
#' 
#' # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------------
#' # Plot summaries? -------------------------------------------------------
#' # % of species without any evidence
#' # % of species with only predation studies
#' # % of population studies in support
#' # ifelse(Study_type_simple == "",
#'        # "No studies", Study_type_simple)
#' dat.alt <- copy(dat)
#' unique(dat.alt$Study_type_simplest)
#' 
#' # dat.alt[, .(any_population_study = ifelse(any(.SD$Study_type_simplest %in% c("Population")),
#' #                                           "yes", "no")),
#' #         by = .(scientificName, Rodent_display)]
#' 
#' unique(dat.alt$Hypothesis_supported)
#' dat.alt[, total_species := uniqueN(scientificName),
#'         by = .(Rodent_display)]
#' 
#' # Let's try data.table's case_when
#' dat.alt[, Study_classification := fcase(
#'   #' *case 1: No studies*
#'   all(.SD$Study_type_simplest == ""), "No studies",
#'   #' *case 2: Population study in support*
#'   any(Study_type_simplest %in% c("Population") &
#'       Hypothesis_supported == 1), "Population study in support",
#'   #' *case 3: Population study in opposition*
#'   any(Study_type_simplest %in% c("Population") &
#'         Hypothesis_supported == 0), "Population study not in support",
#'   #' *case 4: ONLY Predation studies*
#'   all(Study_type_simplest == "Predation"), "Predation studies only"#,
#'   #' *case 5: all Predation studies*
#'   #any(Study_type_simplest == "Killing") &
#'    # !any(Study_type_simplest %in% c("Population")), "Lethal control studies"
#'       ),
#'   by = .(scientificName, Rodent_display)]
#' 
#' unique(dat.alt$Study_classification)
#' 
#' dat.alt[is.na(Study_classification), ]
#' dat.alt
#' 
#' # HOW TO CLASSIFY KILLING STUDIES IN THIS?
#' 
#' dat.spp.perc <- dat.alt[, .(percent_species = uniqueN(scientificName) / total_species * 100),
#'                         by = .(Rodent_display, Study_classification, total_species)] |> unique()
#' dat.spp.perc
#' 
#' # make sure this adds up to 100
#' dat.spp.perc[, .(sum(percent_species)), by = .(Rodent_display)]
#' unique(dat.spp.perc$Study_classification)
#' dat.spp.perc$Study_classification <- factor(dat.spp.perc$Study_classification,
#'                                             levels = rev(c("No studies", "Predation studies only",
#'                                                            "Lethal control studies",
#'                                                        "Population study not in support",
#'                                                        "Population study in support")))
#' 
#' dat.spp.perc[, Rodent_label := paste0(Rodent_display, " (", total_species, ")")]
#' unique(dat.spp.perc$Rodent_label)
#' dat.spp.perc$Rodent_label <- factor(dat.spp.perc$Rodent_label,
#'                                       levels = rev(c("Rattus rattus (104)", "Rattus norvegicus (64)", 
#'                                                  "Rattus exulans (48)",
#'                                                  "Mus musculus (13)", "Other rodents (1)")))
#' 
#' ggplot(data = dat.spp.perc[!is.na(Study_classification)], aes( y = Rodent_label, x = percent_species, 
#'                                  fill = Study_classification)) + 
#'   geom_col()+
#'   xlab("Percent of bird species attributed")+
#'   ylab(NULL)+
#'   scale_fill_discrete( guide = guide_legend(reverse = TRUE))+
#'   theme_lundy
#' 
#' ggsave("figures/systematic_review/prelim_sept_2024/evidence percent summaries by rodent.pdf", width = 8, height = 8)
#' 
#' 
