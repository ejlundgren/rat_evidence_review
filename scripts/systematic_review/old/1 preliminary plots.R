
# Prepare environment -----------------------------------------------------

rm(list = ls())

library("groundhog")

date <- "2024-03-15"
pcks <- c("data.table", "ggplot2", "tidyr", "readxl",
          "stringr", "dplyr")
groundhog.library(pcks, date)


theme_lundy <- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Load systematic review --------------------------------------------------

dat <- fread("data/Live/Evidence July 9 2024.csv")
dat

names(dat)
dat <- dat[, .(scientificName, Common_name,
               redlistCategory,
               Synonyms_or_previous_lump, Rodent_attributed_IUCN,
               Rodent_primary, Rodent_only_primary,
               Article, Fishy_Study_Type, Study_type,
               Predation_type, Hypothesis_supported,
               Predator_and_prey_data, Experiment_control_site_or_spatial_variation,
               Experiment_sample_size, Experiment_confounding_variables_included,
               Experiment_sites_randomly_selected_REMOVE,
               Experiment_BACI, Study_rodent, Study_location,
               Latitude, Longitude)]
dat

# >>> Merge in traits/orders ----------------------------------------------
taxa <- fread("data/Live/Provisional_Trait_Taxonomy.csv")
taxa

all(dat$scientificName %in% taxa$scientificName)
dat.mrg <- merge(dat,
                 taxa[, .(scientificName, Order)],
                 by = "scientificName")

dat.mrg

dat <- copy(dat.mrg)

# >>> Simplify study type? ------------------------------------------------
unique(dat$Study_type)
# For now, let's leave it.

# >>> Categorize by study rodent ----------------------------------------
# unique(dat$Study_rodent)
# dat[Study_rodent %in% c("Rattus rattus, Rattus exulans",
#                         "Rattus exulans, Rattus rattus", 
#                         "Rattus rattus, Rattus norvegicus, Rattus exulans",
#                         "Rattus rattus, Mus musculus",
#                         "Rodent",
#                         "Rattus rattus, Rattus norvegicus"),
#     Study_rodent_group := "Mixed rodent community"]
# 
# unique(dat[is.na(Study_rodent_group)]$Study_rodent)
# dat[Study_rodent %in% c("Rattus"),
#     Study_rodent_group := "Unspecified Rattus"]
# 
# 
# unique(dat[is.na(Study_rodent_group)]$Study_rodent)
# dat[is.na(Study_rodent_group),
#     Study_rodent_group := Study_rodent]
# 
# unique(dat$Study_rodent_group)
# Let's do some separating, melting, and casting



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Rodent centered analysis ------------------------------------------------
# >>> Split and melt by rodent species ------------------------------------
# 1 row per rodent species...
# Add a row ID...
# dat[, row_ID := seq(1:.N)]

attributed <- dat[, .(scientificName, Common_name, redlistCategory,
                      Order,
                      Rodent_attributed_IUCN)]
unique(attributed$Rodent_attributed_IUCN)
n <- max(str_count(attributed$Rodent_attributed_IUCN, ","))+1
attributed <- attributed %>%
  separate(col = "Rodent_attributed_IUCN", sep = ",",
           into = paste0("rodent_", seq(1:n)))

setDT(attributed)
attributed <- melt(attributed,
                   id.vars = c("scientificName", "Common_name",
                               "Order",
                               "redlistCategory"),
                   value.name = "Rodent_attributed")

attributed <- unique(attributed[!is.na(Rodent_attributed), ])
attributed$variable <- NULL
attributed

# Now do the same for the study rodent:
studies <- dat[!is.na(Article) & Article != "", .(scientificName, Hypothesis_supported,
                                Article, Study_type, Study_rodent)]
studies
unique(studies$Study_rodent)
n <- max(str_count(studies$Study_rodent, ","))+1
studies <- studies %>%
  separate(col = "Study_rodent", sep = ",",
           into = paste0("rodent_", seq(1:n)))
setDT(studies)
studies <- melt(studies,
                   id.vars = c("scientificName", "Article", "Study_type",
                               "Hypothesis_supported"),
                   value.name = "Rodent_studied")
studies <- studies[!is.na(Rodent_studied) & Rodent_studied != "", ]
studies

# Now get frequency of support...
studies[, value := ifelse(Hypothesis_supported == 0, -1, 1)]
studies
studies.freq <- studies[, .(evidence = sum(value),
                            n_articles = uniqueN(Article)),
                        by = .(scientificName, Study_type, Rodent_studied,
                               Hypothesis_supported)]
studies.freq[, n_articles := ifelse(Hypothesis_supported == 0, -n_articles, n_articles)]
studies.freq

# Merge into attribution:
unique(attributed$Rodent_attributed)
attributed[, Rodent_attributed := trimws(Rodent_attributed)]
attributed <- attributed[Rodent_attributed != "", ]

unique(studies.freq$Rodent_studied)
studies.freq[, Rodent_studied := trimws(Rodent_studied)]
studies.freq[, Rodent_studied := ifelse(Rodent_studied == "Rodent",
                                        "Rodentia", Rodent_studied)]

studies.freq[Rodent_studied == "", ]
# dat[!is.na(Article) & Article != "" & Rodent_studied == "", ]

attributed[, key := paste(scientificName, Rodent_attributed)]
studies.freq[, key := paste(scientificName, Rodent_studied)]

setdiff(studies.freq$key, attributed$key)
length(unique(studies.freq$key))
studies.freq[scientificName == "Pterodroma brevipes"]
attributed[scientificName == "Pterodroma brevipes"]
# OK...So How do make these match?
# Hmmmm.

# >>> Deal with hierarchical keys. ----------------------------------------
# This is a messy pain in the ass.
# I think we have to melt again....Ugh...
studies.freq[, ID := seq(1:.N)]

unique(attributed$Rodent_attributed)
studies.freq[, Rodent_studied1 := Rodent_studied]
studies.freq[, Rodent_studied2 := ifelse(grepl("Rattus", Rodent_studied),
                                         "Rattus", NA)]
studies.freq[, Rodent_studied3 := "Rodentia"]
studies.freq.mlt <- melt(studies.freq,
                         measure.vars = c("Rodent_studied1", "Rodent_studied2",
                                          "Rodent_studied3"))
studies.freq.mlt$variable <- NULL
studies.freq.mlt <- studies.freq.mlt[!is.na(value) & value != "NA"]

# Remake key
attributed[, key := paste(scientificName, Rodent_attributed)]
studies.freq.mlt[, key := paste(scientificName, value)]
unique(studies.freq.mlt$key)

setdiff(attributed$key, studies.freq.mlt$key)
# That's ok, because many don't have studies.

setdiff(studies.freq.mlt$key, attributed$key)
setdiff(studies.freq.mlt[!grepl("Rodentia", key)]$key, attributed$key)

#
unique(attributed$Rodent_attributed)
attributed[Rodent_attributed == "Rodentia"]
studies.freq.mlt[grepl("argiventer", value)]

# Now do the same for attributed...
unique(studies.freq$Rodent_studied)
unique(attributed$Rodent_attributed)
attributed[, attr_ID := seq(1:.N)]

attributed[, Rodent_attributed1 := Rodent_attributed]
attributed[, Rodent_attributed2 := ifelse(grepl("Rattus", Rodent_attributed),
                                         "Rattus", NA)]
attributed[, Rodent_attributed3 := "Rodentia"]

attributed.mlt <- melt(attributed,
                         measure.vars = c("Rodent_attributed1", "Rodent_attributed2",
                                          "Rodent_attributed3"))
attributed.mlt
attributed.mlt <- attributed.mlt[!is.na(value) & value != "NA"]

# Remake key
attributed.mlt[, key := paste(scientificName, value)]
studies.freq.mlt[, key := paste(scientificName, value)]
unique(studies.freq.mlt$key)
#
rodent.dat <- merge(attributed.mlt,
                    studies.freq.mlt[, !c("scientificName")],
                    by = "key", 
                    all.x = T,
                    all.y = F,
                    allow.cartesian = T)

rodent.dat$variable <- NULL
setdiff(studies.freq$ID, rodent.dat$ID)
# damn.
studies.freq[!ID %in% rodent.dat$ID]
attributed[!attr_ID %in% rodent.dat$attr_ID]

# NIghtmare. But I think we have to fix this here...Not before...
unique(rodent.dat[Rodent_attributed != Rodent_studied, .(Rodent_attributed, Rodent_studied)])

# rodent.dat <- rodent.dat[(Rodent_attributed == "Mus musculus" &
#                            Rodent_studied == "Rattus")]
# rodent.dat
# Damnit....But that drops it GAHHH ARIAN!!!!
rodent.dat[Rodent_attributed == "Mus musculus" &
             Rodent_studied == "Rattus",]
rodent.dat[Rodent_attributed == "Mus musculus" &
                Rodent_studied == "Rattus",
           `:=` (ID = NA, 
                 Hypothesis_supported = NA, Study_type = NA,
                 value.x = NA, Rodent_studied = NA, evidence = NA, 
                 n_articles = NA)]
rodent.dat
#
rodent.dat[Rodent_attributed == "Peromyscus maniculatus" &
             Rodent_studied == "Rattus rattus",
           `:=` (ID = NA, 
                 Hypothesis_supported = NA, Study_type = NA,
                 value.x = NA, Rodent_studied = NA, evidence = NA, 
                 n_articles = NA)]

#
rodent.dat[Rodent_attributed %in% c("Mus musculus", "Rattus rattus") &
             Rodent_studied == "Peromyscus maniculatus",
           `:=` (ID = NA, 
                 Hypothesis_supported = NA, Study_type = NA,
                 value.x = NA, Rodent_studied = NA, evidence = NA, 
                 n_articles = NA)]

rodent.dat

rodent.dat[Rodent_attributed %in% c("Mus musculus") &
             Rodent_studied %in% c("Peromyscus maniculatus", "Rattus norvegicus",
                                   "Rattus rattus", "Rattus"),
           `:=` (ID = NA, 
                 Hypothesis_supported = NA, Study_type = NA,
                 value.x = NA, Rodent_studied = NA, evidence = NA, 
                 n_articles = NA)]
#
rodent.dat[Rodent_attributed %in% c("Rattus norvegicus") &
             Rodent_studied %in% c("Peromyscus maniculatus",
                                   "Rattus rattus",  "Rattus exulans", "Mus musculus"),
           `:=` (ID = NA, 
                 Hypothesis_supported = NA, Study_type = NA,
                 value.x = NA, Rodent_studied = NA, evidence = NA, 
                 n_articles = NA)]

rodent.dat[Rodent_attributed %in% c("Rattus exulans") &
             Rodent_studied %in% c("Peromyscus maniculatus",
                                   "Rattus rattus",  "Rattus norvegicus", "Mus musculus"),
           `:=` (ID = NA, 
                 Hypothesis_supported = NA, Study_type = NA,
                 value.x = NA, Rodent_studied = NA, evidence = NA, 
                 n_articles = NA)]


rodent.dat[Rodent_attributed %in% c("Rattus rattus") &
             Rodent_studied %in% c("Peromyscus maniculatus",
                                   "Rattus exulans",  "Rattus norvegicus", "Mus musculus"),
           `:=` (ID = NA, 
                 Hypothesis_supported = NA, Study_type = NA,
                 value.x = NA, Rodent_studied = NA, evidence = NA, 
                 n_articles = NA)]

unique(rodent.dat[Rodent_attributed != Rodent_studied, .(Rodent_attributed, Rodent_studied)])

rodent.dat[Rodent_attributed %in% c("Rattus norvegicus", 'Rattus rattus',
                                    "Rattus") &
             Rodent_studied %in% c("Peromyscus maniculatus",
                                   "Mus musculus"),
           `:=` (ID = NA, 
                 Hypothesis_supported = NA, Study_type = NA,
                 value.x = NA, Rodent_studied = NA, evidence = NA, 
                 n_articles = NA)]
#
rodent.dat[Rodent_attributed %in% c("Rattus exulans") &
             Rodent_studied %in% c("Rattus rattus", "Rattus norvegicus",
                                   "Mus musculus"),
           `:=` (ID = NA, 
                 Hypothesis_supported = NA, Study_type = NA,
                 value.x = NA, Rodent_studied = NA, evidence = NA, 
                 n_articles = NA)]
unique(rodent.dat[Rodent_attributed != Rodent_studied, .(Rodent_attributed, Rodent_studied)])


rodent.dat

x <- studies.freq[!ID %in% rodent.dat$ID]$scientificName
attributed[!attr_ID %in% rodent.dat$attr_ID]
dput(x)

dat[scientificName %in% x[1], .(scientificName, Rodent_attributed_IUCN,
                                Study_rodent)]


dat[scientificName %in% x[2], .(scientificName, Rodent_attributed_IUCN,
                                Study_rodent)]



dat[scientificName %in% x[3], .(scientificName, Rodent_attributed_IUCN,
                                Study_rodent)]



dat[scientificName %in% x[4], .(scientificName, Rodent_attributed_IUCN,
                                Study_rodent)]


dat[scientificName %in% x[5], .(scientificName, Rodent_attributed_IUCN,
                                Study_rodent)]
# These all make sense being excluded...

rodent.dat[, `:=` (value.x = NULL, value.y = NULL)]
rodent.dat
unique(rodent.dat[, .(evidence, n_articles)])
rodent.dat[is.na(evidence), evidence := 0]
rodent.dat[is.na(n_articles), n_articles := 0]


# >>> Plot ----------------------------------------------------------------
length(unique(rodent.dat[Rodent_attributed == "Mus musculus"]$scientificName))
length(unique(rodent.dat[Rodent_attributed == "Mus musculus" &
                           is.na(Rodent_studied)]$scientificName))
rodent.dat
rodent.dat[Rodent_attributed == "Mus musculus"]

# rodent.dat[evidence > 30]
dat

#
ggplot(data = rodent.dat, aes(x = evidence, y = scientificName, 
                                   fill = Study_type))+
  geom_vline(xintercept = 0)+
  geom_col()+
  facet_wrap(~Rodent_attributed, scales = "free_y",
             ncol = 2)+
  theme_lundy



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Overall number of studies ---------------------------------

support_alldata <- dat[, .(n_studies = uniqueN(Article),
                           n_data = .N),
                       by = .(scientificName, Common_name,
                              redlistCategory, Study_type,
                              Hypothesis_supported, 
                              Study_rodent)]

support_alldata
range(support_alldata$n_data)
range(support_alldata$n_studies)

support_alldata[Hypothesis_supported == 0, `:=` (n_data = -n_data,
                                                 n_studies = -n_studies)]
support_alldata[is.na(Hypothesis_supported), `:=` (n_data = 0,
                                                   n_studies = 0)]

support_alldata[n_data != n_studies, ]

# THis should have rows:
support_alldata[duplicated(paste(scientificName, Study_type))]
support_alldata[scientificName == "Zosterops modestus"]
# good. 

# This should have 0 rows:
support_alldata[duplicated(paste(scientificName, Study_type, Hypothesis_supported))]
support_alldata[scientificName == "Chlidonias albostriatus"]


# >>> By rodent community -------------------------------------------------
#  THIS DOESN"T MAKE SENSE
# unique(dat$Hypothesis_supported)
# 
# support_alldata <- dat[, .(n_studies = uniqueN(Article),
#                            n_data = .N),
#                        by = .(scientificName, Common_name,
#                               redlistCategory, Study_type,
#                               Hypothesis_supported, Study_rodent_group)]
# 
# support_alldata
# range(support_alldata$n_data)
# range(support_alldata$n_studies)
# 
# support_alldata[Hypothesis_supported == 0, `:=` (n_data = -n_data,
#                                                  n_studies = -n_studies)]
# support_alldata[is.na(Hypothesis_supported), `:=` (n_data = 0,
#                                                    n_studies = 0)]
# 
# support_alldata[n_data != n_studies, ]

# ggplot(data = support_alldata, aes(x = n_data, y = scientificName, 
#                                    fill = Study_type))+
#   geom_vline(xintercept = 0)+
#   geom_col()+
#   facet_wrap(~Study_rodent_group, scales = "free_y")+
#   theme_lundy


# >>> Plot by rodent species ---------------------------------------------------

# >>> By bird order ---------------------------------------

unique(dat$Hypothesis_supported)

support_alldata <- dat[, .(n_studies = uniqueN(Article),
                           n_data = .N),
                       by = .(scientificName, Common_name,
                              redlistCategory, Study_type,
                              Hypothesis_supported, Order)]

support_alldata
range(support_alldata$n_data)
range(support_alldata$n_studies)

support_alldata[Hypothesis_supported == 0, `:=` (n_data = -n_data,
                                                 n_studies = -n_studies)]
support_alldata[is.na(Hypothesis_supported), `:=` (n_data = 0,
                                                   n_studies = 0)]
support_alldata[n_data != n_studies, ]

support_alldata[n_data > 20, ]

dat[scientificName %in% c("Diomedea dabbenena", "Puffinus yelkouan"), .(n = .N),
    by = .(scientificName, Hypothesis_supported)]


# >>> Preliminary plot ----------------------------------------------------
# sort(unique(support_alldata$Order))
# support_alldata[Order %in% c("")]

ggplot(data = support_alldata, aes(x = n_data, y = scientificName, 
                                   fill = Study_type))+
  geom_vline(xintercept = 0)+
  geom_col()+
  facet_wrap(~Order, scales = "free_y")+
  theme_lundy
# Impossible.

# Make this into multiple columns
nrow(support_alldata)
support_alldata[duplicated(paste(scientificName, Study_type, Hypothesis_supported))]

length(unique(support_alldata$scientificName))
342/3

spp <- data.table(scientificName = unique(support_alldata$scientificName))
spp
setorder(spp, scientificName)
spp[1:114, group := 1]
spp[115:228, group := 2]
spp[228:342, group := 3]

spp[, species_num := seq(1:.N)]
spp[, fill := ifelse(species_num %% 2 == 0,
                     "grey50", "white")]
spp

support_alldata.mrg <- merge(support_alldata,
                         spp,
                         by = "scientificName")

# labs <- 

#
ggplot(data = support_alldata.mrg, aes(x = n_data, y = scientificName, 
                                   fill = Study_type))+
  geom_vline(xintercept = 0)+
  geom_col()+
  facet_wrap(~group, scales = "free_y")+
  scale_fill_discrete(breaks = unique(support_alldata.mrg[Study_type != "", ]$Study_type))+
  # scale_y_continuous(breaks = as.numeric(spp$species_num))+
  theme_lundy+
  theme(legend.position = "bottom",
        strip.text = element_blank())
ggsave("figures/prelim_may_2024/prelim_evidence.png", width = 15, height = 12)

