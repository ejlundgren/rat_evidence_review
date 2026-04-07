#
#
# Plot timing data
#
#

rm(list = ls())

library("groundhog")

date <- "2024-07-15"
pcks <- c("data.table", "ggplot2", "tidyr", "readxl",
          "scico")
groundhog.library(pcks, date)

theme_lundy <- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

raw <- readRDS("builds/timing_dataset_clean.Rds")
raw

# Let's try flipping the direction of this. Might be more intuitive...
raw[, `:=` (min_difference = -min_difference,
            max_difference = -max_difference,
            median_difference = -median_difference)]

raw <- raw[Rodent_species_attributed == 1]

# Create an all-rodent summary --------------------------------------------
dat.all <- copy(raw)

dat.all$Rodent_species
dat.all[, Rodent_species := "All rodents"]

dat.all

dat.all <- dat.all[, .(min_difference = min(min_difference),
                       max_difference = max(max_difference),
                       median_difference = median(median_difference)),
                   by = .(Rodent_species, Location,
                          scientificName)]

dat.comb <- rbind(dat.all,
                  raw[, .(min_difference, max_difference,
                          median_difference,
                          Rodent_species, Location, scientificName)])

# Add categorical variable -----------------------------------------------------------------
unique(dat.comb[, .(min_difference, max_difference, median_difference)])
#' [We want the following levels:]
#' ------- * 1. Extinction definitely before arrival *
#' ------- * 2. Extinction before or after arrival *
#' ------- * 3. Extinction within 100 years of arrival *
#' ------- * 4. Extinction at least 100-499 years after arrival *
#' ------- * 5. Extinction at least 500-999 years after arrival *
#' ------- * 6. Extinction at least 1,000 years after arrival *

# 1. 
dat.comb[min_difference < 0 & max_difference < 0,
    cat_arrival := "Extinction before arrival"]

# 2.
dat.comb[min_difference <= 0 & max_difference >= 0,
    cat_arrival := "Extinction before or after arrival"]

# 3. 
dat.comb[min_difference >= 0 & min_difference <= 100, #' [Check with Arian that this should be based on max]
    cat_arrival := "Extinction within 100 years of arrival"]

# 4. 
dat.comb[min_difference > 100 & #' [This might cause issues]
      min_difference <= 499, #' [Check with Arian that this should be based on max]
    cat_arrival := "Extinction at least 100-499 years after arrival"]

# 5.
dat.comb[min_difference > 499 & #' [This might cause issues]
      min_difference <= 999, #' [Check with Arian that this should be based on max]
    cat_arrival := "Extinction at least 500-999 years after arrival"]

# 6.
dat.comb[min_difference > 999, #' [Check with Arian that this should be based on max]
    cat_arrival := "Extinction at least 1,000 years after arrival"]

dat.comb[is.na(cat_arrival), .(min_difference, max_difference)]
#' [Need to resolve this with Arian]

dat.comb
fwrite(dat.comb, "builds/timing/timing_dataset_for_checking.csv")

# >>> Checking ------------------------------------------------------------
nms <- dat.comb[cat_arrival == "Extinction at least 1,000 years after arrival"]$scientificName
raw[scientificName %in% nms, ]

dat.comb[min_difference > 999, ]
nms

key <- paste(dat.comb[cat_arrival == "Extinction at least 1,000 years after arrival"]$scientificName,
             dat.comb[cat_arrival == "Extinction at least 1,000 years after arrival"]$Location)

# Summarize ------------------------------------------------------------
# Calculate frequencies. Including by all rodents.

dat.summary <- dat.comb[, .(n = .N),
                   by = .(Rodent_species, cat_arrival)]
dat.summary
setorder(dat.summary, Rodent_species, cat_arrival)
fwrite(dat.summary, "builds/timing/timing_dataset_summaries.csv")


# Plot -----------------------------------------------------
dat.summary$Rodent_species <- factor(dat.summary$Rodent_species,
                                     levels = c("All rodents",
                                                "Rattus rattus",
                                                "Rattus norvegicus",
                                                "Rattus exulans",
                                                "Mus musculus"))

dat.summary$cat_arrival <- factor(dat.summary$cat_arrival,
                                  levels = rev(c("Extinction before arrival",
                                             "Extinction before or after arrival",
                                             "Extinction within 100 years of arrival",
                                             "Extinction at least 100-499 years after arrival",
                                             "Extinction at least 500-999 years after arrival",
                                             "Extinction at least 1,000 years after arrival")))
scico_palette_show()
# pal <- rev(c("#000000", "#777777", "dodgerblue4",
#          "#6E868C", "#A0A597", "#B9B88C"))
# pal <- scico(n = (length(unique(dat.summary$cat_arrival)) + 1),
#             palette = "vikO")
# pal <- pal[-7]
# pal <- c("Extinction before arrival" = "#B1B7D1",
#          "Extinction before or after arrival" = "#9B9FB5",
#          "Extinction within 100 years of arrival" = "#6BAA75",# "#9EBD6E",
#          "Extinction at least 100-499 years after arrival" = "#7A6174",#"#255957",# "#CDC392", #"#FCB07E", , ##D1B1C8",
#          "Extinction at least 500-999 years after arrival" = "#664E4C", # "#8C7284",
#          "Extinction at least 1,000 years after arrival" = "#002A32"#"#7A6174"
#          ) 
# scico(n = 5, palette = "grayC")
# scico(n = 7, palette = "nuuk")

length(unique(dat.summary$cat_arrival))

pal <- rev(scico(n = 6, palette = "grayC", end = .7))

names(pal) <- levels(dat.summary$cat_arrival)
pal[names(pal) == "Extinction within 100 years of arrival"] <- "dodgerblue4"


ggplot(data = dat.summary[Rodent_species != "All rodents"], 
       aes(x = Rodent_species, y = n,
                               fill = cat_arrival))+
  geom_col(width = .75)+
  scale_x_discrete(labels = c("All rodents" = "All rodents",
                                "Rattus rattus" = "Black rats", 
                                "Rattus norvegicus" = "Brown rats",
                                "Rattus exulans" = "Pacific rats",
                                "Mus musculus" = "House mouse"))+
  scale_fill_manual(name = NULL, values = pal)+
  ylab("Number of attributed extinctions")+
  xlab(NULL)+
  theme_lundy+
  theme(legend.position=c(.75,.75))

ggsave("figures/timing/initial/timing summary.png", width = 7, height = 7,
       dpi = 300)
ggsave("figures/timing/initial/timing summary.pdf", width = 7, height = 7,
       dpi = 300)

# Species level plots -----------------------------------------------
dat <- dat.comb[Rodent_species != "All rodents"]

dat$Rodent_species_attributed
ggplot()+
  geom_vline(xintercept = 0)+
  geom_pointrange(data = dat,
                  aes(y = scientificName,
                      x = median_difference, xmin = min_difference,
                      xmax = max_difference, fill = Rodent_species),
                  shape = 21)+
  theme_bw()+
  theme(panel.border = element_blank())

#
annot <- data.frame(lab = c("Extinction occurred before rodent arrival",
                            "Extinction occurred after rodent arrival"),
                    y = "Zoothera terrestris",
                    x = c(-500, 1000))

ggplot()+
  geom_vline(xintercept = 0)+
  geom_pointrange(data = dat,
                  aes(y = scientificName,
                      x = median_difference, xmin = min_difference,
                      xmax = max_difference, fill = Rodent_species),
                  shape = 21,
                  position = position_jitter(height = .3))+
  geom_text(data = annot, aes(x = x, y = y, 
                              label = lab),
            size = 3,
            vjust = -.5)+
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 20))+
  scale_x_continuous(breaks = rev(c(-2000, -1000, -500, -200, -100, -50, 0,
                                50, 100, 200, 500)))+ #breaks = scales::pretty_breaks(n = 20)
  coord_cartesian(clip = "off")+
  scale_fill_discrete(name = NULL)+
  xlab("Years between rodent arrival and extinction/extirpation")+
  ylab(NULL)+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

# ggsave("figures/prelim_may_2024/timing_fig.pdf", width = 14, height = 7)


# Do a line range....But if the distance is 0...
dat[, range := min_difference - max_difference]
unique(dat$range)

annot <- data.frame(lab = c("Extinction\nbefore arrival",
                            "Extinction\nafter arrival"),
                    y = 68,
                    x = c(-500, 2500),
                    Rodent_species = "Rattus rattus")
annot$Rodent_species <- factor(annot$Rodent_species,
                               levels = c("Rattus rattus", "Rattus norvegicus",
                                          "Rattus exulans", "Mus musculus"))
unique(dat$Rodent_species)
dat$Rodent_species <- factor(dat$Rodent_species,
                             levels = c("Rattus rattus", "Rattus norvegicus",
                                        "Rattus exulans", "Mus musculus"))
unique(dat$Rodent_species)

library("ggstance")
library(scales)

#
# dat$col <- cut_number(dat$scientificName, 3, labels = c(1:3))
# Let's split into multiple columns
pal2 <- rev(scico(n = 6, palette = "grayC", end = .9))

names(pal2) <- levels(dat.summary$cat_arrival)
pal2[names(pal2) == "Extinction within 100 years of arrival"] <- "dodgerblue"
show_col(pal2)

unique(dat$cat_arrival)
dat$cat_arrival <- factor(dat$cat_arrival,
                          levels = c("Extinction before arrival", "Extinction before or after arrival",
                                     "Extinction within 100 years of arrival", "Extinction at least 100-499 years after arrival",
                                     "Extinction at least 500-999 years after arrival",
                                     "Extinction at least 1,000 years after arrival"))

ggplot()+
  geom_vline(xintercept = 0)+
  geom_pointrange(data = dat,
                  aes(y = scientificName, xmin = min_difference,
                      x = median_difference,
                      xmax = max_difference, color = cat_arrival),
                 position = ggstance::position_dodgev(height = .5),
                 linewidth = 1)+ # position = position_jitter(height = .3)
  geom_text(data = annot, aes(x = x, y = y, 
                              label = lab),
            size = 3)+
  scale_color_manual(name = NULL,
                     values = pal2)+
  scale_x_continuous(breaks = rev(c(-2000, -1000, -500, -200, 0,
                                    200, 500, 1000, 2000)))+ #breaks = scales::pretty_breaks(n = 20)
  coord_cartesian(clip = "off")+
  # guides(color = guide_legend(reverse = TRUE))+
  facet_wrap(~Rodent_species,
             ncol = 2,
             labeller = as_labeller(c("Rattus rattus" = "Black rats",
                                      "Rattus norvegicus" = "Brown rats",
                                      "Rattus exulans" = "Pacific rats",
                                      "Mus musculus" = "House mice")))+
  # scale_color_discrete(name = NULL)+
  xlab("Years between rodent arrival and extinction/extirpation")+
  ylab(NULL)+
  theme_bw()+
  theme(#panel.border = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_text(size = 7),
        strip.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave("figures/timing/initial/timing supplemental.pdf", width = 14, height = 19)


key
dat[, test := paste(scientificName, Location)]

ggplot()+
  geom_vline(xintercept = 0)+
  geom_pointrange(data = dat,
                  aes(y = scientificName, xmin = min_difference,
                      x = median_difference,
                      xmax = max_difference, color = cat_arrival),
                  position = ggstance::position_dodgev(height = .5),
                  linewidth = 1)+ # position = position_jitter(height = .3)
  geom_pointrange(data = dat[test %in% key & 
                              cat_arrival == "Extinction at least 1,000 years after arrival", ],
                 aes(y = scientificName, xmin = min_difference,
                     x = median_difference,
                     xmax = max_difference),
                 color = "hotpink",
                 position = ggstance::position_dodgev(height = .5),
                 linewidth = 1)+ # position = position_jitter(height = .3)
  geom_text(data = annot, aes(x = x, y = y, 
                              label = lab),
            size = 3)+
  scale_color_manual(name = NULL,
                     values = pal2)+
  # scale_color_manual(NULL,
  #                    values = c("Rattus rattus" = "black",
  #                              "Rattus norvegicus" = "#934B00", ##502419",
  #                              "Rattus exulans" = "#247BA0",
  #                              "Mus musculus" = "#A8C69F"),
  #                   labels = c("Rattus rattus" = "Black rats",
  #                              "Rattus norvegicus" = "Brown rats",
  #                              "Rattus exulans" = "Pacific rats",
  #                              "Mus musculus" = "House mouse"))+
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 20))+
  scale_x_continuous(breaks = rev(c(-2000, -1000, -500, -200, 0,
                                    200, 500, 1000, 2000)))+ #breaks = scales::pretty_breaks(n = 20)
  coord_cartesian(clip = "off")+
  # guides(color = guide_legend(reverse = TRUE))+
  facet_wrap(~Rodent_species,
             ncol = 2,
             labeller = as_labeller(c("Rattus rattus" = "Black rats",
                                      "Rattus norvegicus" = "Brown rats",
                                      "Rattus exulans" = "Pacific rats",
                                      "Mus musculus" = "House mice")))+
  # scale_color_discrete(name = NULL)+
  xlab("Years between rodent arrival and extinction/extirpation")+
  ylab(NULL)+
  theme_bw()+
  theme(#panel.border = element_blank(),
    strip.background = element_blank(),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    legend.position = "bottom")


