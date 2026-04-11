#
#
# Clean up timing dataset and export a ready-to-plot build
#
#

rm(list = ls())

library("groundhog")

date <- "2024-07-15"
pcks <- c("data.table", "ggplot2", "tidyr", "readxl")
groundhog.library(pcks, date)

# groundhog.library("github::nickmckay/geoChronR",
#                   date)
# remotes::install_github("nickmckay/geoChronR")
# library("geoChronR")

# Load and clean data -----------------------------------------------------
dat <- read_excel("data/Raw/Timing_6-Dec_2024.xlsx")
dat
setDT(dat)
names(dat)
names(dat)[duplicated(names(dat))]

# Good.
names(dat)[grepl(" ", names(dat))]
names(dat)[grepl("/", names(dat))]
names(dat)[grepl("-", names(dat))]


# Convert CE to BP --------------------------------------------------------
names(dat)[grepl("Time_frame", names(dat))]
names(dat)
#
# convertAD2BP(0)
#
#
unique(dat$Bird_extirpation_year_earliest_Time_frame)
unique(dat$Bird_extirpation_year_earliest)
dat[, min_bird_extinction_year_BP := ifelse(Bird_extirpation_year_earliest_Time_frame == "CE",
                                            1950 - Bird_extirpation_year_earliest,
                                            Bird_extirpation_year_earliest)]
dat$min_bird_extinction_year_BP
#
unique(dat$Bird_extirpation_year_latest_Time_frame)
unique(dat$Bird_extirpation_year_latest)
dat[, max_bird_extinction_year_BP := ifelse(Bird_extirpation_year_latest_Time_frame == "CE",
                                            1950 - Bird_extirpation_year_latest,
                                            Bird_extirpation_year_latest)]
dat$max_bird_extinction_year_BP

#
unique(dat$Rodent_arrival_year_earliest_Time_frame)
unique(dat$Rodent_arrival_year_earliest)
dat[Rodent_arrival_year_earliest == 0, ]$Rodent_arrival_year_earliest_Time_frame
dat[Rodent_arrival_year_earliest == 0, ]$Rodent_species_attributed
dat[Rodent_arrival_year_earliest == 0, .(scientificName, Rodent_species)]

dat[, min_rodent_arrival_year_BP := ifelse(Rodent_arrival_year_earliest_Time_frame == "CE",
                                           1950 - Rodent_arrival_year_earliest,
                                            Rodent_arrival_year_earliest)]
dat$min_rodent_arrival_year_BP
dat[min_rodent_arrival_year_BP == 0, .(Rodent_arrival_year_earliest, 
                                       Rodent_arrival_year_earliest_Time_frame)]

#
unique(dat$Rodent_arrival_year_latest_Time_frame)
unique(dat$Rodent_arrival_year_latest)
dat[, max_rodent_arrival_year_BP := ifelse(Rodent_arrival_year_latest_Time_frame == "CE",
                                           1950 - Rodent_arrival_year_latest,
                                           Rodent_arrival_year_latest)]
dat$max_rodent_arrival_year_BP
dat[max_rodent_arrival_year_BP == 0, .(Rodent_arrival_year_latest, 
                                       Rodent_arrival_year_latest_Time_frame)]

# Calculate difference median/min/max -----------------------------------------

#' *In fox/cat review we calculated like this: *
# arrivals.bind[, `:=` (min_years_since = Year_last_record_min - max_arrival,
#                       max_years_since = Year_last_record_max - min_arrival)]


dat[, min_difference := min_bird_extinction_year_BP - max_rodent_arrival_year_BP]
dat[, .(Bird_extirpation_year_earliest, min_bird_extinction_year_BP,
        Rodent_arrival_year_earliest, min_rodent_arrival_year_BP, min_difference)]

dat[, max_difference := max_bird_extinction_year_BP - min_rodent_arrival_year_BP]
dat[, .(Bird_extirpation_year_latest, max_bird_extinction_year_BP,
        Rodent_arrival_year_latest, max_rodent_arrival_year_BP, max_difference)]


dat[, median_difference := (min_difference + max_difference) / 2]
dat

range(dat$median_difference)
dat
#' [Negative if rodent arrived AFTER extinction]
#' 
# Export for plotting -----------------------------------------------------

saveRDS(dat, "builds/timing_dataset_clean.Rds")

