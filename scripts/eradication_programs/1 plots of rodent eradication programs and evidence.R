#Aim: To plot island rodent eradication programs that overlap with threatened/extinct bird species and it's evidence 

# 1. Loading packages -----------------------------------------------------
library(data.table)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(cowplot)

# 2. Load data and convert to sf ------------------------------------------
#Plots theme 
theme_lundy <- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

#For world map plots
world_sf <- st_as_sf(ne_countries(scale = "medium", returnclass = "sf"))
#world_sf <- st_as_sf(world_sf, crs = "+proj=longlat +datum=WGS84 +no_defs")
world_sf <- st_as_sf(world_sf, crs = 4326)

#For the factor levels and plots
evidence_levels <- c(
  "No studies", 
  "No study in support", 
  "Only predation in support",
  "Lethal program in support",
  "Population study without data in support",
  "Population study with data in support",
  "Population study with all qualities in support"
)

plot_col<-c(
  "No studies" = "black",
  "No study in support" = "grey20",
  "Only predation in support" = "grey50",
  "Lethal program in support" = "indianred4",
  "Population study without data in support" = "dodgerblue4",
  "Population study with data in support" = "dodgerblue2",
  "Population study with all qualities in support" = "dodgerblue2"
)

col_pal <- c("No studies" = "transparent",
             "No study in support" = "transparent",
             "Only predation in support" = "transparent",
             "Lethal program in support" = "transparent",
             "Population study without data in support" = "transparent",
             "Population study with data in support" = "transparent",
             "Population study with all qualities in support" = "gold"
)

#Frequency of support
study_freq <- fread("builds/eradication_programs/study_freq.csv")

#Data for each rodent species
m_musculus_island <- readRDS("builds/eradication_programs/m_musculus_island.rds")
r_exulans_island <- readRDS("builds/eradication_programs/r_exulans_island.rds")
r_norvegicus_island <- readRDS("builds/eradication_programs/r_norvegicus_island.rds")
r_rattus_island <- readRDS("builds/eradication_programs/r_rattus_island.rds")

#Overall data for eradication programs
Rodents_overlap_data <- fread("builds/eradication_programs/Rodents_overlap_data.csv")

# 3. Organize data for plots -----------------------------------------------------
#Make sure the best evidence is on top (for ploting)
prepare_island_data <- function(dt) {
  dt <- dt[order(match(dt$synth_col, names(plot_col))), ]
  dt[, border_col := ifelse(synth_col == "Population study with all qualities in support", 
                            "gold", "transparent")]
  dt
}

rodent_list <- list(
  list(data = prepare_island_data(m_musculus_island),   title = "House mouse"),
  list(data = prepare_island_data(r_exulans_island),    title = "Pacific rats"),
  list(data = prepare_island_data(r_norvegicus_island), title = "Brown rats"),
  list(data = prepare_island_data(r_rattus_island),     title = "Black rats")
)

all_eradications <- rbindlist(lapply(rodent_list, `[[`, "data"), use.names = TRUE, fill = TRUE)
all_eradications[, synth_col := factor(synth_col, levels = evidence_levels)]
all_eradications <- all_eradications[order(Island.Name, -as.integer(synth_col))]
all_eradications[, border_col := ifelse(synth_col == "Population study with all qualities in support",
                                        "gold", "transparent")]

## Figure 3 -----------
##Evidence basis for global island rodent eradication programs for the conservation of threatened birds found in a systematic review
all_eradications<- st_as_sf(all_eradications, crs = 4326)

#Main map
main_map <- ggplot() +
  geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +  # base map
  geom_sf(data = all_eradications,
          aes(fill = synth_col, color = synth_col),
          shape = 21, size = 1.5, stroke = 0.5) +
  scale_fill_manual(values = plot_col) +
  scale_color_manual(values = col_pal) +  
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 2, stroke = 1))) +
  theme_minimal() +
  labs(color="", fill="") +
  theme(
    legend.position = "bottom",
    axis.text = element_blank()
  ) +
  coord_sf(crs = "+proj=moll")

main_map 

#Main barplot
all_eradications$synth_col <- factor(all_eradications$synth_col, 
                                     levels = rev(levels(all_eradications$synth_col)))
main_barplot<- ggplot(all_eradications,
                      aes(reorder(Rodent_attributed_final, Rodent_attributed_final, function(x) -length(x)),
                          fill=synth_col, color=synth_col)) +
  geom_bar(stat = "count", linewidth=.75) +
  scale_fill_manual(values = plot_col)+
  scale_color_manual(values = col_pal) +
  ylab("Number of eradications")+
  xlab("")+
  scale_x_discrete(labels = c(c("Rattus rattus" = "Black rats", 
                                "Rattus norvegicus" = "Brown rats",
                                "Rattus exulans" = "Pacific rats",
                                "Mus musculus" = "House mouse")))+
  
  theme_lundy+
  theme(legend.position = "none")

main_barplot

#Add frequency of support and not in support studies for each evidence category
p_study_freq <- ggplot(data = study_freq[Evidence_category_effect != "NONE"],
                       aes(x = number_studies,
                           y = Evidence_category_effect,
                           fill = Evidence_category_effect_data,
                           color = Quality_Study_Highlight))+
  geom_col(linewidth = 1, width = .5)+
  geom_vline(xintercept = 0)+
  scale_color_manual(values = c("Highlight" = "gold",
                                "No" = "transparent"))+
  annotate(geom = "text", x = -50, y = 5.4,
           label = "Not in support", size = 3.3, hjust = 0,
           color = "grey30")+
  annotate(geom = "text", x = 50, y = 5.4,
           label = "In support", size = 3.3, hjust = 0,
           color = "grey30")+
  scale_fill_manual(values = c("Predation" = "grey50",
                               "Lethal program Reproductive success" = "indianred4",
                               "Lethal program Abundance" = "indianred4",
                               "Population Reproductive success 0" = "dodgerblue4",
                               "Population Abundance 0" = "dodgerblue4",
                               "Population Reproductive success 1" = "dodgerblue2",
                               "Population Abundance 1" = "dodgerblue2"))+ #
  scale_y_discrete(labels = c("Predation" = "Predation",
                              "Lethal program Reproductive success" = "Lethal program\n(reproductive success)",
                              "Lethal program Abundance" = "Lethal program\n(abundance)",
                              "Population Reproductive success" = "Population\n(reproductive success)",
                              "Population Abundance" = "Population\n(abundance)"))+
  guides(fill = "none", color = "none")+
  ylab(NULL)+
  xlab("Number of studies")+
  theme_lundy+
  theme(axis.text.y = element_text(hjust=0),
        axis.ticks.y = element_blank())

p_study_freq

#Now complete the final figure
main_figure_right <- cowplot::plot_grid(main_barplot, p_study_freq, ncol= 1, labels = c("B", "C"))
main_figure <- cowplot::plot_grid(main_map, main_figure_right, nrow = 1, labels = c("A", ""), rel_widths = c(1, .4))
main_figure

## Figure S3 -----------
#Function to create map and barplots for each rodent species
make_rodent_plots <- function(df, title) {
  df <- st_as_sf(df, crs = 4326)
  
  map <- ggplot() +
    geom_sf(data = world_sf, fill = "gray90", color = "black", size = 0.1) +
    geom_sf(data = df, aes(fill = synth_col, color = synth_col),
            shape = 21, size = 1.5, stroke = 0.5) +
    scale_fill_manual(values = plot_col) +
    scale_color_manual(values = col_pal) +
    guides(fill = guide_legend(override.aes = list(shape = 21, size = 2, stroke = 1))) +
    theme_minimal() +
    labs(title = title, color = "", fill = "") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
          axis.text = element_blank()) +
    coord_sf(crs = "+proj=moll")
  
  barplot <- ggplot(df, aes(synth_col, fill = synth_col)) +
    geom_bar(stat = "count") +
    scale_fill_manual(values = plot_col) +
    xlab("Evidence") + ylab("Number of eradications") +
    labs(title = paste("Evidence per", title, "eradication")) +
    theme_lundy +
    theme(legend.position = "none")
  
  list(map = map, barplot = barplot)
}

#Generate all plots in one line
plots <- lapply(rodent_list, function(x) make_rodent_plots(x$data, x$title))

#To access individual plots
#plots[[1]]$map      # house mouse map
#plots[[1]]$barplot  # house mouse barplot

#Now complete the final figure
all_rodent <- cowplot::plot_grid(
  plots[[4]]$map, plots[[3]]$map, plots[[2]]$map, plots[[1]]$map,
  nrow = 2, labels = "AUTO"
)

#Legend (from black rats)
leg <- plots[[4]]$map + theme(legend.position = "bottom")
legend <- cowplot::get_plot_component(leg, 'guide-box-bottom')
legend <- cowplot::ggdraw(legend)

all_rodent <- cowplot::plot_grid(all_rodent, legend, ncol = 1, rel_heights = c(1, .08))
all_rodent
