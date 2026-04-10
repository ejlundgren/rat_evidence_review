#
#
#
# Clean environment and load packages -------------------------------------
rm(list = ls())

pacman::p_load("data.table", "ggplot2", "tidyr", "readxl",
                      "stringr", "dplyr", "metafor", "tidyr", "broom",
                      "ape", "rotl", "orchaRd", "gt")


theme_RATS <- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------------
# Load data ---------------------------------------------------------------

# Load prepped data and model results
meta.final <- readRDS("builds/meta_analysis/final_meta_analytic_dataset_post_modeling.Rds")

guide.final <- readRDS("builds/meta_analysis/model_summaries.Rds")
guide.final <- guide.final[exclude_converted_effect_sizes == "no", ]

unique(guide.final$phylogeny)

#
unique(guide.final$random_formula)
# OK, simple.

guide.final

unique(guide.final$analysis_group)

# >>> Plot ------------------------------------------------------------------

guide.final[analysis_group == "Before-after eradication abundance"]

guide.final[, N_string := paste0(n_observations, "(", n_articles, ",", 
                                      #n_rodents, ",",
                                 n_birds, ")")]

guide.final[, N_string_alt := paste0(n_articles, " articles;\n", 
                        n_birds, " birds")]


unique(meta.final$Study_rodent)
meta.final[Study_rodent == "Rattus rattus, Rattus norvegicus, Rattus exulans", 
         Study_rodent := "Rattus"]

pal <- c("Rattus" = "#466995",
         "Rattus rattus" = "#2E282A",
         "Rattus norvegicus" = "#7D4600",
         "Rattus exulans" = "#BDA0BC"
         )

meta.final$Study_rodent <- factor(meta.final$Study_rodent,
                                levels = c("Rattus rattus", 
                                           "Rattus norvegicus",
                                           "Rattus exulans",
                                           "Rattus"))

unique(meta.final$analysis_group)
meta.final$analysis_group <- factor(meta.final$analysis_group,
                              levels = rev(c("Long-term abundance",
                                               "Short-term abundance",
                                               "Before-after eradication abundance",
                                               "Short-term reproduction",
                                               "Before-after eradication reproduction")))

guide.final$analysis_group <- factor(guide.final$analysis_group,
                              levels = rev(c("Long-term abundance",
                                             "Long-term abundance w/o converted effect size",
                                         "Short-term abundance",
                                         "Before-after eradication abundance",
                                         "Short-term reproduction",
                                         "Before-after eradication reproduction")))
#

p <- ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_jitter(data = meta.final, 
              aes(y = effect_size, x = analysis_group, 
                  size = sqrt((1/sampling_variance)) + 2,
                 fill = Study_rodent),
              shape = 21,
              alpha = 0.7,
              width = .1,
              height = 0)+
  geom_errorbar(data = guide.final[analysis_group != "Before-after eradication abundance"], 
                aes(x = analysis_group, ymin = ci.lb, ymax = ci.ub),
                width = .1)+
  geom_errorbar(data = guide.final[analysis_group == "Before-after eradication abundance"], 
                aes(x = analysis_group, ymin = ci.lb, ymax = ci.ub),
                color = "grey50",
                width = .1)+
  geom_pointrange(data = guide.final[analysis_group != "Before-after eradication abundance"], 
                  aes(x = analysis_group, ymin = pi.lb, ymax = pi.ub,
                      y = pred),
                  shape = 21,
                  size = 1, fill = "grey")+
  geom_pointrange(data = guide.final[analysis_group == "Before-after eradication abundance"], 
                  aes(x = analysis_group, ymin = pi.lb, ymax = pi.ub,
                      y = pred),
                  shape = 21, alpha = 0.5,
                  size = 1, fill = "grey")+
  geom_text(data = guide.final[analysis_group != "Before-after eradication abundance"], 
            aes(x = analysis_group, y = -8, label = N_string_alt),
            size = 3, vjust = 1.5)+
  geom_text(data = guide.final[analysis_group == "Before-after eradication abundance"], 
            aes(x = analysis_group, y = -8, label = N_string_alt),
            color = "red",
            size = 3, vjust = 1.5)+
  scale_size_identity()+
  scale_fill_manual(name = NULL,
                    values = pal,
                    labels = c("Rattus" = "Rat",
                               "Rattus rattus" = "Black rat",
                               "Rattus norvegicus", "Brown rat",
                               "Rattus exulans" = "Pacific rat"))+
  scale_x_discrete(breaks = c("Long-term abundance",
                              "Short-term abundance",
                              "Before-after eradication abundance",
                              "Short-term reproduction",
                              "Before-after eradication reproduction"),
                   labels = c("Long-term abundance",
                              "Short-term abundance",
                              "Before-after eradication\nabundance",
                              "Short-term reproduction",
                              "Before-after eradication\nreproduction"))+
  guides(size = "none")+
  guides(fill = guide_legend(override.aes = list(size=4)))+
  coord_flip()+
  xlab(NULL)+
  ylab("Association between rats and birds")+
  theme_RATS+
  theme(axis.text = element_text(color = "black"),
        legend.position = "bottom")
p

ggsave("figures/meta_analysis/overall_effects.png", width = 7, height = 9)
ggsave("figures/meta_analysis/overall_effects.pdf", width = 10, height = 7)


# >>> Table ---------------------------------------------------------------
guide.final$analysis_group <- factor(guide.final$analysis_group,
                                     levels = c("Long-term abundance", "Short-term abundance",
                                                "Before-after eradication abundance",
                                                "Short-term reproduction", "Before-after eradication reproduction"))

gt.tab <- guide.final %>%
  mutate(`Sample size` = paste("Rodents = ", n_rodents, "<br>",
                               "Birds = ", n_birds, "<br>",
                               "Articles = ", n_articles,"<br>",
                               "Observations =", n_observations)) %>%
  mutate(`I2` = paste("$I^2_{total}$ = ", round(I2_Total, 2), "<br>",
                      "$I^2_{article}$ = ", round(I2_Article_ID, 2), "<br>",
                      "$I^2_{obs}$ = ", round(`I2_Article_ID/effect_size_id`, 2))) %>%
  # mutate(header = paste(analysis_group, `Sample size`, I2, sep = " | ")) %>%
  select(analysis_group,
         pred, se, ci.lb, ci.ub, pi.lb, pi.ub, t, df, pval,
         `Sample size`, `I2`) %>%
  rename("Estimate" = "pred",
         "Lower CI" = "ci.lb",
         "Upper CI" = "ci.ub",
         "Lower PI" = "pi.lb",
         "Upper PI" = "pi.ub",
         "p" = "pval") %>%
  # group_by(header) %>%
  gt(groupname_col = "analysis_group") %>%
  fmt_number(columns = everything(), decimals = 2) %>%
  fmt_number(columns = "df", decimals = 0) %>%
  fmt_markdown(columns = I2) %>%
  fmt_markdown(columns = `Sample size`) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) %>%
  opt_table_font(
    size = 12
  )
gt.tab
gtsave(gt.tab, filename = "figures/meta_analysis/model table.pdf")
