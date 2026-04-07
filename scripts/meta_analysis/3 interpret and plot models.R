

# Clean environment and load packages -------------------------------------


rm(list = ls())

pacman::p_load("data.table", "ggplot2", "tidyr", "readxl",
                      "stringr", "dplyr", "metafor", "tidyr", "broom",
                      "ape")
# remotes::install_github("ropensci/rotl")
library("rotl")
# groundhog.library(pcks, date)
library("orchaRd")

theme_lundy <- theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank())

#
tidy_with_CIs <- function(m){
  tidy(m) %>%
    mutate(ci.lb = m$ci.lb,
           ci.ub = m$ci.ub) %>%
    return()
}


rma_predictions <- function(m, newgrid){
  
  #, removeIntercept = FALSE
  # https://stackoverflow.com/questions/63554740/predict-rma-onto-complex-new-data-in-metafor-polynomials-and-factor-levels
  
  if(!is.data.frame(newgrid)){errorCondition("ERROR newgrid must be a data frame")}
  #create the new model matrix. 
  
  if(!all(unlist(lapply(names(newgrid), # lapply through names of newgrid to check that they're in formula
                        function(x) grepl(pattern=x,
                                          x = as.character(m$formula.mods)[-1]))))){
    errorCondition("ERROR: variables in newgrid are not in model formula")
  }
  
  
  # Drop levels that might be missing from the model...
  cols <- names(newgrid)
  coef_nms <- names(coef(m))
  temp <- c()
  for(i in 1:length(cols)){
    if(class(unlist(newgrid[, cols[i], with = F])) %in% c("factor", "character")){
      temp <- paste0(names(newgrid[, cols[i], with = F]),
                     unlist(newgrid[, cols[i], with = F]))
      newgrid <- newgrid[temp %in% coef_nms, ]
    }
  }
  newgrid
  
  # Create prediction matrix
  predgrid <- (model.matrix(m$formula.mods, data=newgrid))
  predgrid
  
  if(any(grepl("intercept", colnames(predgrid), 
               ignore.case = TRUE))){
    #if intercept is present, remove it?
    predgrid <- predgrid[, -1]
  }
  
  # predict onto the new model matrix
  pred.out <- as.data.frame(predict(m, newmods=predgrid))
  
  #attach predictions to variables for plotting
  final.pred <- cbind(newgrid, pred.out)
  
  final.pred
}


unscale <- function(vec1, vec2){
  # vec1 is the vector to unscale (usually part of the prediction output)
  # vec2 is the scaled vector from the dataset
  scalar <- attr(vec2, "scaled:scale")
  center <- attr(vec2, "scaled:center")
  
  unscaled <- vec1 * scalar + center
  
  return(unscaled)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------------
# Load data ---------------------------------------------------------------
# Load original meta-analysis for reference
original <- read_excel("data/Raw/Meta-analysis-8-Dec.xlsx")
setDT(original)
original$Study_ID

# Load formatted meta and model guide
meta.final <- readRDS("builds/meta_analysis/final_meta_analytic_dataset_post_modeling.Rds")

guide.final <- readRDS("builds/meta_analysis/final_model_guide.Rds")

guide.final <- guide.final[best_model_random == "best_model"]

unique(guide.final$phylogeny)
#
unique(guide.final$random_formula)
# OK, simple.

guide.final

meta.final[is.na(Longevity_years)]

unique(guide.final$analysis_group)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Overall effects of rodents, sans moderators ------------------------------------
# 
guide.final[analysis_group == "Short-term reproduction" &
              moderators == "1", ]


sub.guide <- guide.final[moderators == "1", ]
sub.guide

ms <- lapply(sub.guide$model_path_base_model,
             readRDS)
ms

ms.tbl <- lapply(ms, tidy_with_CIs)
names(ms.tbl) <- sub.guide$model_id_base_model
ms.tbl
lapply(ms.tbl, setDT)
       
ms.tbl <- rbindlist(ms.tbl, idcol = "model_id_base_model")
ms.tbl

ms.tbl.mrg <- merge(ms.tbl,
                    guide.final[, .(model_id_base_model,
                                    analysis_group,
                                    exclude_converted_effect_sizes,
                                    exclusion, N, n_articles, n_Study_rodent, n_scientificName,
                                    I2_base)],
                    by = "model_id_base_model")
ms.tbl.mrg


ms.tbl.mrg[, model_name := ifelse(exclude_converted_effect_sizes == "no",
                                  analysis_group, paste(analysis_group, "w/o converted effect size"))]

ms.tbl.mrg[, I2_base := round(I2_base, 2)]
ms.tbl.mrg
ms.tbl.mrg[model_name != "Long-term abundance w/o converted effect size", .(model_name,
                                                                            estimate,ci.lb, ci.ub,
                                                                            statistic, p.value
                                                                            )]
range(ms.tbl.mrg$I2_base)


# >>> Create dataset for plotting ---------------------------------------------

plot_dat <- list()
i <- 1

for(i in 1:nrow(ms.tbl.mrg)){
  
  plot_dat[[i]] <- meta.final[eval(parse(text = ms.tbl.mrg[i, ]$exclusion))]
  plot_dat[[i]] <- cbind(plot_dat[[i]][, .(Article_ID, Article, Study_ID, effect_size_id,
                                           Original_study_design_effect_type, effect_size_type, Study_rodent,
                                          scientificName, effect_size, sampling_variance)],
                         ms.tbl.mrg[i, .(analysis_group)])
  
}
names(plot_dat) <- ms.tbl.mrg$model_name
plot_dat <- rbindlist(plot_dat, 
                      idcol = "model_name")

plot_dat
unique(plot_dat$model_name)
# >>> Plot ------------------------------------------------------------------

# There is no SMD for Reproduction.
# ms.tbl.mrg <- ms.tbl.mrg[!(effect_size == "Combined_OR_SMD" & response == "Reproduction")]
# plot_dat <- plot_dat[!(effect_size == "Combined_OR_SMD" & response == "Reproduction")]

plot_dat[model_name == "Before-after eradication abundance"]

# Recalculate sample size:
Ns <- plot_dat[, .(N = uniqueN(effect_size_id),
                   refs = uniqueN(Article),
                   n_rodents = uniqueN(Study_rodent),
                   n_birds = uniqueN(scientificName)),
               by = .(model_name)]

Ns[, N_string := paste0(N, "(", refs, ",", 
                        #n_rodents, ",",
                        n_birds, ")")]

Ns[, N_string_alt := paste0(refs, " articles;\n", 
                        n_birds, " birds")]

unique(Ns$N_string)
unique(Ns$N_string_alt)

Ns[model_name == "Before-after eradication abundance"]

unique(plot_dat$model_name)
#
unique(plot_dat$Study_rodent)
plot_dat[Study_rodent == "Rattus rattus, Rattus norvegicus, Rattus exulans", 
         Study_rodent := "Rattus"]

pal <- c("Rattus" = "grey70",
         "Rattus rattus" = "#2E282A",
         "Rattus norvegicus" = "#DB504A",
         "Rattus exulans" = "#016FB9"
         )

plot_dat$Study_rodent <- factor(plot_dat$Study_rodent,
                                levels = c("Rattus rattus", 
                                           "Rattus norvegicus",
                                           "Rattus exulans",
                                           "Rattus"))
Ns$Study_rodent <- factor(Ns$Study_rodent,
                                levels = c("Rattus rattus", 
                                           "Rattus norvegicus",
                                           "Rattus exulans",
                                           "Rattus"))
ms.tbl.mrg$Study_rodent <- factor(ms.tbl.mrg$Study_rodent,
                          levels = c("Rattus rattus", 
                                     "Rattus norvegicus",
                                     "Rattus exulans",
                                     "Rattus"))

unique(plot_dat$model_name)
plot_dat$model_name <- factor(plot_dat$model_name,
                              levels = rev(c("Long-term abundance",
                                             "Long-term abundance w/o converted effect size",
                                         "Short-term abundance",
                                         "Before-after eradication abundance",
                                         "Short-term reproduction",
                                         "Before-after eradication reproduction")))
Ns$model_name <- factor(Ns$model_name,
                              levels = rev(c("Long-term abundance",
                                             "Long-term abundance w/o converted effect size",
                                         "Short-term abundance",
                                         "Before-after eradication abundance",
                                         "Short-term reproduction",
                                         "Before-after eradication reproduction")))
ms.tbl.mrg$model_name <- factor(ms.tbl.mrg$model_name,
                              levels = rev(c("Long-term abundance",
                                             "Long-term abundance w/o converted effect size",
                                         "Short-term abundance",
                                         "Before-after eradication abundance",
                                         "Short-term reproduction",
                                         "Before-after eradication reproduction")))
#

p <- ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_jitter(data = plot_dat[model_name != "Long-term abundance w/o converted effect size"], 
              aes(x = effect_size, y = model_name, size = (1/sampling_variance),
                                   text = Study_ID,
                                   color = Study_rodent),
              # shape = 21, 
              alpha = 0.5,
              # fill = "grey50"
              width = 0,
              height = .1)+
  annotate(geom = "label", y = 5.5, x = -2.5, label = "Negative association",
           label.size = NA)+
  annotate(geom = "label", y = 5.5, x = 2.5, label = "Positive association",
           label.size = NA)+
  geom_pointrange(data = ms.tbl.mrg[model_name != "Long-term abundance w/o converted effect size"], 
                  aes(y = model_name, xmin = ci.lb, xmax = ci.ub,
                                         x = estimate),
                  shape = 21,
                  size = 1, fill = "grey")+
  geom_text(data = Ns[!model_name %in% c("Long-term abundance w/o converted effect size",
                                         "Before-after eradication abundance")], 
            aes(y = model_name, x = -4.3, label = N_string_alt),
            size = 3)+
  geom_text(data = Ns[model_name == "Before-after eradication abundance"], 
            aes(y = model_name, x = -4.3, label = N_string_alt),
            color = "indianred",
            size = 3)+
  scale_color_manual(name = NULL,
                    values = pal,
                    labels = c("Rattus" = "Rat",
                               "Rattus rattus" = "Black rat",
                               "Rattus norvegicus", "Brown rat",
                               "Rattus exulans" = "Pacific rat"))+
  # facet_wrap(~response, ncol = 2, scales = "free_x")+
  guides(size = "none")+
  ylab(NULL)+
  xlab("Effect size")+
  theme_lundy+
  theme(axis.text = element_text(color = "black"),
        legend.position = "bottom")
p

ggsave("figures/meta_analysis/initial_figs/overall_effects.png", width = 10, height = 7)
ggsave("figures/meta_analysis/initial_figs/overall_effects.pdf", width = 10, height = 7)

library("plotly")

# p <- ggplotly(p)
# p
# htmlwidgets::saveWidget(p, "figures/meta_analysis/initial_figs/overall_effects.html")

#
plot_dat <- plot_dat[model_name != "Long-term abundance w/o converted effect size"]
plot_dat[, .(n_effects = uniqueN(effect_size_id),
             refs = uniqueN(Article),
             birds = uniqueN(scientificName))]

plot_dat[, abundance_reproduction := ifelse(grepl("abundance", analysis_group),
                                            "abundance", "reproduction")]
plot_dat[, .(n_effects = uniqueN(effect_size_id),
             refs = uniqueN(Article),
             birds = uniqueN(scientificName)),
         by = .(abundance_reproduction)]

plot_dat[, .(n_effects = uniqueN(effect_size_id),
             refs = uniqueN(Article),
             birds = uniqueN(scientificName)),
         by = .(effect_size_type, abundance_reproduction)]


plot_dat[abundance_reproduction == "abundance", .(n_effects = uniqueN(effect_size_id),
             refs = uniqueN(Article),
             birds = uniqueN(scientificName)),
         by = .(effect_size_type, abundance_reproduction)]

plot_dat[abundance_reproduction == "reproduction", .(n_effects = uniqueN(effect_size_id),
                                                  refs = uniqueN(Article),
                                                  birds = uniqueN(scientificName)),
         by = .(effect_size_type, abundance_reproduction)]


# >>> Colored by bird species ---------------------------------------------

ggplot()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_jitter(data = plot_dat[model_name != "Long-term abundance w/o converted effect size"], 
              aes(x = effect_size, y = model_name, size = (1/sampling_variance),
                  text = Study_ID,
                  color = scientificName),
              # shape = 21, 
              alpha = 0.75,
              # fill = "grey50"
              width = 0,
              height = .1)+
  annotate(geom = "label", y = 5.5, x = -2.5, label = "Negative association",
           label.size = NA)+
  annotate(geom = "label", y = 5.5, x = 2.5, label = "Positive association",
           label.size = NA)+
  geom_pointrange(data = ms.tbl.mrg[model_name != "Long-term abundance w/o converted effect size"], 
                  aes(y = model_name, xmin = ci.lb, xmax = ci.ub,
                      x = estimate))+
  geom_text(data = Ns[model_name != "Long-term abundance w/o converted effect size"], 
            aes(y = model_name, x = -4.3, label = N_string),
            size = 3)+
  # scale_color_manual(name = NULL,
  #                    values = pal,
  #                    labels = c("Rattus" = "Rat",
  #                               "Rattus rattus" = "Black rat",
  #                               "Rattus norvegicus", "Brown rat",
  #                               "Rattus exulans" = "Pacific rat"))+
  # facet_wrap(~response, ncol = 2, scales = "free_x")+
  guides(size = "none")+
  ylab(NULL)+
  xlab("Effect size")+
  theme_lundy+
  theme(axis.text = element_text(color = "black"),
        legend.position = "bottom")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Significant moderators ------------------------------------

sub.guide <- guide.final[moderators != "1" &
                           LRT_pval < 0.05, ]
sub.guide

ms <- lapply(sub.guide$model_path_base_model,
             readRDS)
ms
names(ms) <- sub.guide$moderators

# Let's do these one at a time....
ms

# >>> Reproduction ~ min-time-since ---------------------------------------

sub.dat <- meta.final[eval(parse(text = sub.guide[moderators == "min_time_since" &
                                                    analysis_group == "Short-term reproduction"]$exclusion)), ]
sub.dat$effect_size_type

unique(sub.dat$min_time_since)
sub.dat[, min_time_since_scaled := scale(min_time_since)]

m <- readRDS(sub.guide[moderators == "min_time_since" &
                         analysis_group == "Short-term reproduction"]$model_path_base_model)

range(sub.dat$min_time_since_scaled)
pred <- rma_predictions(m,
                        newgrid = data.table(min_time_since_scaled = seq(-1.1,
                                                                          1.0,
                                                                          by = .1)))
pred

# Unscale.
pred[, min_time_since := unscale(min_time_since_scaled,
                                  sub.dat$min_time_since_scaled)]
range(pred$min_time_since)
range(sub.dat$min_time_since)
# what the hell. but ti's plotting at much lower than those values
unique(sub.dat$min_time_since)
# 
# sub.guide[moderators == "min_time_since"]$response
sub.dat$effect_size_type

ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(data = pred, aes(x = min_time_since, ymin = ci.lb, ymax = ci.ub,),
              fill = "grey80", alpha = .5)+
  geom_line(data = pred, aes(x = min_time_since, y = pred))+
  geom_jitter(data = sub.dat, 
              aes(x = min_time_since, y = effect_size, size = 1/sampling_variance,
                  fill = effect_size_type),
              shape = 21,
              width = 0)+
  guides(size = "none")+
  ylab("Effect on short-term reproduction (ZCOR and OR converted to SMD)")+
  theme_lundy

# This looks like it's driven just by teh effect size type...
ggsave("figures/meta_analysis/initial_figs/effect_by_longevity.png", width = 8, height = 8)
# opposite than we expected


# NOT ENOUGH SAMPLE SIZE FOR THESE:
# >>> Longevity -----------------------------------------------------------
ms$Longevity_years
sub.dat <- meta.final[eval(parse(text = sub.guide[moderators == "Longevity_years"]$exclusion)), ]
sub.dat$effect_size_type

unique(sub.dat$Longevity_years)
sub.dat[, Longevity_years_scaled := scale(Longevity_years)]

pred <- rma_predictions(ms$Longevity_years,
                        newgrid = data.table(Longevity_years_scaled = seq(-0.7,
                                                                          1.5,
                                                                          by = .1)))
pred

# Unscale.
pred[, Longevity_years := unscale(Longevity_years_scaled,
                                  sub.dat$Longevity_years_scaled)]
range(pred$Longevity_years)
range(sub.dat$Longevity_years)
# what the hell. but ti's plotting at much lower than those values
unique(sub.dat$Longevity_years)

sub.guide[moderators == "Longevity_years"]$response
sub.guide[moderators == "Longevity_years"]$effect_size

ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(data = pred, aes(x = Longevity_years, ymin = ci.lb, ymax = ci.ub),
              fill = "grey80", alpha = .5)+
  geom_line(data = pred, aes(x = Longevity_years, y = pred))+
  geom_jitter(data = sub.dat, aes(x = Longevity_years, y = effect_size, size = 1/sampling_variance),
              width = 0)+
  guides(size = "none")+
  ylab("Effect on long-term presence/absence (log odds ratio)")+
  theme_lundy

ggsave("figures/meta_analysis/initial_figs/effect_by_longevity.png", width = 8, height = 8)
# opposite than we expected


# >>> Longevity -----------------------------------------------------------
ms$Longevity_years
sub.guide[moderators == "Clutch_size_mean"]

sub.dat <- meta.final[eval(parse(text = sub.guide[moderators == "Clutch_size_mean"]$exclusion)), ]
sub.dat$effect_size_type

unique(sub.dat$Clutch_size_mean)
sub.dat[, Clutch_size_mean_scaled := scale(Clutch_size_mean)]

unique(sub.dat$Clutch_size_mean_scaled)
pred <- rma_predictions(ms$Clutch_size_mean,
                        newgrid = data.table(Clutch_size_mean_scaled = seq(-0.6,
                                                                          1.6,
                                                                          by = .1)))
pred

# Unscale.
pred[, Clutch_size_mean := unscale(Clutch_size_mean_scaled,
                                  sub.dat$Clutch_size_mean_scaled)]
range(pred$Clutch_size_mean)
range(sub.dat$Clutch_size_mean)
# what the hell. but ti's plotting at much lower than those values
unique(sub.dat$Clutch_size_mean)

sub.guide[moderators == "Clutch_size_mean"]$analysis_group

ggplot()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_ribbon(data = pred, aes(x = Clutch_size_mean, ymin = ci.lb, ymax = ci.ub),
              fill = "grey80", alpha = .5)+
  geom_line(data = pred, aes(x = Clutch_size_mean, y = pred))+
  geom_jitter(data = sub.dat, aes(x = Clutch_size_mean, y = effect_size, size = 1/sampling_variance),
              width = 0)+
  guides(size = "none")+
  xlab("Clutch size (mean)")+
  ylab("Effect on long-term presence/absence (log odds ratio)")+
  theme_lundy

ggsave("figures/meta_analysis/initial_figs/effect_by_clutch_size.png", width = 8, height = 8)
# opposite than we expected

# this is not enough data

# >>> min_time_since -----------------------------------------------------------
names(ms)
sub.guide[moderators == "min_time_since"]
# neither is this
# 
# ms$min_time_since
# sub.dat <- meta.final[eval(parse(text = sub.guide[moderators == "min_time_since"]$exclusion)), ]
# sub.dat
# 
# unique(sub.dat$min_time_since)
# sub.dat[, min_time_since_scaled := scale(min_time_since)]
# 
# range(sub.dat$min_time_since_scaled)
# pred <- rma_predictions(ms$min_time_since,
#                         newgrid = data.table(min_time_since_scaled = seq(-0.52,
#                                                                           2.45,
#                                                                           by = .1)))
# pred
# 
# # Unscale.
# pred[, min_time_since := unscale(min_time_since_scaled,
#                                   sub.dat$min_time_since_scaled)]
# range(pred$min_time_since)
# range(sub.dat$min_time_since)
# # what the hell. but ti's plotting at much lower than those values
# unique(sub.dat$min_time_since)
# 
# sub.guide[moderators == "min_time_since"]$response
# sub.guide[moderators == "min_time_since"]$effect_size
# 
# ggplot()+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   geom_ribbon(data = pred, aes(x = min_time_since, ymin = ci.lb, ymax = ci.ub),
#               fill = "grey80", alpha = .5)+
#   geom_line(data = pred, aes(x = min_time_since, y = pred))+
#   geom_jitter(data = sub.dat, aes(x = min_time_since, y = effect, size = 1/variance),
#               width = 0)+
#   guides(size = "none")+
#   ylab("Effect on abundance (SMD)")+
#   theme_lundy
# ggsave("figures/meta_analysis/initial_figs/effect_by_minyears.png", width = 8, height = 8)



# >>> max_time_since -----------------------------------------------------------
names(ms)
ms$max_time_since
sub.guide[moderators == "max_time_since"]

# sub.dat <- meta.final[eval(parse(text = sub.guide[moderators == "max_time_since"]$exclusion)), ]
# sub.dat
# 
# unique(sub.dat$max_time_since)
# sub.dat[, max_time_since_scaled := scale(max_time_since)]
# 
# range(sub.dat$max_time_since_scaled)
# pred <- rma_predictions(ms$max_time_since,
#                         newgrid = data.table(max_time_since_scaled = seq(-0.56,
#                                                                          2.25,
#                                                                          by = .1)))
# pred
# 
# # Unscale.
# pred[, max_time_since := unscale(max_time_since_scaled,
#                                  sub.dat$max_time_since_scaled)]
# range(pred$max_time_since)
# range(sub.dat$max_time_since)
# # what the hell. but ti's plotting at much lower than those values
# unique(sub.dat$max_time_since)
# 
# sub.guide[moderators == "max_time_since"]$response
# sub.guide[moderators == "max_time_since"]$effect_size
# 
# ggplot()+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   geom_ribbon(data = pred, aes(x = max_time_since, ymin = ci.lb, ymax = ci.ub),
#               fill = "grey80", alpha = .5)+
#   geom_line(data = pred, aes(x = max_time_since, y = pred))+
#   geom_jitter(data = sub.dat, aes(x = max_time_since, y = effect, size = 1/variance),
#               width = 0)+
#   guides(size = "none")+
#   ylab("Effect on abundance (combined OR and SMD)")+
#   theme_lundy
# 
# ggsave("figures/meta_analysis/initial_figs/effect_by_maxyears.png", width = 8, height = 8)
# 
