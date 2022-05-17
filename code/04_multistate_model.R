### Meta ###

# Author: Andreas Höhn
# Version: 1.0
# Date:  2022-05-09
# About: This module estimates the multistate model and does all predictions 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [0] Load Data & Ensure Correct Variable Type ###

# cohort in long-format # 
cohort_long <- data.table::data.table(read.csv("data/cohort_long.csv",
                                               header = TRUE))
cohort_long[, bday := base::as.Date(bday)]
cohort_long[, bday_center := base::as.Date(bday_center)]
cohort_long[, dday := base::as.Date(dday)]
cohort_long[, study_entry := base::as.Date(study_entry)]
cohort_long[, study_exit  := base::as.Date(study_exit)]
cohort_long[, episode_start_date  := base::as.Date(episode_start_date)]
cohort_long[, episode_end_date    := base::as.Date(episode_end_date)]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [1] LOGICAL CHECKS BEFORE START ###
stopifnot(
  dim(cohort_long[episode_start_date > episode_end_date,])[[1]] == 0)

stopifnot(
  dim(cohort_long[study_entry  > study_exit])[[1]] == 0 &
  dim(cohort_long[dday         > study_exit])[[1]] == 0)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [2] Generate Start State Distribution ###

# start state distribution is based on the age range defined in the definitions
start_states <- .StartStates(definitions = definitions,
                             input_data = cohort_long)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [3] Preparation for Multistate Modelling ###

# Store Transition Matrix in List - we need two different formats 
M_trans <- list()
M_trans$Q <- data.table::data.table(rbind(
          c(0,1,1),
          c(1,0,1),
          c(0,0,0)))

# Reproduce Identical Transition Matrix in "tmat Format" for Predictions 
M_trans$tmat = mstate::transMat(x = list(
        c(2,3),
        c(1,3),
        c()),
        names = c("active","inactive","dead"))

# Put this in Table Format for Report #
table_transitions_allowed <- data.table::data.table(M_trans$tmat)

# number of transitions is what is not NA in M_trans[[2]] ("tmat")
no_of_trans <- sum(!is.na(M_trans[[2]]))

# reshape long-format into multistate long-format / need Q format here
cohort_msm <- msm::msm2Surv(data = cohort_long, Q = M_trans$Q, 
                            subject = "id", time = "age_center",
                            state = "state")

# Check Correctness of Conversion based on Death Counts
.ReshapeCheck(data_long_format = cohort_long,
              data_msm = cohort_msm)

# we need all relevant variables to have the correct variable type
# msm::msm2Surv conversion typically goes with the simplest variable type (int)
cohort_msm$trans <- as.factor(cohort_msm$trans)
cohort_msm$men   <- as.factor(cohort_msm$men)

# table transitions by sex - ensure there are no NA transitions 
table_transitions_descriptive <- data.table::data.table(table(
  cohort_msm$trans[cohort_msm$status==1],cohort_msm$men[cohort_msm$status==1]))
setnames(table_transitions_descriptive, "V1", "Transition")
setnames(table_transitions_descriptive, "V2", "Men")
table_transitions_descriptive[, Men := ifelse(Men == 0, "Women", "Men")]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [4] Estimate and Visualize Multistate Model ###

# estimate models for different distributions
model_list_gompertz <-  .FunctionMSM(data = cohort_msm, no_of_trans = no_of_trans,
                   formula = definitions$formula_msm, distribution = "gompertz")

model_list_weibull <-  .FunctionMSM(data = cohort_msm, no_of_trans = no_of_trans,
                    formula = definitions$formula_msm, distribution = "weibull")

model_list_expo   <-  .FunctionMSM(data = cohort_msm, no_of_trans = no_of_trans,
                    formula = definitions$formula_msm, distribution = "exponential")

model_list_lognorm  <-  .FunctionMSM(data = cohort_msm, no_of_trans = no_of_trans,
                    formula = definitions$formula_msm, distribution = "lognormal")

# Compare Models and Create Overview # 
model_comparison <- vector(no_of_trans, mode="list")
for (j in 1:no_of_trans) {
  model_comparison[[j]] <- AIC(
    model_list_gompertz[[j]],
    model_list_weibull[[j]],
    model_list_expo[[j]],
    model_list_lognorm[[j]])
}
model_comparison <- data.table::rbindlist(model_comparison, idcol = TRUE)
model_comparison <- cbind(data.table::data.table(
          Transition   = rep(1:no_of_trans, each = 4),
          Distribution = rep(rep(c("Gompertz","Weibull","Exponential","lognormal"),
                                 times = no_of_trans))), model_comparison)
model_comparison[, best_AIC := min(AIC), by = Transition]
model_comparison[AIC == best_AIC, best_fit := 1, by = Transition]

model_comparison_best <- copy(model_comparison[best_fit == 1,
                  .(Transition, Distribution, AIC)])
print(paste("Overview of Best Fitting Distributions",model_comparison_best))

# ------------- # 

# !!! NOTE: THIS NEEDS TO BE DONE BY HAND, NOT GREAT - UPDATE ASAP !!!

# select best fitting distributions         
model_list <- vector(no_of_trans, mode="list")
model_list[[1]] <- model_list_weibull[[1]]
model_list[[2]] <- model_list_weibull[[2]]
model_list[[3]] <- model_list_gompertz[[3]]
model_list[[4]] <- model_list_weibull[[4]]

# ------------- # 

# create and print a table for all transition-specific models 
model_table <- rbind(
  .ModelTable(model_list[[1]], trans = 1, num_parameters = 3),
  .ModelTable(model_list[[2]], trans = 2, num_parameters = 3),
  .ModelTable(model_list[[3]], trans = 3, num_parameters = 3),
  .ModelTable(model_list[[4]], trans = 4, num_parameters = 3))
model_table[Parameter == "shape", Parameter := "Shape"]
model_table[Parameter == "rate", Parameter  := "Rate"]
model_table[Parameter == "men1", Parameter  := "Males (Ref: Females)"]

# visualize model fit  
png(filename = "plots/overview_model_fit.png",
    width = definitions$fig_width, height = definitions$fig_height)
par(mfrow = c(2,2))
plot(model_list[[1]], main = "Transition 1", xlab="age centered", 
     xlim = c(0, (definitions$age_max - definitions$age_center)))
plot(model_list[[2]], main = "Transition 2", xlab="age centered", 
     xlim = c(0, (definitions$age_max - definitions$age_center)))
plot(model_list[[3]], main = "Transition 3", xlab="age centered", 
     xlim = c(0, (definitions$age_max - definitions$age_center)))
plot(model_list[[4]], main = "Transition 4", xlab="age centered", 
     xlim = c(0, (definitions$age_max - definitions$age_center)))
dev.off()


# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [5.1] Estimate State Expectancies ###

# function - based 
result_msm <- .EstimateStateExp(model_list = model_list,
                                tmat = M_trans$tmat,
                                start_states = start_states,
                                definitions = definitions)
  
### [5.2] Bootstrap 95% CIs ###

# create empty list
result_msm_boot <- vector(definitions$bootstraps, mode = "list")

# State Expectancy Bootstrap for 95% CIs #  
for (k in 1:definitions$bootstraps){
result_msm_boot[[k]] <- .EstimateStateExp95CI(cohort_msm_input = cohort_msm,
                              start_states_input = start_states, 
                              tmat = M_trans$tmat, definitions = definitions)
print(paste("bootstrapping:", k, "out of", definitions$bootstraps))
}

# Transfer list to data table and modify #
result_msm_95CI <- .BindBootCI(result_msm_boot)

# Prepare for Merging # 
setkey(result_msm_95CI, men, type)
result_msm <- merge(result_msm, result_msm_95CI, by = c("men", "type"))
# Calculate 95% CI #
result_msm[, LE_upper := LE + (sd * 1.96)][,
             LE_lower := LE - (sd * 1.96)]
result_msm[, conf  := paste0("(",sprintf("%.1f",round(LE_lower,
                                          definitions$rounding_results)),
                             "-",
                             sprintf("%.1f",round(LE_upper,
                                          definitions$rounding_results)),")")]

# visualize
plot_msm_results <- ggplot(result_msm, aes(fill = type, y = LE, x = men)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values=c(RColorBrewer::brewer.pal(9,"YlGn")[5],
                             RColorBrewer::brewer.pal(9,"YlOrRd")[3],
                             RColorBrewer::brewer.pal(9,"YlOrRd")[5])) +
  scale_y_continuous(limits = c(0,55),
                     breaks = seq(0, 55, 10),
                     name = "Years of Life\n") +
  scale_x_discrete(breaks = seq(0, 1),
                     label = c("Women","Men"), name = NULL) +
  geom_text(aes(x = men, y = LE + 3.5,
            label = sprintf("%.1f", round(LE, definitions$rounding_results))),
            position = position_dodge(width = 1), size = 3) +
  geom_text(aes(x = men, y = LE + 1.5, label = conf),
            position = position_dodge(width = 1), size = 3) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    legend.text     = element_text(size=12),
    legend.key      = element_rect(fill = NA, color = NA),
    legend.key.size = unit(1.00, "line"),
    axis.text.x  = element_text(size=14, vjust=0.5),
    axis.text.y  = element_text(size=14),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    strip.text.x = element_text(size=20),
    strip.background = element_rect(colour=NA,
                                    fill=NA))+ 
  guides(fill = guide_legend(reverse = FALSE)) +
  theme(strip.text.x = element_text(size=14),
        strip.background = element_rect(colour="black",
                                        fill="white")) 
# save plot
ggsave(plot = plot_msm_results, filename = "plots/plot_msm_results.png",
       device = "png", width = definitions$gg_fig_width,
       height = definitions$gg_fig_height, units = "cm")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [6] Start State Specific Probabilities of State Occupancy ###

# create empty data.table for predictions containing the model covariates 
empty_data <- data.table::data.table(men = as.factor(c(0,1)))
setkey(empty_data, men)

# create corresponding list containing all covariate - transition combinations
empty_data_list <- vector(dim(empty_data)[1], mode="list")
for (i in 1:dim(empty_data)[1]) {
  empty_data_list[[i]] <- data.table(
    rbind(empty_data[i], empty_data[i],
          empty_data[i],empty_data[i]),
    trans=c(1,2,3,4))
}

# Query Results for Men/Women from Function
results_prob_sex <- rbind(.ProbStateOccupancy(empty_data = empty_data[1], 
                            empty_data_list = empty_data_list[[1]],
                            sex = "Women", model_list = model_list,
                            tmat = M_trans$tmat,definitions = definitions),
                          .ProbStateOccupancy(empty_data = empty_data[2], 
                             empty_data_list = empty_data_list[[2]],
                             sex = "Men", model_list = model_list,
                             tmat = M_trans$tmat,definitions = definitions))
                           
# Plot Results 
plot_prob_sex <- 
  ggplot2::ggplot(results_prob_sex, aes(x = age, y = prob, colour = men)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = men),
              size = 0.05, alpha = 0.3, show.legend=FALSE) +
  geom_line(aes(color = men), size = 0.5) + 	
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2),
                     name="Probability to be in State & 95% CI\n ",
                     sec.axis = dup_axis(name=NULL)) +
  scale_x_continuous(limits = c(definitions$age_center, definitions$age_max),
                breaks = seq(definitions$age_center, definitions$age_max, 20),
                     name="Age") +
  guides(colour=guide_legend(title = "Sex", ncol = 1, byrow = FALSE),
         override.aes = list(size = 12,alpha=2)) + 
  facet_wrap( ~ type, ncol=3) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    legend.text     = element_text(size=12),
    legend.key      = element_rect(fill = NA, color = NA),
    legend.key.size = unit(1.00, "line"),
    axis.text.x  = element_text(size=12, vjust=0.5),
    axis.text.y  = element_text(size=12),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    strip.text.x = element_text(size=12),
    strip.background = element_rect(colour = "black", fill = "white")) 

# save plot
ggsave(plot = plot_prob_sex, filename = "plots/plot_prob_sex.png",
       device = "png", width = definitions$gg_fig_width,
       height = definitions$gg_fig_height, units = "cm")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [7] Disaggregated "Stacked" Survival Curves  ###

# Query stacked survival probabilities
results_stacked_sex <- .StackSurvProbs(results_prob_sex)

# corresponding plot 
plot_stacked_sex <- 
  ggplot(results_stacked_sex, aes(x = age, fill = type)) +
  geom_ribbon(aes(ymax=max, ymin=min, fill=type), alpha=1) +
  scale_fill_manual(values=c(RColorBrewer::brewer.pal(9,"YlGn")[5],
                             RColorBrewer::brewer.pal(9,"YlOrRd")[3],
                             RColorBrewer::brewer.pal(9,"YlOrRd")[5])) +
  scale_y_continuous(limits=c(0,1),
                     breaks=seq(0,1,0.2),
                     name="Survival Probability\n") +
  scale_x_continuous(limits = c(definitions$age_center, definitions$age_max),
                     breaks = seq(definitions$age_center, definitions$age_max, 20),
                     name="Age") +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    legend.text     = element_text(size=12),
    legend.key      = element_rect(fill = NA, color = NA),
    legend.key.size = unit(1.00, "line"),
    axis.text.x  = element_text(size=14, vjust=0.5),
    axis.text.y  = element_text(size=14),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    strip.text.x = element_text(size=20),
    strip.background = element_rect(colour=NA,
                                    fill=NA))+ 
  guides(fill = guide_legend(reverse = FALSE)) +
  facet_wrap( ~ men, ncol=2) +
  theme(strip.text.x = element_text(size=14),
        strip.background = element_rect(colour="black",
                                        fill="white")) 
# save plot
ggsave(plot = plot_stacked_sex, filename = "plots/plot_stacked_sex.png",
       device = "png", width = definitions$gg_fig_width,
       height = definitions$gg_fig_height, units = "cm")
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
