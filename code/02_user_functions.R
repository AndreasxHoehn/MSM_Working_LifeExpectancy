### Meta ###

# Author: Andreas H?hn
# Version: 1.0
# Date:  2022-05-09
# About: this file contains all user-written functions 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [0] Install Required Packages if not already installed + Load ###

# input: a character vector containing the names of all required packages
# output: downloads all required Packages if not already installed and loads them

.EnsurePackages <- function(packages_vector) {
  new_package <- packages_vector[!(packages_vector %in% 
                                     installed.packages()[, "Package"])]
  if (length(new_package) == TRUE) {
    install.packages(new_package, dependencies = TRUE) }
  sapply(packages_vector, suppressPackageStartupMessages(require),
         character.only = TRUE)
}
.EnsurePackages(RequiredPackages)
here <- here::here
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [1] Create Cohort Function ###

# input: definitions with parameters for population
# output: a baseline-level cohort table with 1 row = 1 individual

.CreateCohort <- function(definitions) {
  
  # Baseline - Level Frame 
  data <- data.table::data.table(
    id    = seq(1:definitions$register_size),
    men   = c(rep(0, times = definitions$register_size/2),
            rep(1, times = definitions$register_size/2)),
    bday  = sample(x = seq(definitions$cohorts_range[1],
                         definitions$cohorts_range[2], by = "day"),
                 size = definitions$register_size, replace = TRUE), 
   life_length = round(c(rnorm(definitions$register_size/2,
                              definitions$LE[1],definitions$LE_sd[1]),
                        rnorm(definitions$register_size/2,
                              definitions$LE[2],definitions$LE_sd[2])), 
                      definitions$rounding))
  
  # date of death is based on simulated birthdays and lengths of life's 
  data[, dday        := bday + (life_length * definitions$year_length)]
  
  # we need the birthday of the center age for inclusion / exclusion into study 
  data[, bday_center := bday + 
                    (definitions$age_center * definitions$year_length)]
  
  # explicit return
  return(data)
  }

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #


### [2] Long-Format Episode Split ###

# input: baseline-level table with study entry and study exit information for
#        each individual + definitions with parameters for episode length
# output: a long-format study cohort based on episode length

# function for general episode splitting
.EpisodeSplitCohort  <- function(data_input,  definitions) {
  # specify columns to be carried over, default = all 
  all_columns   <- colnames(data_input)
  # create episodes based on episode length and framed by start / end dates
  data_output <- data_input[, .(episode = 1:ceiling(
    as.numeric((study_exit + 1 - study_entry) /
                 definitions$episode_length))), by = all_columns]
  # helper variable: maximum number of episodes
  data_output[, episode_max := max(episode), by = id]
  # define episode start dates
  data_output[, episode_start_date := (study_entry + ((episode - 1) * 
                                         definitions$episode_length))]
  # define episode end dates 
  data_output[, episode_end_date := episode_start_date + 
                (definitions$episode_length - 1)]
  data_output[(episode_max == episode) & (episode_end_date > study_exit),
              episode_end_date := study_exit]
  data_output$episode_max <- NULL
  # set key for time-updated overlapping join 
  setkey(data_output, id, episode_start_date, episode_end_date)
  # extend the last episode until study exit
  data_output[, drop := 0]
  data_output[episode_start_date >= episode_end_date, drop := 1]
  data_output <- data_output[drop == 0]
  data_output[, episode_max := max(episode), by = id]
  data_output[(episode == episode_max) & (episode_end_date < study_exit),
              episode_end_date := study_exit, by = id]
  # episode length
  data_output[, episode_length := round(as.numeric(episode_end_date - 
                        episode_start_date) /  definitions$year_length, 2)]
 
  # chronological age #
  data_output[, age := round(as.numeric(episode_end_date - bday) / 
                        definitions$year_length, definitions$rounding)]
  
  # chronological age centered # 
  data_output[, age_center := age - definitions$age_center]
  
   # data minimization
  data_output$drop           <- NULL
  
  # logical check: typically triggered by study_entry > study exit
  stopifnot(dim(data_output[episode < 0,])[[1]] == 0) # no negative episodes 
  
  # explicit return
  return(data_output)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [3] Simulate Employment ###

# input: definitions, long-format study cohort
# output: long-format study cohort with simulated employment Status

.SimEmployment <- function(definitions, input_data) {
  
  # hardcopy input data 
  output_data <- copy(input_data)
  
  # initialize active state 
  output_data[, state := 1]
  output_data[age >= definitions$age_retirement, state := 2] 
  
  # chance to become inactive is lower for men than for women
  output_data[men == 0 & age < definitions$age_retirement,
          state := state + rbinom(1, 1,(age-definitions$prob_inactive[1])/100),
          by = c("id","episode")]
  output_data[men == 1 & age < definitions$age_retirement,
          state := state + rbinom(1, 1, (age-definitions$prob_inactive[2])/100)
          , by = c("id","episode")]
    
  # staying inactive probability
  il_vector <- seq(1,definitions$age_retirement,1)
  
  for(i in 1:length(il_vector)) {
    output_data[men == 0 & age < definitions$age_retirement &  
          id == shift(id, il_vector[i], type = "lag") &
          state == 1 & shift(state, il_vector[i], type = "lag") == 2,
          state := state + rbinom(1, 1, definitions$prob_stay[1]), by = "id"] 
  }
  
  for(i in 1:length(il_vector)) {
    output_data[men == 1 & age < definitions$age_retirement & 
          id == shift(id, il_vector[i], type = "lag") &
          state == 1 & shift(state, il_vector[i], type = "lag") == 2,
          state := state + rbinom(1, 1, definitions$prob_stay[2]), by = "id"] 
  }
  
  # absorbing states 
  output_data[dday == episode_end_date, state := 3, by = "id"]
  
  # ensure correct variable type 
  output_data$state <- as.factor(output_data$state)
  
  # explicit return
  return(output_data)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [4] Start State Distribution ###

# input: definitions, long-format study cohort
# output: a wide-format table with start state distribution for men / women 

.StartStates <- function(definitions, input_data) {
  
  # subset input data 
  data_output <- copy(input_data[episode == 1 & age >= definitions$age_center &
                                   age <  definitions$age_dist_limit, ])
  
  # create data.table to be filled 
  data_output <- data.table::data.table(
    men = c(0,1),
    state1 = as.vector(table(data_output$men, data_output$state)[,1]),
    state2 = as.vector(table(data_output$men, data_output$state)[,2]),
    N      = c(sum(as.vector(table(data_output$men, data_output$state)[1,])),
               sum(as.vector(table(data_output$men, data_output$state)[2,]))))
  
  # relative frequency 
  data_output[, state1 := round(state1 / N, definitions$rounding)]
  data_output[, state2 := round(state2 / N, definitions$rounding)]

  # explicit return
  return(data_output)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [5] Examine the Reshaped Data Set ###

# input: baseline cohort, long-format cohort, msm converted cohort
# output: a message whether number of death matches in all three data sets 

.ReshapeCheck <- function(data_long_format, data_msm) {
  # extract unique IDs #
  id2 <- data_long_format[!is.na(dday), .(unique(id)) ]
  id3 <- copy(data.table::data.table(data_msm))
  id3 <- id3[!is.na(dday), .(unique(id))]
  # length checks based on vector length 
  if (length(id2) == length(id3)){
    print("continue: correct number of deaths after msm2Surv reshape")
  }
  # otherwise return error warning
  else { print("warning: incorrect number of deaths after msm2Surv conversion")}
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [6] Estimate Transition - Specific Models  ###

# input: no_of_trans, reshaped data set, formula for parametric model, distribution
# output: a list with all parametric survival models 

# modelling function
.FunctionMSM <- function(no_of_trans, data, formula, distribution) {
  
  # allocate an empty list for every transition-specific model 
  model_list <- vector(no_of_trans, mode = "list")
  print(paste("Estimating for Distribution:", distribution))
  # fill allocated list with transition-specific models 
  for(i in 1:no_of_trans)  {
    print(paste("Estimating Transition No:",i))
    model_list[[i]] <- flexsurv::flexsurvreg(as.formula(base::noquote(formula)),
                                             subset = (trans==i), data = data, 
                                             dist = distribution)
  }
  # explicit return
  return(model_list)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [7] Estimate Start-State - Independent State Expectancies  ###

# input: the exact model object from the result list, the corresponding 
#        transition number, the number of parameters, 
# output: a table showing all transition-specific models 

.ModelTable <- function(model_input, trans, num_parameters) {
  
  # initialize table 
  data_output <- data.table::data.table(
    Trans    = trans,
    Parameter= names(model_input$res[,"est"])[1:num_parameters],
    est      = model_input$res[,"est"][1:num_parameters],
    L95      = model_input$res[,"L95%"][1:num_parameters],
    H95      = model_input$res[,"U95%"][1:num_parameters],
    dist     = model_input$dlist$name)
   
  # format table
  data_output[, est :=  sprintf("%.2f",round(est,2))]
  data_output[, L95 :=  sprintf("%.2f",round(L95,2))]
  data_output[, H95 :=  sprintf("%.2f",round(H95,2))]	

  # explicit return
  return(data_output)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [8] Estimate Start-State - Independent State Expectancies  ###

# input: model_list, transition matrix in tmat format, distribution of start 
#        states, definitions
# output: start-state - independent state expectancy estimates 

# Estimate State Expectancy Function #  
.EstimateStateExp <- function(model_list, tmat,
                              start_states, definitions) {
  
  data_output <- data.table::data.table(men = as.factor(c(0,1)))
  setkey(data_output, men)
  
  data_output_list <- vector(dim(data_output)[1], mode = "list")
  
  for (i in 1:dim(data_output)[1]) {
    data_output_list[[i]] <- data.table(
      rbind(data_output[i],data_output[i],data_output[i],data_output[i]),
      trans = c(1,2,3,4))
  }
  
  
  # make predictions and store results in a list 
  results_list <- vector(dim(data_output)[1], mode="list")
  
  for (i in 1:dim(data_output)[1]){ 
    results_list[[i]] <- flexsurv::totlos.fs(model_list,
            trans = tmat, t = definitions$age_max - definitions$age_center,
            newdata = data_output_list[[i]], ci = FALSE, sing.inf = 1e+100)
  }
  
  # ----------------------- #
  
  # total life expectancy
  life_expectancy_total <- rep(99, dim(data_output)[1])
  for (i in 1:dim(data_output)[1]) {
    life_expectancy_total[i] <- 
      (results_list[[i]][1,1] * start_states$state1[i])  + 
      (results_list[[i]][1,2] * start_states$state1[i])  + 
      (results_list[[i]][2,1] * start_states$state2[i])  + 
      (results_list[[i]][2,2] * start_states$state2[i])   						
  }
  data_output_1 <- copy(data_output)
  data_output_1[, LE := life_expectancy_total][, type := as.factor("Total")]
  
  # ----------------------- #
  
  # active life expectancy 
  life_expectancy_state1 <- rep(99, dim(data_output)[1])
  for (i in 1:dim(data_output)[1]) {
    life_expectancy_state1[i] <- 
      (results_list[[i]][1,1] * start_states$state1[i])  +
      (results_list[[i]][2,1] * start_states$state2[i])   	
  }
  data_output_2 <- copy(data_output)
  data_output_2[, LE := life_expectancy_state1][,
                                        type := as.factor("State 1: Active")]
  
  # ------------------------# 
  
  # inactive life expectancy 
  life_expectancy_state2 <- rep(99, dim(data_output)[1])
  for (i in 1:dim(data_output)[1]) {
    life_expectancy_state2[i] <- 
      (results_list[[i]][1,2] * start_states$state1[i])  +
      (results_list[[i]][2,2] * start_states$state2[i])   	
  }
  data_output_3 <- copy(data_output)
  data_output_3[, LE := life_expectancy_state2][,
                                        type := as.factor("State 2: Inactive")]
  
  # combine # 
  data_output <- rbind(data_output_1, data_output_2, data_output_3)
  
  # ------------------------# 
  
  # round # 
  data_output[, LE := round(LE, definitions$rounding)]
  
  # ------------------------# 
  
  # explicit return return
  return(data_output)
  
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [9] Bootstrap 95% CIs for Start-State - Independent State Expectancies  ###

# input: the msm-format cohort table, transition matrix in tmat format, 
#        distribution of start states, definitions
# output: start-state - independent state expectancy estimates 

# Estimate State Expectancy Function #  
.EstimateStateExp95CI <- function(cohort_msm_input,
                                  start_states_input, 
                                  tmat, definitions) {
  
  # ---------------------- #
  
  # Sample with Replacement of Actual Study Population Size ###
  # initialize lists and vectors #
  sample_from     <- copy(data.table(cohort_msm_input))
  subset_sample   <- vector(length(unique(sample_from$id)), mode = "list")
  sample_id       <- sample(unique(sample_from$id), 
                            size = length(unique(sample_from$id)),
                            replace = TRUE)
  # sampling with replacement of study pop size with new id #
  for (b in 1:length(sample_id)) {	
    subset_sample[[b]] <- sample_from[sample_from$id %in% sample_id[b],]
    subset_sample[[b]][, id := NULL][, id := b]
  }
  # transform lists to data.table
  sample_data <- rbindlist(subset_sample)	
  
  # ----------------------- #
  
  ### ESTIMATE MODELS BASED ON BEST FIT MODELS ###
  # empty list #
  model_list_sample <- vector(no_of_trans, mode="list")
  # transitions 1 - 4 # 
  model_list_sample[[1]] <- flexsurv::flexsurvreg(
    as.formula(base::noquote(definitions$formula_msm)),
    subset = (trans == 1), data = sample_data,
    dist = definitions$distributions_msm[1])
  model_list_sample[[2]] <- flexsurv::flexsurvreg(
    as.formula(base::noquote(definitions$formula_msm)),
    subset = (trans == 2), data = sample_data,
    dist = definitions$distributions_msm[2])
  model_list_sample[[3]] <- flexsurv::flexsurvreg(
    as.formula(base::noquote(definitions$formula_msm)),
    subset = (trans == 3), data = sample_data,
    dist = definitions$distributions_msm[3])
  model_list_sample[[4]] <- flexsurv::flexsurvreg(
    as.formula(base::noquote(definitions$formula_msm)),
    subset = (trans == 4), data = sample_data,
    dist = definitions$distributions_msm[4])
  
  # ----------------------- #
  
  data_output <- data.table::data.table(men = as.factor(c(0,1)))
  setkey(data_output, men)
  
  data_output_list <- vector(dim(data_output)[1], mode = "list")
  
  for (i in 1:dim(data_output)[1]) {
    data_output_list[[i]] <- data.table(
      rbind(data_output[i], data_output[i], data_output[i], data_output[i]),
      trans = c(1,2,3,4))
  }
  
  
  # make predictions and store results in a list 
  results_list <- vector(dim(data_output)[1], mode = "list")
  
  for (i in 1:dim(data_output)[1]){ 
    results_list[[i]] <- flexsurv::totlos.fs(model_list_sample,
        trans = M_trans$tmat, t = definitions$age_max - definitions$age_center,
        newdata = data_output_list[[i]], ci = FALSE, sing.inf = 1e+100)
  }
  
  # ----------------------- #
  
  # total life expectancy
  life_expectancy_total <- rep(99, dim(data_output)[1])
  for (i in 1:dim(data_output)[1]) {
    life_expectancy_total[i] <- 
      (results_list[[i]][1,1] * start_states_input$state1[i])  + 
      (results_list[[i]][1,2] * start_states_input$state1[i])  + 
      (results_list[[i]][2,1] * start_states_input$state2[i])  + 
      (results_list[[i]][2,2] * start_states_input$state2[i])   						
  }
  data_output_1 <- copy(data_output)
  data_output_1[, LE := life_expectancy_total][, type := as.factor("Total")]
  
  # ----------------------- #
  
  # active life expectancy 
  life_expectancy_state1 <- rep(99, dim(data_output)[1])
  for (i in 1:dim(data_output)[1]) {
    life_expectancy_state1[i] <- 
      (results_list[[i]][1,1] * start_states_input$state1[i])  +
      (results_list[[i]][2,1] * start_states_input$state2[i])   	
  }
  data_output_2 <- copy(data_output)
  data_output_2[, LE := life_expectancy_state1][,
                                                type := as.factor("State 1: Active")]
  
  # ------------------------# 
  
  # inactive life expectancy 
  life_expectancy_state2 <- rep(99, dim(data_output)[1])
  for (i in 1:dim(data_output)[1]) {
    life_expectancy_state2[i] <- 
      (results_list[[i]][1,2] * start_states_input$state1[i])  +
      (results_list[[i]][2,2] * start_states_input$state2[i])   	
  }
  data_output_3 <- copy(data_output)
  data_output_3[, LE := life_expectancy_state2][,
                                                type := as.factor("State 2: Inactive")]
  
  # combine # 
  data_output <- rbind(data_output_1, data_output_2, data_output_3)
  
  # ------------------------# 
  
  # round # 
  data_output[, LE := round(LE, definitions$rounding)]
  
  # ------------------------# 
  
  # explicit return # 
  return(data_output)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [10] Bind Bootstrap Results ###

# input: the list containing all boots-trap data sets
# output: a data set with the standard errors for on these estimates 

.BindBootCI <- function(data_input){ 
  data_input <- data.table::rbindlist(data_input, idcol = TRUE)
  setkey(data_input, men, type)
  data_input[, counter := .GRP, by=c("men","type")]
  data_output <- data.table()
  for (i in 1:length(unique(data_input$counter))) {
    subset_msm_boot <- data_input[counter == i, ] 
    LE_sort         <- sort(subset_msm_boot$LE)
    LE_sort_sd      <- data.table(sd = sd(LE_sort))
    subset_msm_boot <- subset_msm_boot[.id == 1,][, c(".id","LE") := NULL]
    rbind_data      <- cbind(LE_sort_sd, subset_msm_boot)
    data_output   <- rbind(data_output, rbind_data)
  }
  return(data_output)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [11] Estimate Start-State - Specific Probabilities of State Occupancy ###

# input:  an empty data object outlining the covariates we have in the model, a 
#         list with the corresponding covariate - transition combination, a 
#         string for sex, a tmat object, definitions
# output: start-state - specific probabilities of state occupancy 

.ProbStateOccupancy <- function(empty_data, empty_data_list, sex,
                                model_list, tmat, definitions) {
  
  # predict and fill list
  results_list <- flexsurv::pmatrix.fs(model_list, trans = tmat,
                      t = (0:(definitions$age_max - definitions$age_center)),
                      newdata = empty_data_list, ci = TRUE, B = 100)
  
  # ---------# 
  
  # Extract: Active # 
  vector_prob <- rep(NA, times = definitions$age_max - definitions$age_center+1) 
  vector_lower <- rep(NA, times = definitions$age_max - definitions$age_center+1) 
  vector_upper <- rep(NA, times = definitions$age_max - definitions$age_center+1) 
  
  for (i in 1:(definitions$age_max - definitions$age_center+1)) {
    vector_prob[[i]] <- 	
      as.matrix(as.data.table(results_list[[i]][[1,1]]))
    vector_lower[[i]] <- 	
      as.matrix(as.data.table(attr(results_list[[i]], "lower")[1,1]))
    vector_upper[[i]] <- 	
      as.matrix(as.data.table(attr(results_list[[i]], "upper")[1,1]))
  }
  
  # combine #
  results_state1<- data.table::data.table( 
    age  = seq(definitions$age_center, definitions$age_max, 1),
    prob = vector_prob,
    lower = vector_lower,
    upper = vector_upper,
    type = as.factor("active"),
    men = as.factor(sex))
  
  # ---------# 
  
  # Extract: Inactive # 
  vector_prob <- rep(NA, times = definitions$age_max - definitions$age_center+1) 
  vector_lower <- rep(NA, times = definitions$age_max - definitions$age_center+1) 
  vector_upper <- rep(NA, times = definitions$age_max - definitions$age_center+1) 
  
  for (i in 1:(definitions$age_max - definitions$age_center+1)) {
    vector_prob[[i]] <- 
      as.matrix(as.data.table(results_list[[i]][[1,2]]))
    vector_lower[[i]] <- 
      as.matrix(as.data.table(attr(results_list[[i]], "lower")[1,2]))
    vector_upper[[i]] <- 
      as.matrix(as.data.table(attr(results_list[[i]], "upper")[1,2]))
  }
  
  # combine #
  results_state2<- data.table::data.table( 
    age  = seq(definitions$age_center, definitions$age_max, 1),
    prob = vector_prob,
    lower = vector_lower,
    upper = vector_upper,
    type = as.factor("inactive"),
    men = as.factor(sex))
  
  # ---------# 
  
  # Extract: Death # 
  vector_prob <- rep(NA, times = definitions$age_max - definitions$age_center+1) 
  vector_lower <- rep(NA, times = definitions$age_max - definitions$age_center+1) 
  vector_upper <- rep(NA, times = definitions$age_max - definitions$age_center+1) 
  
  for (i in 1:(definitions$age_max - definitions$age_center+1)) {
    vector_prob[[i]] <- 
      as.matrix(as.data.table(results_list[[i]][[1,3]]))
    vector_lower[[i]] <- 
      as.matrix(as.data.table(attr(results_list[[i]], "lower")[1,3]))
    vector_upper[[i]] <- 
      as.matrix(as.data.table(attr(results_list[[i]], "upper")[1,3]))
  }
  # combine #
  results_state <- data.table::data.table( 
    age  = seq(definitions$age_center,definitions$age_max,1),
    prob = vector_prob,
    lower = vector_lower,
    upper = vector_upper,
    type = as.factor("dead"),
    men = as.factor(sex))
  
  # ---------# 
  
  # Combine all
  data_output <- data.table::as.data.table(rbind(
    results_state1,
    results_state2,
    results_state))
  
  # ---------# 
  
  # recode
  data_output[prob  < 0 & type != "dead", prob  := 0]
  data_output[lower < 0 & type != "dead", lower := 0]
  data_output[upper < 0 & type != "dead", upper := 0]
  data_output[age >= definitions$age_max - 10 & type == "dead", prob := 1]
  data_output[age >= definitions$age_max - 10 & type == "dead", lower := 1]
  data_output[age >= definitions$age_max - 10 & type == "dead", upper:= 1]
  
  # explicit result
  return(data_output)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [12] Stack Probabilities of State Occupancy for disaggregated S' Curves ###

# input:  a long-format dataset containing all start-state - specific 
#         probabilities of state occupancy 
# output: start-state - specific probabilities of state occupancy stacked 

.StackSurvProbs <- function(data_input) {
  
  # hard copy data with start state specific probabilities of state occupancy
  prob_data <- copy(data_input)
  
  # set initials 
  state_1_ini <- copy(prob_data[type == "active", .(age, prob, type, men)])
  state_2_ini <- copy(prob_data[type == "inactive", .(age, prob, type, men)])
  state_3_ini <- copy(prob_data[type == "dead", .(age, prob, type, men)])
  
  # set finals 
  state_1_fin <- copy(prob_data[type == "actives", .(age, prob, type, men)])
  state_2_fin <- copy(prob_data[type == "inactive", .(age, prob, type, men)])
  state_3_fin <- copy(prob_data[type == "dead", .(age, prob, type, men)])
  
  # modify finals: state 1
  state_1_fin  <- state_1_ini
  state_1_fin$max <- state_1_fin$prob
  state_1_fin$min <- 0
  
  # modify finals: state 2
  state_2_fin$prob <- state_1_ini$prob + state_2_ini$prob
  state_2_fin$max  <- state_2_fin$prob
  state_2_fin$min  <- state_1_fin$max
  
  # death fin
  state_3_fin$max <- 1
  state_3_fin$min <- state_2_fin$max 
  
  # combine 
  data_final <- rbind(state_1_fin, state_2_fin, state_3_fin)
  
  # explicit return
  return(data_final)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [13] Data Preparation for Chiang 1984 LTB  ###

# input: a long-format cohort table
# output: a data set with Nx, Dx

# Prepare Data Function
.DataPrepChiang <- function(data_input, definitions) {
  
  data_mort <- copy(data_input)
  
  # identify deaths #
  data_mort[, death := 0]
  data_mort[dday <= episode_end_date & dday >= episode_start_date,
            death := 1, by="id"]
  
  data_mort[, age := round((as.numeric(episode_end_date - bday)) / 
                                         definitions$year_length, 4)]
  data_mort[, age := trunc(age, prec = 0)]
  data_mort[, age_band := cut(age, c(-1, seq(9,99,10), Inf),
                              include.lowest=FALSE, left=FALSE)]
  levels(data_mort$age_band) <- c("0-9","10-19","20-29","30-39","40-49",
                          "50-59","60-69","70-79","80-89","90-99","100p")
  
  # Death Counts #
  deaths <- data_mort[, .(Dx = sum(death)), by="age_band"]
  setkey(deaths, age_band)
  
  # Exposure Pop #
  exposure <- data_mort[, .(Nx = sum(episode_length)), by="age_band"]
  setkey(exposure, age_band)
  
  result <- merge(deaths, exposure, by="age_band")
  
  # return
  return(result)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [14] Estimate Chiang 1984 LTB  ###

# input: a data set prepared for life table estimates
# output: a data set with ltbs

.LTBChiang <- function(input_data) {
  
  # Deaths and Exposure
  Dx <- input_data$Dx
  Nx <- input_data$Nx
  
  age_band   <- input_data$age_band
  age_band   <- droplevels(age_band)
  age_band   <- levels(as.factor(age_band))
  
  n                <- c(rep(10, length(age_band)-1),0)
  ax               <- c(rep(5,  length(age_band)-1),0)
  mx               <- Dx/Nx
  qx               <- (n*mx) / (1+(n-ax)*mx)
  qx[length(qx)]   <- 1.0
  px               <- 1.0-qx
  lx               <- rep(x=0, times=length(mx))
  lx[1]            <- 100000
  for (i in 2:(length(lx))){
    lx[i]          <- lx[i-1] * px[i-1]
  }
  dx              <- lx*qx
  dx[length(dx)]  <- lx[[length(dx)]]
  Lx              <- n*c(lx[-1],0) + ax * dx
  Lx[length(Lx)]  <- lx[length(Lx)]/mx[length(Lx)]
  Tx              <- rev(cumsum(rev(Lx)))
  ex              <- Tx/lx
  ltb_return <- data.table::data.table(age_band,
                n, Nx, Dx, mx, qx, ax, lx, dx, Lx, Tx, ex)
  # explicit return
  return(ltb_return)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [15] Estimate LE based on a two-state parametric model ###

# input: the long-format cohort table, and definitions
# output: estimates  of LE based 

.ParSurvLE <- function(data_input, definitions) {
  
  # copy dataset 
  data_input <- copy(cohort_long)
  
  # alive - death states 
  data_input[, state_bin := 1]
  data_input[dday == episode_end_date, state_bin := 2]
  
  # REDUCE DATA SET #
  data_input <- data_input[,c("id","men","episode","age_center","state_bin")]
  
  # Specify the transitions that are allowed in 'Q'
  Q <- rbind(c(0, 1),
             c(0, 0))
  
  # Transfer into tmat format
  tmat_twostate <- mstate::transMat(x = list(c(2), 
                                             c()),
                                    names = c("Alive", "Death"))
  # Number of possible transitions #
  no_of_trans <- sum(!is.na(tmat_twostate))
  
  # msm2urv reshapes the data 
  data <- msm::msm2Surv(data=data_input, Q = Q, 
                        subject="id", time="age_center", state="state_bin")
  
  # transition and men need to be factor
  data$trans  <- as.factor(data$trans)
  data$men    <- as.factor(data$men)
  
  # estimate model # 
  model <- vector(no_of_trans, mode="list")
  model[[1]] <-flexsurv::flexsurvreg(Surv(Tstart, Tstop, status) ~ men,
                      subset=(trans==1), data = data,
                      dist = definitions$distribution_2state)
  # gompertz usually fits best when the studied age range is 30-90
  # weibull usually fits best with respect to the entire age range
  # gompertz is proportional hazards exp(est) == Hazard Ration
  # weibull is accelerated failure time model exp(est) == Acceleration Factor
  
  # new data 
  new_data <- data.table(
    men = as.factor(c(0,1)),
    trans = as.factor(rep(1,times=2)))
  
  # Predictions #
  females_parsurv <- flexsurv::totlos.fs(model,
       trans = tmat_twostate, t = definitions$age_max - definitions$age_center,
       newdata = new_data[1], ci = FALSE, sing.inf = 1e+100, B=100)	
  males_parsurv <- flexsurv::totlos.fs(model,
       trans = tmat_twostate, t = definitions$age_max - definitions$age_center,
       newdata = new_data[2], ci = FALSE, sing.inf = 1e+100, B=100)	
  
  return(list(females_parsurv,males_parsurv))
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

