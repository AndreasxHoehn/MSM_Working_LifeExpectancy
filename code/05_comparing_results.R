### Meta ###

# Author: Andreas H?hn
# Version: 1.0
# Date: 2022-05-09
# About: this module compares the MSM results for total life expectancy with 
# total life expectancy estimates from 2 other approaches 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [1] CHIANG METHOD 1984 ###

result_chiang_females  <- .DataPrepChiang(data_input = cohort_long[men == 0,],
                                  definitions = definitions)
result_chiang_males    <- .DataPrepChiang(data_input = cohort_long[men == 1,],
                                  definitions = definitions)

result_chiang_females <- .LTBChiang(input_data = result_chiang_females)
result_chiang_males   <- .LTBChiang(input_data = result_chiang_males)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [2] TWO STATE FLEXSURV MODEL ###

# Results of Parametric Survival Model
result_parsurv <- .ParSurvLE(data_input = cohort_long,
                      definitions = definitions)
result_parsurv_females <- result_parsurv[[1]]
result_parsurv_males   <- result_parsurv[[2]]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
