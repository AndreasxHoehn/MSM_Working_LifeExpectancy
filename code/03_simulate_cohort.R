### Meta ###

# Author: Andreas H?hn
# Version: 1.0
# Date:  2022-05-09
# About: This file creates a baseline-level cohort, subsets with this cohort, 
#       rolls it out in long-format and simulates labor market activity 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [1] Create a Baseline-Level Cohort ###

# Initialize a Baseline-Level Cohort #
cohort_baseline <- .CreateCohort(definitions)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [2] Entry and Exit into the Study / Inclusion and Exclusion Criteria ###

# study_entry: latest of either study start or birthday for the age we center 
cohort_baseline[, study_entry := max(definitions$study_start, bday_center),
                by = "id"]

# study exit: earliest of study end or death
cohort_baseline[, study_exit := min(definitions$study_end, dday), by = "id"]

# we only include individuals which were alive at study Entry 
cohort_baseline <- copy(cohort_baseline[study_entry < study_exit])

# minimum observability
cohort_baseline <- copy(cohort_baseline[as.numeric(study_exit - study_entry) > 
                                          definitions$min_obs, ])

# for those who died after study end, we are not supposed to know the date  
cohort_baseline[dday > study_exit, dday := base::as.Date(NA) ]

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [3] Long-Format Episode Split ###

cohort_long <- .EpisodeSplitCohort(data_input = cohort_baseline,
                                   definitions = definitions)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [4] Simulate Time-Updated Employment ###

cohort_long <- .SimEmployment(definitions = definitions,
                              input_data = cohort_long)
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [5] Produce Overview of Study Cohort ###

png(filename = "plots/overview_cohort.png",
    width = definitions$fig_width, height = definitions$fig_height)
par(mfrow = c(2,2))
hist(cohort_baseline$life_length, breaks = 50)
hist(cohort_baseline$bday, breaks = "months")
hist(cohort_baseline$study_entry, breaks = "months")
hist(cohort_baseline$study_exit, breaks = "months")
dev.off()

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [6] Save as CSV ###
write.csv(cohort_long, "data/cohort_long.csv", row.names = TRUE)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
