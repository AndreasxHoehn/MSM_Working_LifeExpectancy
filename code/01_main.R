### Meta ###

# Author: Andreas Höhn
# Version: 1.0
# Date:  2022-05-12
# About: The Main Control File

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Setting ###

# Longitudinal Cohort Study 
# Multistate Survival Model -> Centered for Age 30
# State Space: [1] Active / [2] Inactive / [3] Death (absorbing state)
# Four Allowed Transitions:
# No. 1: [1] -> [2] / 
# No. 2: [1] -> [3] / 
# No. 3: [2] -> [1] / 
# No. 4: [2] -> [3] /

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Preparation ###

# clean workspace
rm(list = ls())  # remove all objects from work space 
gc(full = TRUE)  # deep clean garbage

# set seed 
set.seed(20220512) 

# display format
options(scipen = 1000)

# benchmark time 
benchmark_time <- list() 
benchmark_time$start <- Sys.time()

# libraries: to be installed/loaded automatically; called in user_functions
RequiredPackages <- c("here",                        # Folder Structure
                      "data.table","flexsurv",       # Analysis
                      "ggplot2","RColorBrewer",      # Data Visualization
                      "rmarkdown","tinytex","knitr") # Report 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Definitions ###

# initialize ...
definitions <- list()   
# GENERAL 
definitions$year_length    <- 365.25       # duration of one year
# REGISTER
definitions$register_size  <- 30000        # size of the register 
definitions$LE             <- c(80,75)     # LE for women [1] / men [2]
definitions$LE_sd          <- c(10,10)     # LE std dev for women [1 / men [2]
definitions$age_retirement <- 75           # age of mandatory retirement
definitions$prob_inactive  <- c(25,29)     # inactivity term for simulation / women [1] / men [2] 
                                           # probability = (age-term)/100 per year
definitions$prob_stay      <- c(0.15,0.10) # probability of staying inactive for women [1] / men [2]
definitions$age_max        <- 120          # maximum life span to solve l(x) integral
definitions$cohorts_range  <- c(base::as.Date("1900-01-01"),  # range of cohorts 
                                base::as.Date("2019-12-31"))  # lower / upper
# COHORT STUDY DESIGN 
definitions$study_start    <- base::as.Date("2010-01-01") # start of cohort study 
definitions$study_end      <- base::as.Date("2019-12-31") # end of cohort study   
definitions$min_obs        <- 365.25   # minimum number of days under observation
definitions$episode_length <- 365.25   # duration of one episode in days 
definitions$age_center     <- 30       # centered for age 30
definitions$age_dist_limit <- 34       # upper age for start state distribution
# MODELLING 
definitions$formula_msm      <- base::noquote("Surv(Tstart, Tstop, status) ~ men") # functional form
definitions$distributions_msm   <- c("weibull", "weibull",  # best fitting distributions for trans 1, 2, 3, 4; 
                                  "gompertz", "weibull")    # ensure this matches 04_ / L122     
definitions$distribution_2state <- "gompertz"               # dist for parametric LE estimate in sensitivity checks
definitions$bootstraps       <- 3
# PLOTTING / VISUALIZATION
definitions$fig_width        <- 1024
definitions$fig_height       <- 768
definitions$gg_fig_width     <- 25
definitions$gg_fig_height    <- 15
definitions$rounding         <- 3
definitions$rounding_results <- 1

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Source Files ###

source("code/02_user_functions.R")
# source("code/03_simulate_cohort.R")
source("code/04_multistate_model.R")
source("code/05_comparing_results.R")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Create Report ###

rmarkdown::render(input = "code/06_report.Rmd",
                  output_file = here::here("output","report.html"),
                  output_format = "html_document")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### Benchmark Time ###

benchmark_time$end <- Sys.time()
print("Duration of Program:")
print(round(benchmark_time$end - benchmark_time$start), 
      definitions$rounding_results)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

