
*Author: Andreas Höhn
*Version: 1.0
*Date:  2022-05-17
*About: The Readme file for MSM_Working_LifeExpectancy
Estimating Working Life Expectancy from a Parametric Multistate Model using "flexsurv"

# How do I run the code? 
Running the code is very simple, you first need to download the folder 
containing all files and extract them into any folder on your PC (i.e. Desktop). 
Be aware that the program has not been tested for network drives. Close all 
tabs and RStudio. Then open "three_state_cohort.rproj" file in the main folder. 
This will will open RStudio in project mode (check top left bar and ensure that 
"three_state_cohort" is displayed there. Within R Studio, open "code/01_main.R" 
file and run it. All analyses wil be run automatically and the corresponding 
output will be created in the output folder.

# Do I need to specify a working directory manually?
Nope, all file paths are defined relative, just ensure the file structure 
provided in the example is reproduced 

# Do I need to install packages manually? #
Nope, the program will check automatically for all required libraries and load 
them If requiered (and only if required), it will download and install missing 
libraries. Therefore, running code for the very first time might take a bit longer. 
All subsequent runs wont require this and all will happen much faster. However, 
it is a good idea to install/update the following packages manually: "rmarkdown", 
"rcpp".

# How do I report bugs? #
send an email to: hoehn@riseup.net 
