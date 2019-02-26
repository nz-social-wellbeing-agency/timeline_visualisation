# ================================================================================================ #
# Description: R Shiny global setup scape
#
# Input:
#
# Output:
#
# Author: Simon Anastasiadis
#
# Dependencies: corresponding ui and server files
#
# Notes:
#
# Issues:
#
# History (reverse order): 
# 2019 Feb 25 SA v0
# ================================================================================================ #

# to support development
setwd('C:/NotBackedUp/shiny apps/timeline_visualisation')

## required packages ----
library(shiny)
library(tidyverse)
library(readxl)

## parameters ----
ROLE_LIST = c("baby", "mother", "father", "full sibling", "half sibling")
GROUP_LIST = list("machine defined clusters" = c("cluster 1", 
                                                 "cluster 2", 
                                                 "cluster 3", 
                                                 "cluster 4", 
                                                 "cluster 5", 
                                                 "cluster 6", 
                                                 "cluster 7", 
                                                 "cluster 8", 
                                                 "cluster 9", 
                                                 "cluster 10", 
                                                 "cluster 11", 
                                                 "cluster 12"),
                  "ethnic group" = c("asian ethnicity", 
                                     "european ethnicity", 
                                     "maori ethnicity", 
                                     "other ethnicity", 
                                     "pacific ethnicity"),
                  "health" = c("any part of B4SC declined", 
                               "any part of B4SC identified need", 
                               "parents experience a chronic condition"),
                  "life stage" = c("a parent is a recent migrant", 
                                   "mother is older at time of birth", 
                                   "mother is teen age at time of birth"),
                  "mental health, addictions and justice" = c("attends alcohol/drug program", 
                                                              "concern that mother smoked during pregnancy", 
                                                              "either parent has had a corrections sentence", 
                                                              "mother attends maternal mental health program"),
                  "pregnancy" = c("birth weight of baby is low", 
                                  "first pregnancy for the mother", 
                                  "hard birth for mother", 
                                  "hard for mother to carry to term", 
                                  "hard pregnancy for mother"),
                  "qualification" = c("certificate qualificiation", 
                                      "graduate qualificiation", 
                                      "no qualificiation", 
                                      "postgrad qualificiation")
)
JOURNEY_EDUCATION_MEASURE_LIST = c("enrolled tertiary education", 
                                   "enrolled industry training", 
                                   "enrolled targeted training", 
                                   "awarded qualification")
JOURNEY_EMPLOYMENT_MEASURE_LIST = c("Full employment with wages & salaries", 
                                    "Partial employment with wages & salaries", 
                                    "Paid parental leave")
JOURNEY_HEALTH_MEASURE_LIST = c("enrolled PHO contact", 
                                "non-enrolled PHO contact", 
                                "enrolled with a PHO", 
                                "emergency department visit", 
                                "hospital out-patient visit", 
                                "admitted to hospital", 
                                "community visit by hospital staff", 
                                "lab test", 
                                "antidepressant dispensing", 
                                "contraceptives dispensing", 
                                "program with maternal MH team", 
                                "program with alcohol and drug team")
JOURNEY_JUSTICE_MEASURE_LIST = c("police concern for family violence", 
                                 "court hearing", 
                                 "community sentence", 
                                 "detained sentence", 
                                 "home detention sentence", 
                                 "under conditions", 
                                 "under supervision")
JOURNEY_MAINBENEFIT_MEASURE_LIST = c("Job Seeker benefit", 
                                     "Sole Parent benefit", 
                                     "Supported Living benefit", 
                                     "Youth benefit", 
                                     "Other benefit", 
                                     "gap in main benefit receipt")
JOURNEY_OTHER_MEASURE_LIST = c("address change", 
                               "ACC claim", 
                               "married or civil union", 
                               "pregnancy")
JOURNEY_SUPPLEMENTAL_BENEFIT_MEASURE_LIST = c("Accommodation Supplement", 
                                              "Employment support", 
                                              "Care subsidy", 
                                              "Disability Allowance", 
                                              "Other support")



## load data ----
journey_results <- read_xlsx("www/input data.xlsx", "journeys results")

# groups_list <- get_options(journey_results, "group_name", "group_type")
# roles <- get_options(journey_results, "role")
# measures_list <- get_options(journey_results, "description", "description_type")


# histogram_results <- read_xlsx("www/input data.xlsx", "histogram results")
# total_results <- read_xlsx("www/input data.xlsx", "totals results")




