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
# 2019 Mar 06 AK v0.1
# ================================================================================================ #

# to support development
# setwd('C:/NotBackedUp/shiny apps/timeline_visualisation')

## required packages ----
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(readxl)
library(plotly)
library(ggiraph)
library(purrr)

## parameters ----
JOURNEY_LINE_MARGIN <- 0.05

## data parameters ----
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
# histogram_results <- read_xlsx("www/input data.xlsx", "histogram results")
# total_results <- read_xlsx("www/input data.xlsx", "totals results")

## supporting functions ----

plot_timeline <- function(group_name, role, selected_measures){
  # stop if no measures
  if(length(selected_measures) == 0)
    return(list(figure = NULL, figure_height = NA))
  # trim to measures of interest
  df <- journey_results %>% 
    filter(group_name == !!enquo(group_name),
           role == !!enquo(role),
           description %in% !!enquo(selected_measures)) %>%
    mutate(percent_with = 100* round(num_contributing_indiv / group_size,3)) %>%
    gather(key = "period", value = "indicator", `-20`, `-19`, `-18`, `-17`, `-16`, `-15`, `-14`, `-13`, `-12`,
           `-11`, `-10`, `-9`, `-8`, `-7`, `-6`, `-5`, `-4`, `-3`, `-2`, `-1`, `1`, `2`, `3`, `4`, `5`, `6`, 
           `7`, `8`, `9`, `10`, `11`, `12`, `13`) %>%
    select(description, percent_with, period, indicator) %>%
    filter(indicator != 0) %>%
    mutate(period = as.numeric(period))
  # calculate height
  figure_height <- df %>% select(description) %>% distinct() %>% nrow()
  # stop if no measures
  if(figure_height == 0)
    return(list(figure = NULL, figure_height = NA))
  
  # add vertical height
  tmp <- data.frame(description = selected_measures, height = -(1:length(selected_measures)), stringsAsFactors = FALSE)
  df <- df %>%
    inner_join(tmp, by = 'description')
  
  # set rectangle limits
  df <- df %>%
    mutate(x_min = ifelse(sign(period) == -1, period, period - 1),
           x_max = ifelse(sign(period) == -1, period + 1, period),
           y_min = height + 0.5 - (0.5 - JOURNEY_LINE_MARGIN) * percent_with / 100,
           y_max = height + 0.5 + (0.5 - JOURNEY_LINE_MARGIN) * percent_with / 100,
           y_min_grey = height + 0.5 - (0.5 - JOURNEY_LINE_MARGIN),
           y_max_grey = height + 0.5 + (0.5 - JOURNEY_LINE_MARGIN))
  
  # plot
  suppressWarnings(
    p <- ggplot(data = df) +
      geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min_grey, ymax = y_max_grey), fill = 'grey', text = NULL) +
      geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = description,
                    text = paste("Measure: ",description,
                                 "<br>Time (fortnight): ", period,
                                 "<br> Percentage of group with this measure: ",percent_with
                    )))
  )
  
  p <- p  +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    geom_vline(xintercept = 0, colour = 'yellow', linetype = 'dashed') +
    xlab('Time from birth (fortnights)') +
    xlim(-20,13) #+
    # coord_fixed(ratio = 33 / 8)
  p <- ggplotly(p, tooltip = "text") %>%
    layout(yaxis = list(scaleanchor = "x", scaleratio = 33 / 8, domain = c(0, -figure_height)),
           xaxis = list(domain = c(-20,13)))
  # return
  return(list(figure = p, figure_height = figure_height))
}



