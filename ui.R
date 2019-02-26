# ================================================================================================ #
# Description: R Shiny user interface
#
# Input:
#
# Output:
#
# Author: Simon Anastasiadis
#
# Dependencies: corresponding server and global files
#
# Notes:
#
# Issues:
#
# History (reverse order): 
# 2019 Feb 25 SA v0
# ================================================================================================ #

# Define UI ----
ui <- fluidPage(
  
  actionButton("resetButton","Reset"),
  actionButton("roleButton","Role"),
  actionButton("journeyButton","Journey"),
  actionButton("prepostButton","Pre/Post"),
  actionButton("generalButton","General"),
  actionButton("updateButton","Update"),
  
  hr(),
  
  conditionalPanel("output.view_reset == 'show'",
                   selectInput("group_selectInput", "Select group to investigate", choices = GROUP_LIST)),
  
  conditionalPanel("output.view_role == 'show'",
                   checkboxGroupInput("role_checkbox", "Select roles to display",
                                      choiceNames = ROLE_LIST,
                                      choiceValues = ROLE_LIST)
                   ),
  
  conditionalPanel("output.view_journey == 'show'",
                   h3("Select measures to appear on the journey"),
                   checkboxGroupInput("health_journey_checkbox", "Health measures",
                                      choiceNames = JOURNEY_HEALTH_MEASURE_LIST,
                                      choiceValues = JOURNEY_HEALTH_MEASURE_LIST),
                   checkboxGroupInput("employment_journey_checkbox", "Employment measures",
                                      choiceNames = JOURNEY_EMPLOYMENT_MEASURE_LIST,
                                      choiceValues = JOURNEY_EMPLOYMENT_MEASURE_LIST),
                   checkboxGroupInput("mainbenefit_journey_checkbox", "Main benefit measures",
                                      choiceNames = JOURNEY_MAINBENEFIT_MEASURE_LIST,
                                      choiceValues = JOURNEY_MAINBENEFIT_MEASURE_LIST),
                   checkboxGroupInput("supportbenefit_journey_checkbox", "Supporting benefit measures",
                                      choiceNames = JOURNEY_SUPPLEMENTAL_BENEFIT_MEASURE_LIST,
                                      choiceValues = JOURNEY_SUPPLEMENTAL_BENEFIT_MEASURE_LIST),
                   checkboxGroupInput("education_journey_checkbox", "Education measures",
                                      choiceNames = JOURNEY_EDUCATION_MEASURE_LIST,
                                      choiceValues = JOURNEY_EDUCATION_MEASURE_LIST),
                   checkboxGroupInput("justice_journey_checkbox", "Justice measures",
                                      choiceNames = JOURNEY_JUSTICE_MEASURE_LIST,
                                      choiceValues = JOURNEY_JUSTICE_MEASURE_LIST),
                   checkboxGroupInput("other_journey_checkbox", "Other measures",
                                      choiceNames = JOURNEY_OTHER_MEASURE_LIST,
                                      choiceValues = JOURNEY_OTHER_MEASURE_LIST)
                   ),
  conditionalPanel("output.view_prepost == 'show'","viewing pre/post - controls not implemented yet"),
  conditionalPanel("output.view_general == 'show'","viewing general - controls not implemented yet"),
  
  hr(),
  
  textOutput("title"),
  
  hr(),
  
  
  conditionalPanel(condition = "output.view_baby == 'show'","baby",
                   fluidRow(
                     column(3,"pre-output goes here"),
                     column(6,"journey output goes here"),
                     column(3,"post-output goes here")
                   ),
                   
                   hr()
  ),
  conditionalPanel(condition = "output.view_mother == 'show'","mother",
                   fluidRow(
                     column(3,"pre-output goes here"),
                     column(6,"journey output goes here"),
                     column(3,"post-output goes here")
                   ),
                   
                   hr()
  ),
  conditionalPanel(condition = "output.view_father == 'show'","father",
                   fluidRow(
                     column(3,"pre-output goes here"),
                     column(6,"journey output goes here"),
                     column(3,"post-output goes here")
                   ),
                   
                   hr()
  ),
  conditionalPanel(condition = "output.view_full_sib == 'show'","full sibling",
                   fluidRow(
                     column(3,"pre-output goes here"),
                     column(6,"journey output goes here"),
                     column(3,"post-output goes here")
                   ),
                   
                   hr()
  ),
  conditionalPanel(condition = "output.view_half_sib == 'show'","half sibling",
                   fluidRow(
                     column(3,"pre-output goes here"),
                     column(6,"journey output goes here"),
                     column(3,"post-output goes here")
                   ),
                   
                   hr()
  ),
  
  
  
  textOutput("debug")
  
  )









