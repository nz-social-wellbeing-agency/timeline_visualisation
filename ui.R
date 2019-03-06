#################################################################### 
# Description: R Shiny user interface 
# 
# Input: 
# 
#
# 
# Output:
# 
# Authors: Simon Anastasiadis, Akilesh Chokkanathapuram 
# 
#
# 
# Dependencies:corresponding server and global files 
# Notes: 
# 
# Issues: 
# 
#
# 
# History (reverse order):
# 2019 Feb 25 SA v0 
# 2019 Mar 06 AK v0.1
# 
#################################################################### 

## Define UI

ui <- fluidPage(
        mainPanel(
          tabsetPanel(
          id = "tabs",

          tabPanel("Visualizations",
                   
                   br(),
                   br(),
                   
                   actionButton("resetButton", "Reset"),
                   actionButton("roleButton", "Role"),
                   actionButton("journeyButton", "Journey"),
                   actionButton("prepostButton", "Pre/Post"),
                   actionButton("generalButton", "General"),
                   actionButton("updateButton", "Update"),
                   
                   hr(),
                   
                   
                   conditionalPanel(
                     "output.view_reset == 'show'",
                     selectInput("group_selectInput", "Select group to
                                 investigate", choices = GROUP_LIST),
                     hr()
                     ),
                   
                   conditionalPanel(
                     "output.view_role == 'show'",
                     # pickerInput("role_checkbox", "Choose one or more:", choices = ROLE_LIST, multiple = TRUE),
                   # lapply(ROLE_LIST, function(x) checkboxInput(x, x)),
                     checkboxGroupInput(
                       "role_checkbox",
                       "Select roles to
                       display",
                       choiceNames = ROLE_LIST,
                       choiceValues = ROLE_LIST
                     ),
                     hr()
                   ),
                   
                   
                      conditionalPanel(
                      "output.view_journey == 'show'",
                      h2("Select measures to appear on the journey"),
                      # h3("Health Measures"),
                      # lapply(JOURNEY_HEALTH_MEASURE_LIST, function(x) checkboxInput(x, x)),
                        checkboxGroupInput(
                          "health_journey_checkbox",
                          "Health
                          measures",
                          choiceNames =
                            JOURNEY_HEALTH_MEASURE_LIST,
                          choiceValues
                          = JOURNEY_HEALTH_MEASURE_LIST
                        ),
                      
                      # h3("Employment Measures"),
                      
                      # lapply(JOURNEY_EMPLOYMENT_MEASURE_LIST, function(x) checkboxInput(x, x)),  
                      
                      checkboxGroupInput(
                          "employment_journey_checkbox",
                          "Employment measures",
                          choiceNames =
                            JOURNEY_EMPLOYMENT_MEASURE_LIST,
                          choiceValues = JOURNEY_EMPLOYMENT_MEASURE_LIST
                        ),
                      
                      # h3("Main Benefit Measures"),
                      # lapply(JOURNEY_MAINBENEFIT_MEASURE_LIST, function(x) checkboxInput(x, x)),
                      
                      checkboxGroupInput(
                          "mainbenefit_journey_checkbox",
                          "Main benefit measures",
                          choiceNames =
                            JOURNEY_MAINBENEFIT_MEASURE_LIST,
                          choiceValues = JOURNEY_MAINBENEFIT_MEASURE_LIST
                        ),
                      
                      # h3("Supplemental Benefit Measures"),
                      # lapply(JOURNEY_SUPPLEMENTAL_BENEFIT_MEASURE_LIST, function(x) checkboxInput(x, x)),
                      
                      checkboxGroupInput(
                        "supportbenefit_journey_checkbox",
                        "Supporting benefit measures",
                        choiceNames
                        = JOURNEY_SUPPLEMENTAL_BENEFIT_MEASURE_LIST,
                        choiceValues =
                          JOURNEY_SUPPLEMENTAL_BENEFIT_MEASURE_LIST
                      ),
                      
                      # h3("Education Measures"),
                      # lapply(JOURNEY_EDUCATION_MEASURE_LIST, function(x) checkboxInput(x, x)),
                      
                      checkboxGroupInput(
                        "education_journey_checkbox",
                        "Education measures",
                        choiceNames =
                          JOURNEY_EDUCATION_MEASURE_LIST,
                        choiceValues = JOURNEY_EDUCATION_MEASURE_LIST
                      ),
                      
                      # h3("Justice Measures"),
                      # lapply(JOURNEY_JUSTICE_MEASURE_LIST, function(x) checkboxInput(x, x)),
                      
                      checkboxGroupInput(
                        "justice_journey_checkbox",
                        "Justice measures",
                        choiceNames =
                          JOURNEY_JUSTICE_MEASURE_LIST,
                        choiceValues
                        = JOURNEY_JUSTICE_MEASURE_LIST
                      ),
                      
                      # h3("Other Measures"),
                      # lapply(JOURNEY_OTHER_MEASURE_LIST, function(x) checkboxInput(x, x)),
                      
                      checkboxGroupInput(
                        "other_journey_checkbox",
                        "Other
                        measures",
                        choiceNames =
                          JOURNEY_OTHER_MEASURE_LIST,
                        choiceValues =
                          JOURNEY_OTHER_MEASURE_LIST
                      ),
                      hr()
                      ),
                   
                   conditionalPanel(
                     "output.view_prepost ==
                     'show'",
                     "viewing pre/post - controls not implemented
                     yet",
                     hr()
                   ),
                   
                   conditionalPanel(
                     "output.view_general ==
                     'show'",
                     "viewing general - controls not implemented
                     yet",
                     hr()
                   ),
                   
                   textOutput("title"),
                   
                   hr(),
                   
                   conditionalPanel(
                     condition = "output.view_baby ==
                     'show'",
                     "baby",
                     fluidRow(
                       column(3, "pre-output goes here"),
                       column(6, "journey output goes here",
                              plotlyOutput("journey_baby")),
                       column(3, "post-output goes here")
                     ),
                     hr()
                   ),
                   
                   conditionalPanel(condition = "output.view_father == 'show'", "father",
                                    fluidRow(
                                      column(3, "pre-output goes here"),
                                      column(6, "journey
                                      output goes here"),
                                      column(3, "post-output goes here")
                                    ), hr()),
                   
                   conditionalPanel(
                     condition = "output.view_full_sib == 'show'",
                     "full sibling",
                     fluidRow(
                       column(3, "pre-output goes here"),
                       column(6, "journey output goes here"),
                       column(3 , "post-output goes here")
                     ),
                     hr()
                   ),
                   
                   conditionalPanel(
                     condition = "output.view_half_sib == 'show'",
                     "half sibling",
                     fluidRow(
                       column(3, "pre-output goes here"),
                       column(6, "journey output goes here"),
                       column(3, "post-output goes here")
                     ),
                     hr()
                   )
          ),
          tabPanel("Bookmarking",
                   br(),
                   br(),
                   textInput(inputId = "StateName",
                             "Name of the Saved State",
                             "defaultState"),


                   actionButton('save_inputs', 'Save inputs'),
                   # actionButton('input_list', 'Get States')
                   # useShinyjs(),
                   # ## Toggle for showing the list
                   # actionButton("toggle.main.button", "Toggle Main"),
                   br(),
                   conditionalPanel(
                     condition = !is.null(list.files(pattern = "\\.RDS$")),

                   selectInput("loadFileSelection",
                               "Select Session",
                               choices = NULL),
                   actionButton('load_inputs', 'Load Selected State')

                   ))
          )
 
  
    ),
  textOutput("debug"))