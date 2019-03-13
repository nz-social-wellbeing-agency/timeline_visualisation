#################################################################### 
#' Description: R Shiny user interface 
#' 
#' Input: 
#' 
#' Output:
#' 
#' Authors: Simon Anastasiadis, Akilesh Chokkanathapuram 
#' 
#' Dependencies:corresponding server and global files 
#' 
#' Notes: 
#' 
#' Issues: 
#' 
#' History (reverse order):
#' 2019 Feb 25 SA v0 
#' 2019 Mar 06 AK v0.1
#################################################################### 

## Define UI

ui <- fluidPage(
  tabsetPanel(
    id = "tabs",
    
    ## tab: visualisation ----
    tabPanel("Visualizations",
             
             br(),
             br(),
             
             #### buttons ----
             actionButton("resetButton", "Reset"),
             actionButton("roleButton", "Role"),
             actionButton("journeyButton", "Journey"),
             actionButton("prepostButton", "Pre/Post"),
             actionButton("generalButton", "General"),
             actionButton("updateButton", "Update"),
             
             hr(),
             
             #### controls ----
             conditionalPanel(condition = "output.view_reset == 'show'",
                              selectInput("group_selectInput", "Select group to investigate", choices = group_list),
                              hr()
             ),
             
             conditionalPanel(condition = "output.view_role == 'show'",
                              checkboxGroupInput("role_checkbox", "Select roles to display",
                                                 choiceNames = role_list, choiceValues = role_list
                              ),
                              hr()
             ),
             
             
             conditionalPanel(condition = "output.view_journey == 'show'",
                              h2("Select measures to appear on the journey"),
                              
                              lapply(names(journey_description_list),
                                     FUN = function(x){
                                       tmp_inputID <- paste0("journey_",gsub(" ","_",x),"_checkbox")
                                       return(checkboxGroupInput(tmp_inputID, label = x,
                                                                 choiceNames = journey_description_list[[x]],
                                                                 choiceValues = journey_description_list[[x]]))
                                     }),
                              
                              hr()
             ),
             
             conditionalPanel(condition = "output.view_prepost == 'show'",
                              h2("Select measures to appear pre & post the journey"),
                              
                              lapply(names(pre_post_description_list),
                                     FUN = function(x){
                                       tmp_inputID <- paste0("pre_post_",gsub(" ","_",x),"_checkbox")
                                       return(checkboxGroupInput(tmp_inputID, label = x,
                                                                 choiceNames = pre_post_description_list[[x]],
                                                                 choiceValues = pre_post_description_list[[x]]))
                                     }),
                              
                              hr()
             ),
             
             conditionalPanel(condition = "output.view_general == 'show'",
                              h2("Select the general measures to appear below the journey"),
                              
                              lapply(names(general_description_list),
                                     FUN = function(x){
                                       tmp_inputID <- paste0("general_",gsub(" ","_",x),"_checkbox")
                                       return(checkboxGroupInput(tmp_inputID, label = x,
                                                                 choiceNames = general_description_list[[x]],
                                                                 choiceValues = general_description_list[[x]]))
                                     }),
                              # "viewing general - controls not implemented yet",
                              hr()
             ),
             
             #### results ----
             textOutput("title"),
             
             hr(),
             
             conditionalPanel(condition = "output.view_baby == 'show'", "baby",
                              fluidRow(
                                column(3, "pre-output goes here"),
                                column(6, "journey output goes here",
                                       uiOutput("journey_baby_ui")),
                                column(3, "post-output goes here")
                              ),
                              hr()
             ),
             
             conditionalPanel(condition = "output.view_mother == 'show'", "mother",
                              fluidRow(
                                column(3, "pre-output goes here",
                                       uiOutput("pre_mother_ui")),
                                column(6, "journey output goes here",
                                       plotOutput("journey_mother")),
                                column(3, "post-output goes here")
                              ),
                              hr()
             ),
             
             conditionalPanel(condition = "output.view_father == 'show'", "father",
                              fluidRow(
                                column(3, "pre-output goes here"),
                                column(6, "journey output goes here"),
                                column(3, "post-output goes here")
                              ), hr()),
             
             conditionalPanel(condition = "output.view_full_sib == 'show'", "full sibling",
                              fluidRow(
                                column(3, "pre-output goes here"),
                                column(6, "journey output goes here"),
                                column(3 , "post-output goes here")
                              ),
                              hr()
             ),
             
             conditionalPanel(condition = "output.view_half_sib == 'show'", "half sibling",
                              fluidRow(
                                column(3, "pre-output goes here"),
                                column(6, "journey output goes here"),
                                column(3, "post-output goes here")
                              ),
                              hr()
             )
    ),
    ## tab: bookmarking ----
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
    ## end of tab wrapper ----
  )
  
)