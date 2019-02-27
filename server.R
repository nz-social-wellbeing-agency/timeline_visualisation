# ================================================================================================ #
# Description: R Shiny calculation server
#
# Input:
#
# Output:
#
# Author: Simon Anastasiadis
#
# Dependencies: corresponding ui and global files
#
# Notes:
#
# Issues:
#
# History (reverse order): 
# 2019 Feb 25 SA v0
# ================================================================================================ #

# Define server logic ----
server <- function(input, output, session) {
  ## control panels conditional display ----
  #### setup ----
  panel_control <- reactiveValues()
  # control panels
  panel_control$view_reset <- FALSE
  panel_control$view_role <- FALSE
  panel_control$view_journey <- FALSE
  panel_control$view_prepost <- FALSE
  panel_control$view_general <- FALSE
  # role panels
  panel_control$view_baby <- FALSE
  panel_control$view_mother <- FALSE
  panel_control$view_father <- FALSE
  panel_control$view_full_sib <- FALSE
  panel_control$view_half_sib <- FALSE
  
  #### reactives ----
  output$view_reset <- renderText(ifelse(panel_control$view_reset,"show","noshow"))
  output$view_role <- renderText(ifelse(panel_control$view_role,"show","noshow"))
  output$view_journey <- renderText(ifelse(panel_control$view_journey,"show","noshow"))
  output$view_prepost <- renderText(ifelse(panel_control$view_prepost,"show","noshow"))
  output$view_general <- renderText(ifelse(panel_control$view_general,"show","noshow"))
  output$view_baby <- renderText(ifelse(panel_control$view_baby,"show","noshow"))
  output$view_mother <- renderText(ifelse(panel_control$view_mother,"show","noshow"))
  output$view_father <- renderText(ifelse(panel_control$view_father,"show","noshow"))
  output$view_full_sib <- renderText(ifelse(panel_control$view_full_sib,"show","noshow"))
  output$view_half_sib <- renderText(ifelse(panel_control$view_half_sib,"show","noshow"))
  outputOptions(output, "view_reset", suspendWhenHidden = FALSE) # needed for conditional panel
  outputOptions(output, "view_role", suspendWhenHidden = FALSE) # needed for conditional panel
  outputOptions(output, "view_journey", suspendWhenHidden = FALSE) # needed for conditional panel
  outputOptions(output, "view_prepost", suspendWhenHidden = FALSE) # needed for conditional panel
  outputOptions(output, "view_general", suspendWhenHidden = FALSE) # needed for conditional panel
  outputOptions(output, "view_baby", suspendWhenHidden = FALSE) # needed for conditional panel
  outputOptions(output, "view_mother", suspendWhenHidden = FALSE) # needed for conditional panel
  outputOptions(output, "view_father", suspendWhenHidden = FALSE) # needed for conditional panel
  outputOptions(output, "view_full_sib", suspendWhenHidden = FALSE) # needed for conditional panel
  outputOptions(output, "view_half_sib", suspendWhenHidden = FALSE) # needed for conditional panel
  
  #### observers ----
  observeEvent(input$resetButton,{
    # set logicals
    panel_control$view_reset <- !panel_control$view_reset
    panel_control$view_role <- FALSE
    panel_control$view_journey <- FALSE
    panel_control$view_prepost <- FALSE
    panel_control$view_general <- FALSE
  })
  
  observeEvent(input$roleButton,{
    # set logicals
    panel_control$view_reset <- FALSE
    panel_control$view_role <- !panel_control$view_role
    panel_control$view_journey <- FALSE
    panel_control$view_prepost <- FALSE
    panel_control$view_general <- FALSE
  })
  
  observeEvent(input$journeyButton,{
    # set logicals
    panel_control$view_reset <- FALSE
    panel_control$view_role <- FALSE
    panel_control$view_journey <- !panel_control$view_journey
    panel_control$view_prepost <- FALSE
    panel_control$view_general <- FALSE
    
  })
  
  observeEvent(input$prepostButton,{
    # set logicals
    panel_control$view_reset <- FALSE
    panel_control$view_role <- FALSE
    panel_control$view_journey <- FALSE
    panel_control$view_prepost <- !panel_control$view_prepost
    panel_control$view_general <- FALSE
  })
  
  observeEvent(input$generalButton,{
    # set logicals
    panel_control$view_reset <- FALSE
    panel_control$view_role <- FALSE
    panel_control$view_journey <- FALSE
    panel_control$view_prepost <- FALSE
    panel_control$view_general <- !panel_control$view_general
  })
  
  observeEvent(input$updateButton,{
    # set logicals
    panel_control$view_reset <- FALSE
    panel_control$view_role <- FALSE
    panel_control$view_journey <- FALSE
    panel_control$view_prepost <- FALSE
    panel_control$view_general <- FALSE
    update_visualisation()
  })
  
  ## visualisation staging ----
  #### setup ----
  visualisation_parts <- reactiveValues()
  visualisation_parts$title <- "No group presently selected"
  visualisation_parts$baby_journey <- NULL
  visualisation_parts$mother_journey <- NULL
  visualisation_parts$father_journey <- NULL
  visualisation_parts$full_sib_journey <- NULL
  visualisation_parts$half_sib_journey <- NULL
  
  #### update title ----
  update_title <- function(){
    # get group name
    group_name <- input$group_selectInput
    # get group size
    size <- journey_results %>%
      filter(group_name == !!enquo(group_name)) %>%
      select(group_size) %>%
      summarise(group_size = min(group_size))
    size <- size[["group_size"]]
    # update
    visualisation_parts$title <- sprintf(paste0("Group: ",group_name,"\tSize: ",size," journeys"))
  }
  
  #### update panels ----
  update_panels <- function(){
    panel_control$view_baby <- "baby" %in% input$role_checkbox
    panel_control$view_mother <- "mother" %in% input$role_checkbox
    panel_control$view_father <- "father" %in% input$role_checkbox
    panel_control$view_full_sib <- "full sibling" %in% input$role_checkbox
    panel_control$view_half_sib <- "half sibling" %in% input$role_checkbox
  }
  
  #### update journey ----
  update_journey <- function(){
    
    selected_measures <- c(input$health_journey_checkbox,
                           input$employment_journey_checkbox, 
                           input$mainbenefit_journey_checkbox,
                           input$supportbenefit_journey_checkbox,
                           input$education_journey_checkbox,
                           input$justice_journey_checkbox,
                           input$other_journey_checkbox)
    
    if('baby' %in% input$role_checkbox)
      visualisation_parts$baby_journey <- plot_timeline(input$group_selectInput , 'baby', selected_measures)
    if('mother' %in% input$role_checkbox)
      visualisation_parts$mother_journey <- plot_timeline(input$group_selectInput , 'mother', selected_measures)

    
  }
  
  ## master update visualisation ----
  update_visualisation <- function(){
    update_title()
    update_panels()
    update_journey()
    # update_prepost()
    # update_general()
  }
  
  ## output ----
  output$title <- renderText(visualisation_parts$title)
  
  output$journey_baby <- renderPlotly( visualisation_parts$baby_journey$figure )
  output$journey_mother <- renderPlotly( visualisation_parts$mother_journey$figure )
  output$journey_father <- renderPlot( visualisation_parts$journey_father )
  output$journey_full_sib <- renderPlot( visualisation_parts$journey_full_sib )
  output$journey_half_sib <- renderPlot( visualisation_parts$journey_half_sib )
  
  
  ## other ----
  
  # debug print
  output$debug <- renderText(as.character(panel_control$view_journey))
  
  # session$onSessionEnded(function() { # version for stand-alone
  onSessionEnded(function() { # version for RStudio
    # sink_reset() # end sink
    # stopApp() # for stand-alone
  })
}
