# ================================================================================================ #
# Description: R Shiny calculation server
#
# Input:
#
# Output:
#
# Author: Simon Anastasiadis, Akilesh Chokkanathapuram
#
# Dependencies: corresponding ui and global files
#
# Notes:
#
# Issues:
#
# History (reverse order): 
# 2019 Feb 25 SA v0
# 2019 Mar 06 AK v0.1
# ================================================================================================ #

# Define server logic ----
server <- function(input, output, session) {
  
  ## load/save observers ----
  #### initial load of saved files ----
  if (identical(list.files(pattern = "\\.RDS$"), character(0))){
    savedSessionFiles <- ""  
  }else{
    savedSessionFiles <- list.files(pattern = "\\.RDS$")
  }
  updateSelectInput(session = session, inputId = "loadFileSelection", choices = savedSessionFiles)
  
  #### load state ----
  observeEvent(input$load_inputs,{
    
    file <- input$loadFileSelection #select the file from the dropdown list box 
    savedInputs <- readRDS(file) #read the input RDS fileinto the system for processing
    inputIDs      <- names(savedInputs) # input ID names
    inputvalues   <- unlist(savedInputs) # input ID values
    
    #running a loop to match the content to the relevant ID's
    for (i in 1:length(inputIDs)) {
      
      #All components except checkboxes are matched here
      session$sendInputMessage(inputIDs[i],  list(value=inputvalues[[i]]))
      
      #checkbox matching begins here
      #Create a manual list of checkbox names
      check_box_names <- lapply(names(journey_description_list),
                                FUN = function(x){ paste0(gsub(" ","_",x),"_checkbox") })
      
      #dummy dataframe to store values
      check.box.data.frame.loader <- data.frame(matrix(NA, nrow = 1, ncol = length(check_box_names)))
      colnames(check.box.data.frame.loader) <- check_box_names
      
      #identify content from the saved data and segregate it for easier loading
      #ACHOKKANATHAPURAM
      for (i in 1:length(check_box_names)){
        pattern.check <- check_box_names[i]
        check.box.name.choices.checked <- grepl(pattern.check, substr(names(inputvalues), 0, (nchar(names(inputvalues))-1)))
        items.to.load <- which(check.box.name.choices.checked == TRUE)
        loader.buffer <- list(inputvalues[items.to.load])
        loader.buffer <- list(unlist(loader.buffer, use.names = FALSE))
        # print(loader.buffer)
        check.box.data.frame.loader[[i]] <- loader.buffer
      }
    }
    
    #run the segregated content through a loop to load them into the shiny UI
    for(i in 1:ncol(check.box.data.frame.loader)) {
      check.box.to.update <- colnames(check.box.data.frame.loader)[i]
      updateCheckboxGroupInput(
        session = session,
        inputId = check.box.to.update,
        choices = NULL,
        label = NULL,
        selected = unlist(check.box.data.frame.loader[, i], use.names = F)
      )
    }
  })
  
  #### save state ----
  observeEvent(input$save_inputs,{
    saveRDS(reactiveValuesToList(input,all.names = T) , file = paste0(input$StateName, '.RDS'))
    savedSessionFiles <- list.files(pattern = "\\.RDS$")
    updateSelectInput(session = session, inputId = "loadFileSelection", choices = savedSessionFiles)
  })
  
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
    size <- group_controls %>%
      filter(group_display_name == !!enquo(group_name)) %>%
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
    
    selected_measures <- lapply(names(journey_description_list),
           FUN = function(x){
             input_checkboxgroup <- paste0("journey_",gsub(" ","_",x),"_checkbox")
             return(input[[input_checkboxgroup]])
           })
    selected_measures <- unlist(selected_measures, use.names = FALSE)
    
    if('baby' %in% input$role_checkbox)
      visualisation_parts$baby_journey <- plot_timeline(input$group_selectInput , 'baby', selected_measures)
    if('mother' %in% input$role_checkbox)
      visualisation_parts$mother_journey <- plot_timeline(input$group_selectInput , 'mother', selected_measures)
    
    #
    #
    # NEED TO ADD ALL ROLES HERE
    #
    #
  }
  
  #### update pre & post figures ----
  update_pre_post <- function(){
    
    selected_measures <- lapply(names(pre_post_description_list),
                                FUN = function(x){
                                  input_checkboxgroup <- paste0("pre_post_",gsub(" ","_",x),"_checkbox")
                                  return(input[[input_checkboxgroup]])
                                })
    selected_measures <- unlist(selected_measures, use.names = FALSE)
    
    if('baby' %in% input$role_checkbox)
      visualisation_parts$baby_pre_post <- plot_pre_post(input$group_selectInput , 'baby', selected_measures)
    if('mother' %in% input$role_checkbox)
      visualisation_parts$mother_pre_post <- plot_pre_post(input$group_selectInput , 'mother', selected_measures)
    
    #
    #
    # NEED TO ADD ALL ROLES HERE
    #
    #
  }
  
  #### update general figures ----
  update_general <- function(){
    
    selected_measures <- lapply(names(pre_post_description_list),
                                FUN = function(x){
                                  input_checkboxgroup <- paste0("pre_post_",gsub(" ","_",x),"_checkbox")
                                  return(input[[input_checkboxgroup]])
                                })
    selected_measures <- unlist(selected_measures, use.names = FALSE)
    
    if('baby' %in% input$role_checkbox)
      visualisation_parts$baby_pre_post <- plot_pre_post(input$group_selectInput , 'baby', selected_measures)
    if('mother' %in% input$role_checkbox)
      visualisation_parts$mother_pre_post <- plot_pre_post(input$group_selectInput , 'mother', selected_measures)
    
    #
    #
    # NEED TO ADD ALL ROLES HERE
    #
    #
  }
  
  
  ## master update visualisation ----
  update_visualisation <- function(){
    update_title()
    update_panels()
    update_journey()
    update_pre_post()
    # update_general()
  }
  
  ## output ----
  #### output other ----
  output$title <- renderText(visualisation_parts$title)
  
  #### output journey ----
  # renderPlot needed for outputting plot, renderUI needed to make height dynamic
  output$journey_baby <- renderPlot( visualisation_parts$baby_journey$figure )
  output$journey_baby_ui <- renderUI({ 
    req(visualisation_parts$baby_journey)
    req(visualisation_parts$baby_journey$figure_height)
    plotOutput("journey_baby",height = HEIGHT_PIXELS * (1 + visualisation_parts$baby_journey$figure_height))
  })
  
  #
  #
  # THESE FIGURES NEED UPDATING TO MATCH BABY
  #
  #
  output$journey_mother <- renderPlot( visualisation_parts$mother_journey )
  output$journey_father <- renderPlot( visualisation_parts$journey_father )
  output$journey_full_sib <- renderPlot( visualisation_parts$journey_full_sib )
  output$journey_half_sib <- renderPlot( visualisation_parts$journey_half_sib )
  
  #### output pre_post ----
  output$pre_mother_1 <- renderPlot( visualisation_parts$mother_pre_post$pre[[1]] )
  output$pre_mother_2 <- renderPlot( visualisation_parts$mother_pre_post$pre[[2]] )
  output$pre_mother_3 <- renderPlot( visualisation_parts$mother_pre_post$pre[[3]] )
  
  output$pre_mother_ui <- renderUI({
    req(visualisation_parts$mother_pre_post)
    req(visualisation_parts$mother_pre_post$pre)
    req(visualisation_parts$mother_pre_post$pre[[1]])
    
    lapply(1:length(visualisation_parts$mother_pre_post$pre),
           function(x){ plotOutput(paste0("pre_mother_",x), height = 200) })
  })
  
  #
  #
  # NEED TO COMPLETE POST FOR MOTHER
  #
  # AND DUPLICATE FOR EVERY OTHER ROLE
  #
  #
  
  #### output general ----
  
  
  ## other ----
  
}