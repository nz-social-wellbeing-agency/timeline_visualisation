#' ================================================================================================ #
#' Description: R Shiny global setup scape
#'
#' Input:
#'
#' Output:
#'
#' Author: Simon Anastasiadis
#'
#' Dependencies: corresponding ui and server files
#'
#' Notes:
#'
#' Issues:
#'
#' History (reverse order): 
#' 2019 Mar 15 SA first complete prototype, core dashboard functionality complete
#' 2019 Mar 06 AK v0.1 addition of save/load functionality
#' 2019 Feb 25 SA v0
#' ================================================================================================ #

# to support development
setwd('C:/NotBackedUp/shiny apps/timeline_visualisation')

## required packages ----
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(readxl)

## parameters ----
JOURNEY_LINE_MARGIN <- 0.05
HEIGHT_PIXELS <- 50
MAX_PRE_POST_TYPES <- 3

## data load ----
data_control_file <- "./www/data_controls.xlsx"
input_data_file <- "./www/input_data.xlsx"

group_controls <- read_excel(data_control_file, sheet = "GROUP")
role_controls <- read_excel(data_control_file, sheet = "ROLE")
description_controls <- read_excel(data_control_file, sheet = "DESCRIPTION")

journey_data <- read_excel(input_data_file, sheet = "JOURNEY")
totals_data <- read_excel(input_data_file, sheet = "TOTALS")
categorical_data <- read_excel(input_data_file, sheet = "CATEGORICAL")

## data intermediaries ----

# named list of groups
group_list <- group_controls %>%
  select(group_display_type, group_type_display_order) %>%
  distinct() %>%
  arrange(group_type_display_order)

group_list <- sapply(group_list$group_display_type, USE.NAMES = TRUE,
                     FUN = function(x){
                       tmp <- group_controls %>%
                         filter(group_display_type == x) %>%
                         arrange(group_display_order)
                       return(tmp$group_display_name)
                     })
  
# vector of roles
role_list <- role_controls %>%
  arrange(role_display_order)
role_list <- role_list$role_display_name

# named list of journey descriptors
journey_description_list <- description_controls %>%
  semi_join(journey_data, by = c("source", "description")) %>%
  select(description_display_type, type_display_order) %>%
  distinct() %>%
  arrange(type_display_order)

journey_description_list <- sapply(journey_description_list$description_display_type, USE.NAMES = TRUE,
                                   FUN = function(x){
                                     tmp <- description_controls %>%
                                       filter(description_display_type == x) %>%
                                       semi_join(journey_data, by = c("source", "description")) %>%
                                       arrange(description_display_order)
                                     return(tmp$description_display_name)
                                   })

# named list of totals descriptors
pre_post_totals <- totals_data %>%
  filter(position %in% c("pre", "post"))

pre_post_list <- description_controls %>%
  semi_join(pre_post_totals, by = c("source", "description")) %>%
  select(description_display_type, type_display_order) %>%
  distinct() %>%
  arrange(type_display_order)

pre_post_description_list <- sapply(pre_post_list$description_display_type, USE.NAMES = TRUE,
                                   FUN = function(x){
                                     tmp <- description_controls %>%
                                       filter(description_display_type == x) %>%
                                       semi_join(pre_post_totals, by = c("source", "description")) %>%
                                       arrange(description_display_order)
                                     return(tmp$description_display_name)
                                   })

# named list of general descriptors
totals_measures <- totals_data %>%
  select("source", "description", "position") %>%
  filter(position %in% c("general_post", "general_pre", "general", "general_percent")) %>%
  distinct()
categorical_measures <- categorical_data %>%
  select("source", "description", "position") %>%
  filter(position %in% c("general_post", "general_pre", "general", "general_percent")) %>%
  distinct()
general_measures <- rbind(totals_measures, categorical_measures) %>%
  distinct()

general_list <- description_controls %>%
  semi_join(general_measures, by = c("source", "description")) %>%
  select(description_display_type, type_display_order) %>%
  distinct() %>%
  arrange(type_display_order)

general_description_list <- sapply(general_list$description_display_type, USE.NAMES = TRUE,
                                    FUN = function(x){
                                      tmp <- description_controls %>%
                                        filter(description_display_type == x) %>%
                                        semi_join(general_measures, by = c("source", "description")) %>%
                                        arrange(description_display_order)
                                      return(tmp$description_display_name)
                                    })

## supporting functions ----

update_logicals <- function(logical_list, to_false = NULL, to_true = NULL, toggle = NULL){
  for(x in to_false)
    logical_list[[x]] <- FALSE
  for(x in to_true)
    logical_list[[x]] <- FALSE
  for(x in toggle)
    logical_list[[x]] <- !logical_list[[x]]
  return(logical_list)
}

get_selected_measures <- function(reference_list, input, prefix = NULL, suffix = NULL){
  selected_measures <- lapply(names(reference_list),
                              FUN = function(x){
                                input_checkboxgroup <- paste0(prefix, gsub(" ","_",x), suffix)
                                return(input[[input_checkboxgroup]])
                              })
  return(unlist(selected_measures, use.names = FALSE))
}


## plot timeline function ----
plot_timeline <- function(group_name, role, selected_measures){
  # stop if no measures
  if(length(selected_measures) == 0)
    return(list(figure = NULL, figure_height = NA))
  
  # trim to measures of interest
  df <- journey_data %>% 
    ungroup() %>%
    left_join(group_controls, by = "group_name") %>%
    filter(group_display_name == !!enquo(group_name)) %>%
    left_join(role_controls, by = "role") %>%
    filter(role_display_name == !!enquo(role)) %>%
    left_join(description_controls, by = c("source", "description")) %>%
    filter(description_display_name %in% !!enquo(selected_measures)) %>%
    mutate(percent_with = 100* round(number_contributors / group_size,3)) %>%
    mutate(plot_display_text = paste0(description_display_name," (", percent_with,"%)")) %>%
    gather(key = "period", value = "indicator", `-20`, `-19`, `-18`, `-17`, `-16`, `-15`, `-14`, `-13`, `-12`,
           `-11`, `-10`, `-9`, `-8`, `-7`, `-6`, `-5`, `-4`, `-3`, `-2`, `-1`, `1`, `2`, `3`, `4`, `5`, `6`, 
           `7`, `8`, `9`, `10`, `11`, `12`, `13`) %>%
    select(description_display_name, percent_with, plot_display_text, description_display_colour,
           type_display_order, description_display_order, period, indicator) %>%
    filter(indicator != 0) %>%
    mutate(period = as.numeric(period))
  # calculate height
  figure_height <- df %>% ungroup() %>% select(description_display_name) %>% distinct() %>% nrow()
  # stop if no measures
  if(figure_height == 0)
    return(list(figure = NULL, figure_height = NA))

  # add vertical height
  tmp <- df %>%
    select(description_display_name, type_display_order, description_display_order) %>%
    distinct() %>%
    mutate(sort_order = 1000 * type_display_order + description_display_order) %>%
    arrange(sort_order)
  tmp <- tmp %>%
    mutate(height = -(1:nrow(tmp)))
  
  
  df <- df %>%
    inner_join(tmp, by = 'description_display_name')
  
  # set rectangle limits
  df <- df %>%
    mutate(x_min = ifelse(sign(period) == -1, period, period - 1),
           x_max = ifelse(sign(period) == -1, period + 1, period),
           y_min = height + 0.5 - (0.5 - JOURNEY_LINE_MARGIN) * percent_with / 100,
           y_max = height + 0.5 + (0.5 - JOURNEY_LINE_MARGIN) * percent_with / 100,
           y_min_grey = height + 0.5 - (0.5 - JOURNEY_LINE_MARGIN),
           y_max_grey = height + 0.5 + (0.5 - JOURNEY_LINE_MARGIN))
  
  # plot
  p <- ggplot(data = df) +
    geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min_grey, ymax = y_max_grey), fill = 'grey') +
    geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = description_display_colour)) +
    geom_vline(xintercept = 0, colour = 'orange', linetype = 'dashed', size = 1.5) +
    geom_text(aes(x = -20, y = height + 1 - JOURNEY_LINE_MARGIN, label = plot_display_text), hjust = 0)
  
  p <- p  +
    theme_bw() +
    theme(axis.ticks.y = element_blank(), legend.position = "none") +
    scale_y_continuous(breaks = seq(0,-figure_height), minor_breaks = NULL,
                       name = NULL, labels = NULL, limits = c(-figure_height,0)) +
    xlab('Time from birth (fortnights)') +
    xlim(-20,13) +
    scale_fill_manual(values = with(df, setNames(description_display_colour, description_display_colour)))
    
  # return
  return(list(figure = p, figure_height = figure_height))
}

## plot pre/post function ----
plot_pre_post <- function(group_name, role, selected_measures){
  # stop if no measures
  if(length(selected_measures) == 0)
    return(list(figure_pre = NULL, figure_post = NULL))
  
  # trim to measures of interest
  df <- totals_data %>%
    ungroup() %>%
    filter(position %in% c("pre", "post")) %>%
    left_join(group_controls, by = "group_name") %>%
    filter(group_display_name == !!enquo(group_name)) %>%
    left_join(role_controls, by = "role") %>%
    filter(role_display_name == !!enquo(role)) %>%
    left_join(description_controls, by = c("source", "description")) %>%
    filter(description_display_name %in% !!enquo(selected_measures)) %>%
    mutate(display_value = value / group_size)
    
  plot_types <- df %>%
    select(value_type) %>%
    distinct() %>%
    unlist(use.names = FALSE)
    
  # plot pre
  pre_plots <- lapply(plot_types, function(x){ plot_pre_post_figure(df, "pre", x) })
  
  # plot post
  post_plots <- lapply(plot_types, function(x){ plot_pre_post_figure(df, "post", x) })
  
  return(list(pre = pre_plots, post = post_plots))
}

# supporting function to produce the actual plots
# allowing for mutiple types of plots pre & post
plot_pre_post_figure <- function(df, position, value_type){
  # filter
  df <- df %>%
    filter(position == !!enquo(position),
           value_type == !!enquo(value_type)) %>%
    mutate(tmp_display_order = 10000 * type_display_order + description_display_order) %>%
    select(value_display_name, display_value, description_display_name, description_display_colour, tmp_display_order)
  # set factor order
  df$description_display_name = factor(df$description_display_name,
                                       levels = df$description_display_name[order(df$tmp_display_order)])
  
  # handle percentages
  if(grepl("percent",df$value_display_name[1],ignore.case = TRUE))
    df$display_value = 100 * df$display_value
    
  # plot
  p <- ggplot(data = df) +
    geom_col(aes(x = description_display_name, y = display_value, fill = description_display_name)) +
    theme_bw() +
    theme(legend.position = "none") +
    ylab(df$value_display_name[1]) +
    xlab("") +
    scale_fill_manual(values = with(df, setNames(description_display_colour, description_display_name))) +
    coord_flip()
    
  return(p)
}


## plot general functions ----
plot_general <- function(group_name, selected_measures){
  # stop if no measures
  if(length(selected_measures) == 0)
    return(NULL)
  
  # trim to measures of interest
  df_totals <- totals_data %>%
    ungroup() %>%
    filter(substring(position, 1, nchar("general")) == "general") %>%
    left_join(role_controls, by = "role") %>%
    left_join(group_controls, by = "group_name") %>%
    filter(group_display_name == !!enquo(group_name)) %>%
    left_join(description_controls, by = c("source", "description")) %>%
    filter(description_display_name %in% !!enquo(selected_measures)) %>%
    mutate(display_value = value / group_size)
  
  totals_measures <- df_totals %>%
    select(value_type) %>%
    distinct() %>%
    unlist(use.names = FALSE)
  
  df_categorical <- categorical_data %>%
    ungroup() %>%
    filter(substring(position, 1, nchar("general")) == "general") %>%
    left_join(role_controls, by = "role") %>%
    left_join(group_controls, by = "group_name") %>%
    filter(group_display_name == !!enquo(group_name)) %>%
    left_join(description_controls, by = c("source", "description")) %>%
    filter(description_display_name %in% !!enquo(selected_measures)) %>%
    mutate(display_value = value / group_size)
    
  
  categorical_measures <- df_categorical %>%
    select(description) %>%
    distinct() %>%
    unlist(use.names = FALSE)
  
  plot_totals_list <- lapply(totals_measures, function(x){ plot_general_figure(df_totals, x) })
  plot_categorical_list <- lapply(categorical_measures, function(x){ plot_categorical_figure(df_categorical, x) })

  return(c(plot_totals_list, plot_categorical_list))
}  
  
# supporting function to produce the actual plots
# allowing for mutiple types of general plots
plot_general_figure <- function(df, measure){
  # filter
  df <- df %>%
    ungroup() %>%
    filter(value_type == !!enquo(measure)) %>%
    mutate(tmp_display_order = 10000 * type_display_order + description_display_order) %>%
    select(value_display_name, display_value, description_display_name, description_display_colour,
           role_display_name, tmp_display_order)
  
  # handle percentages
  if(grepl("percent",df$value_display_name[1],ignore.case = TRUE))
    df$display_value = 100 * df$display_value
  
  # plot
  p <- ggplot(data = df) +
    geom_col(aes(x = description_display_name, y = display_value, fill = description_display_name)) +
    facet_grid(cols = vars(role_display_name)) +
    theme_bw() +
    theme(legend.position = "none") +
    ylab(df$value_display_name[1]) +
    xlab("") +
    scale_fill_manual(values = with(df, setNames(description_display_colour, description_display_name))) +
    coord_flip() 
  
  return(p)
}

# supporting function to produce the actual plots
# allowing for mutiple types of general plots
plot_categorical_figure <- function(df, measure){
  # filter
  df <- df %>%
    ungroup() %>%
    filter(description == !!enquo(measure)) %>%
    select(category, category_display_name, display_value, description_display_name, description_display_colour,
           category_display_order, role_display_name)
  # set factor order
  df_factors <- df %>% select(category_display_name, category_display_order) %>% distinct()
  df$category_display_name = factor(df$category_display_name,
                                    levels = df_factors$category_display_name[order(df_factors$category_display_order)])
  
  # handle percentages
  df$display_value = 100 * df$display_value
  
  # plot
  p <- ggplot(data = df) +
    geom_col(aes(x = category_display_name, y = display_value, fill = description_display_name)) +
    facet_grid(cols = vars(role_display_name)) +
    theme_bw() +
    theme(legend.position = "none") +
    ylab("Percent") +
    xlab(df$description_display_name[[1]]) +
    scale_fill_manual(values = with(df, setNames(description_display_colour, description_display_name))) +
    coord_flip()
  
  return(p)
}

