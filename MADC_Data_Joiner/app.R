#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(DT)

DT_OPTIONS <- list(pageLength = 5, 
                   lengthMenu = c(5, 10, 25, 50),
                   scrollX = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("MADC Data Joiner"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # file X upload
      fileInput(inputId = "data_x",
                label = "X",
                multiple = FALSE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"
      ),
      # file Y upload
      fileInput(inputId = "data_y",
                label = "Y",
                multiple = FALSE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"
      ),
      # file X join fields
      textInput(inputId = "data_x_join_fields",
                label = "X Join Fields",
                placeholder = "ptid"),
      # file Y join fields
      textInput(inputId = "data_y_join_fields",
                label = "Y Join Fields",
                placeholder = "subject_id"),
      
      # Input: join type (inner, outer left, outer full)
      radioButtons(inputId = "join_type",
                   label = "Join Type",
                   choices = c("Left"  = "left",
                               "Inner" = "inner",
                               "Outer" = "outer"),
                   select = "left",
                   inline = TRUE),
      
      # Input: `redcap_event_name` switch ----
      radioButtons(inputId = "ren_switch",
                   label = "Keep `redcap_event_name`",
                   choices = c("no"  = "no",
                               "yes" = "yes"),
                   selected = "no",
                   inline = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(width = 6,
               h1("X"),
               # textOutput(outputId = "data_x_dims_out"),
               dataTableOutput(outputId = "data_x_out")),
        column(width = 6,
               h1("Y"),
               # textOutput(outputId = "data_y_dims_out"),
               dataTableOutput(outputId = "data_y_out"))
      ),
      hr(), hr(),
      h1("Z"),
      # textOutput(outputId = "data_z_dims_out"),
      dataTableOutput(outputId = "data_z_out")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Get data
  
  df_data_x <- reactive({ 
    df <- read.csv(file = input$data_x$datapath, stringsAsFactors = FALSE) %>% 
      filter(stringr::str_detect(ptid, "^UM\\d{8}$"))
    if (input$ren_switch == "no") {
      return(select(df, -redcap_event_name))
    } else {
      return(df)
    }
  })
  
  df_data_y <- reactive({ 
    df <- read.csv(file = input$data_y$datapath, stringsAsFactors = FALSE) %>% 
      filter(stringr::str_detect(subject_id, "^UM\\d{8}$")) %>% 
      filter(subject_id >= "UM00000543")
    if (input$ren_switch == "no") {
      return(select(df, -redcap_event_name))
    } else {
      return(df)
    }
  })
  
  x_join_fields <- reactive({
    str_split(input$data_x_join_fields, ",")[[1]] %>% 
      str_trim()
  })
  
  y_join_fields <- reactive({
    str_split(input$data_y_join_fields, ",")[[1]] %>%
      str_trim()
  })
  
  df_data_z <- tibble()
  
  df_data_z <- reactive({
    
    req(input$data_x, input$data_y,
        x_join_fields(), y_join_fields(),
        x_join_fields() %in% names(df_data_x()),
        y_join_fields() %in% names(df_data_y()),
        length(x_join_fields()) == length(y_join_fields()))
    
    if (input$join_type == "left") {
      df_data_z <-
        left_join(x = df_data_x(),
                  y = df_data_y(),
                  by = rlang::set_names(y_join_fields(), x_join_fields())) %>% 
        arrange(!!!syms(x_join_fields()))
    } else if (input$join_type == "inner") {
      df_data_z <-
        inner_join(x = df_data_x(),
                   y = df_data_y(),
                   by = rlang::set_names(y_join_fields(), x_join_fields())) %>% 
        arrange(!!!syms(x_join_fields()))
    } else if (input$join_type == "outer") {
      df_data_z <-
        full_join(x = df_data_x(),
                  y = df_data_y(),
                  by = rlang::set_names(y_join_fields(), x_join_fields())) %>% 
        arrange(!!!syms(x_join_fields()))
    }
    
    return(df_data_z)
  })
  
  # Outputs
  
  output$data_x_out <- renderDataTable({
    req(input$data_x)
    datatable(df_data_x(), DT_OPTIONS)
  })

  
  output$data_y_out <- renderDataTable({
    req(input$data_y)
    datatable(df_data_y(), DT_OPTIONS)
  })
  
  output$data_z_out <- renderDataTable({
    req(input$data_x, input$data_y,
        x_join_fields(), y_join_fields(),
        x_join_fields() %in% names(df_data_x()),
        y_join_fields() %in% names(df_data_y()))
    datatable(df_data_z(), DT_OPTIONS)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
