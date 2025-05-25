# ---- bring in pkgs ----- 
{
  # library(dplyr)
  library(DT) 
  library(shiny)
  library(writexl)
}

# ----- bring in column names ---- 
# first grab teh url 
column_url <- "https://raw.githubusercontent.com/benjaminhlina/energy-density-data-form/refs/heads/main/data-raw/data-raw/column_names_tbl_sample.csv"
# read csv 
column_names <- read.csv(column_url)

# ----- bring in fw fish species ---- 
fw_sp_url <- "https://raw.githubusercontent.com/benjaminhlina/energy-density-data-form/refs/heads/main/data-raw/data-raw/fw_fish_genus_sepcies.csv"

fw_fish_genus_sp <- read.csv(fw_sp_url)

# ---- shiny app ui ----- 
ui <- fluidPage(
  titlePanel("Energy Density Data Form"),
  sidebarLayout(
    sidebarPanel(
      textInput("project_id", "Project Name"),
      dateInput("date", "Date (YYYY-MM-DD)", value = Sys.Date(), 
                format = "yyyy-mm-dd"),
      selectizeInput("scientific_name", "Scientific Name", 
                     choices = NULL,
                     selected = NULL,
                     options = list(
                       create = FALSE,
                       placeholder = 'Start typing a Genus and speices ...'
                     )),
      selectInput("wild_lab", "Wild or Lab",
                  choices = c("wild", "lab"), selected = NULL),
      selectInput("lifestage", "Lifestage",
                  choices = c("fry", "larva", "juvenile", "adult"), 
                  selected = NULL),
      numericInput("age", "Age", value = NA, min = 0),
      selectInput("sex", "Sex",
                  choices = c("female", "male", "unknown"), selected = NULL),
      numericInput("fork_length", "Fork Length (mm)", value = NA, min = 0),
      numericInput("total_length", "Total Length (mm)", value = NA, min = 0),
      numericInput("weight", "Weight (g)", value = NA, min = 0),
      selectInput("composite", "Composite", choices = c("individual",
                                                        "composite"
      ), 
      selected = NULL),
      numericInput("composite_n", "Composite n", value = NA, min = 0),
      selectInput("tissue_type", "Tissue Type",
                  choices = c("muscle", "liver", 
                              "stomach", "scales", "whole body"), 
                  selected = NULL),
      selectInput("sample_procedure", "Sample Procedure", 
                  choices = c("ground", "dried", "wet"), 
                  selected = NULL),
      textInput("waterbody", "Waterbody"),
      textInput("area", "Area"),
      textInput("site", "Site"),
      numericInput("site_depth", "Site Depth", value = NA, min = 0),
      numericInput("latitude", "Latitude (dd.ddddd)", value = NA, min = 15, 
                   max = 85),
      numericInput("longitude", "Longitude (ddd.ddddd)", value = NA, 
                   min = -170, max = -50),
      textInput("calorimetry_method", "Calorimetry Method"),
      
      
      actionButton("add_row", "Add Row"),
      downloadButton("download", "Download Excel")
    ),
    
    mainPanel(
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  # ---- limit sci name entry ---- 
  observe({
    updateSelectizeInput(session, "scientific_name",
                         choices = c("", sort(fw_fish_genus_sp$genus_species)),
                         selected = NULL, 
                         server = TRUE,)
  })
  # Empty data frame with predefined columns
  df <- reactiveVal(
    setNames(data.frame(matrix(ncol = length(column_names$columns), 
                               nrow = 0)), 
             column_names$columns)
  )
  
  observeEvent(input$add_row, {
    # valdiate prj id 
    if (is.null(input$project_id) || input$project_id == "") {
      showNotification("Project ID is missing and needs to be a character 
                       string.", 
                       type = "error")
      return()
    }
    
    
    
    
    
    # Validate lat/lon
    if (is.na(input$latitude) || input$latitude < 15 || input$latitude > 85) {
      showNotification("Latitude must be between 15 and 85 (North America).", 
                       type = "error")
      return()
    }
    if (is.na(input$longitude) || input$longitude < -170 || input$longitude > -50) {
      showNotification("Longitude must be between -170 and -50 (North America).", 
                       type = "error")
      return()
    }
    
    
    new_row <- data.frame(
      project_id = input$project_id,
      date = input$date,
      scientific_name = input$scientific_name,
      wild_lab = input$wild_lab,
      lifestage = input$lifestage,
      age = input$age,
      sex = input$sex,
      fork_length = input$fork_length,
      total_length = input$total_length,
      weight = input$weight,
      composite = input$composite,
      composite_n = input$composite_n,
      tissue_type = input$tissue_type,
      sample_procedure = input$sample_procedure,
      waterbody = input$waterbody,
      area = input$area,
      site = input$site,
      site_depth = input$site_depth,
      `latitude (dd.ddddd)` = input$latitude,
      `longitude (ddd.ddddd)` = input$longitude,
      calorimetry_method = input$calorimetry_method,
      stringsAsFactors = FALSE
    )
    
    df(rbind(df(), new_row))
  })
  
  output$table <- renderDT({
    datatable(df(), options = list(pageLength = 5))
  })
  
  output$download <- downloadHandler(
    filename = function() {
      df_current <- df()
      paste("energy_density_tbl_samples_", 
            unique(df_current$project_id),
            "_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(df(), file)
    }
  )
}

shinyApp(ui, server)
