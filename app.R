# ---- bring in pkgs ----- 
{
  # library(dplyr)
  library(DT) 
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
}
# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}
# ----- bring in column names ---- 
# first grab teh url 
column_url <- "https://raw.githubusercontent.com/benjaminhlina/energy-density-data-form/refs/heads/main/data-raw/data-raw/column_names_tbl_sample.csv"
# read csv 
column_names <- read.csv(column_url)

# ----- bring in fw fish species ---- 
fw_sp_url <- "https://raw.githubusercontent.com/benjaminhlina/energy-density-data-form/refs/heads/main/data-raw/data-raw/fw_fish_genus_sepcies.csv"

fw_fish_genus_sp <- read.csv(fw_sp_url)

ui <- dashboardPage(
  # skin = "green",
  dashboardHeader(title = "Energy Density Data Form",  titleWidth = 500),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Enter Sample Data", tabName = "enter_sample_data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    # tags$style(".skin-green .main-sidebar {background-color: #444; }"), 
    tabItems(
      tabItem(tabName = "enter_sample_data",
              fluidRow(
                box(width = 4, title = "Input Data", solidHeader = TRUE, status = "primary",
                    textInput("project_id", "Project Name"),
                    dateInput("date", "Date (YYYY-MM-DD)", value = Sys.Date(), 
                              format = "yyyy-mm-dd"),
                    selectizeInput("scientific_name", "Scientific Name", 
                                   choices = NULL,
                                   selected = NULL,
                                   options = list(
                                     create = FALSE,
                                     placeholder = 'Start typing a Genus and species ...'
                                   )),
                    selectInput("wild_lab", "Wild or Lab", choices = c("wild", "lab"), selected = NULL),
                    selectInput("lifestage", "Lifestage", choices = c("fry", "larva", "juvenile", "adult"), selected = NULL),
                    numericInput("age", "Age", value = NA, min = 0),
                    selectInput("sex", "Sex", choices = c("female", "male", "unknown"), selected = NULL),
                    numericInput("fork_length", "Fork Length (mm)", value = NA, min = 0),
                    numericInput("total_length", "Total Length (mm)", value = NA, min = 0),
                    numericInput("weight", "Weight (g)", value = NA, min = 0),
                    selectInput("composite", "Composite", choices = c("individual", 
                                                                      "composite", 
                                                                      "mean"), selected = NULL),
                    numericInput("composite_n", "Composite n", value = NA, min = 0),
                    selectInput("tissue_type", "Tissue Type",
                                choices = c("muscle", "liver", "stomach", 
                                            "scales", "whole body"), 
                                selected = NULL),
                    selectInput("sample_procedure", "Sample Procedure", 
                                choices = c("ground", "dried", "wet"), 
                                selected = NULL),
                    textInput("waterbody", "Waterbody"),
                    textInput("area", "Area"),
                    textInput("site", "Site"),
                    numericInput("site_depth", "Site Depth", value = NA, min = 0),
                    numericInput("latitude", "Latitude (dd.ddddd)", value = NA, min = 15, max = 85),
                    numericInput("longitude", "Longitude (ddd.ddddd)", value = NA, min = -170, max = -50),
                    textInput("calorimetry_method", "Calorimetry Method"),
                    fileInput("upload_csv", "Upload csv",  
                              accept = c("text/csv", 
                                         "text/comma-separated-values,text/plain", 
                                         ".csv")),
                    checkboxInput("replace_data", "Replace existing data", value = FALSE),
                    actionButton("add_row", "Add Row", icon = icon("plus")), 
                    actionButton("edit_row", "Edit Selected Row", icon = icon("edit")),
                    actionButton("delete_row", "Delete Selected Row", icon = icon("trash")),
                    downloadButton("download", "Download csv")
                ),
                box(width = 8, title = "Table", 
                    solidHeader = TRUE, 
                    status = "info",
                    div(
                      style = "position: fixed;
                      top: 60px;
                      right: 0;
                      width: 50%;
                      height: 80vh;
                      overflow-y: auto;
                      background-color: white;
                      z-index: 999;
                      padding: 10px;
                      box-shadow: -2px 0 5px rgba(0,0,0,0.1);",
                      DTOutput("table")
                    )
                )
              )
      )
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
    if (is.null(input$latitude) || 
        is.na(input$latitude) || input$latitude < 15 || input$latitude > 85) {
      showNotification("Latitude must be between 15 and 85 (North America).", 
                       type = "error")
      return()
    }
    if (is.null(input$longitude) || 
        is.na(input$longitude) || input$longitude < -170 || input$longitude > -50) {
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
    datatable(df(), options = list(pageLength = 5), 
              selection = "single")
  })
  
  
  
  
  observeEvent(input$edit_row, {
    selected <- input$table_rows_selected
    if (length(selected) != 1) {
      showNotification("Select a row to edit.", type = "error")
      return()
    }

    # Validate fields here, same as in add_row...

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

    new_df <- df()
    new_df[selected, ] <- new_row
    df(new_df)
  })

    observeEvent(input$table_rows_selected, {
    selected <- input$table_rows_selected
    if (length(selected) == 1) {
      row <- df()[selected, ]

      updateTextInput(session, "project_id", value = row$project_id)
      updateDateInput(session, "date", value = row$date)
      updateTextInput(session, "scientific_name", value = row$scientific_name)
      updateSelectInput(session, "wild_lab", selected = row$wild_lab)
      updateSelectInput(session, "lifestage", selected = row$lifestage)
      updateTextInput(session, "age", value = row$age)
      updateSelectInput(session, "sex", selected = row$sex)
      updateNumericInput(session, "fork_length", value = row$fork_length)
      updateNumericInput(session, "total_length", value = row$total_length)
      updateNumericInput(session, "weight", value = row$weight)
      updateCheckboxInput(session, "composite", value = row$composite)
      updateNumericInput(session, "composite_n", value = row$composite_n)
      updateSelectInput(session, "tissue_type", selected = row$tissue_type)
      updateTextInput(session, "sample_procedure", value = row$sample_procedure)
      updateTextInput(session, "waterbody", value = row$waterbody)
      updateTextInput(session, "area", value = row$area)
      updateTextInput(session, "site", value = row$site)
      updateNumericInput(session, "site_depth", value = row$site_depth)
      updateNumericInput(session, "latitude", value = row$`latitude (dd.ddddd)`)
      updateNumericInput(session, "longitude", value = row$`longitude (ddd.ddddd)`)
      updateSelectInput(session, "calorimetry_method", selected = row$calorimetry_method)
    }
  })
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    
    tryCatch({
      uploaded <- read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE)
      
      # Optional: Check if uploaded CSV matches the expected structure
      expected_cols <- names(df())
      if (!all(expected_cols %in% names(uploaded))) {
        showNotification("Uploaded file does not have the expected columns.", type = "error")
        return()
      }
      
      # Combine uploaded with existing data
      combined <- rbind(df(), uploaded)
      df(combined)
      showNotification("CSV uploaded and data added.", type = "message")
    }, error = function(e) {
      showNotification(paste("Upload failed:", e$message), type = "error")
    })
  })
  
  output$download <- downloadHandler(
    filename = function() {
      df_current <- df()
      paste("energy_density_tbl_samples_", 
            unique(df_current$project_id),
            "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Use write.csv to a string, then write as plain text
      tmp <- tempfile()
      write.csv(df(), tmp, row.names = FALSE)
      writeLines(readLines(tmp), file)
    },
    contentType = "text/csv"
  )
}

shinyApp(ui, server)
