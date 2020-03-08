library(shiny)
library(shinydashboard)
library(clipr)
library(rintrojs)
library(dplyr)

# URL(s) to Google Form(s) for Evaluation 
# workshop_form <- function(id) {
#   paste0("https://docs.google.com/forms/d/e/1FAIpQLScTMLTJ1ccfBmjdEPhZfk5CyQwqSAW5AUJyDkFxc7Q9ZW6VPQ/viewform?usp=pp_url&entry.840333480=",
#          id)
# }
# contribution_form <- function(id) {
#   paste0("https://docs.google.com/forms/d/e/1FAIpQLSezGbJ1JmgOwDI5BLl28gXp3YQfoFXq8GoMon3k9PZcePCF_w/viewform?usp=pp_url&entry.840333480=",
#          id)
# }

# To pre-populate an element in the evaluation form, drill down to this element
# using Chrome developer tools and get element id as, e.g., name="entry.1845598857"
workshop_form <- function(id) {
  paste0("https://docs.google.com/forms/d/e/1FAIpQLSc4iDxze_Xm8cnYuVuMDnR6rVI7kj7gjL8f5ow4hd2zDJk-mw/viewform?usp=pp_url&entry.1845598857=", id)
}

# Open link in a new window
window_open <- function(form_link) {
  paste0("window.open('", form_link, "', '_blank')")
}
# Open link to evaluation table for a given type of evaluation, if multiple
window_open_eval <- function(id, type) {
  if (type == "workshop") {
    link <- workshop_form(id)
  } else if (type == "contribution") {
    link <- contribution_form(id)
  }
  window_open(link)
}


# Excel file(s) with session information
workshop_path     <- "./Bioc2020 talks and posters review.xlsx"
# contribution_path <- "./BioC2020_talks_posters.xlsx"
# 
workshop_table <- readxl::read_excel(workshop_path)
# contribution_table_raw <- readxl::read_excel(contribution_path)
# contribution_table <- contribution_table_raw[!is.na(contribution_table_raw$Id),]

# Alternatively, read directly from a GSheet that has been shared for view 
# library(gsheet) # install.packages("gsheet")
# workshop_table <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1kaYuJKlt7sNSiLumQYnZau5TgTx9W9SkSQsQa8xBCLM/edit?usp=sharing")
# # Remove duplicate submissions, ignoring Timestamp
# workshop_table <- workshop_table[!duplicated(workshop_table[, colnames(workshop_table) != "Timestamp"]), ]

# Get unique reviewer's names
reviewers_names <- workshop_table %>% dplyr::select(starts_with("Reviewer")) %>% unlist %>% unique() 
reviewers_names <- reviewers_names[!is.na(reviewers_names)] # Remove NA reviwers

# Currently, all columns need to be manually recoded
# colnames(workshop_table) 
# Which columns to display
preselected_cols <- c("Name of presenter",
                      "Affiliation of presenter",
                      "Title of talk / poster",
                      "What are you proposing to present?")
# Compact table to display
abstract_table_compact <- workshop_table[, preselected_cols]
  
  # UI definition -----------------------------------------------------------
  assessr_ui <- shinydashboard::dashboardPage(
    skin = "black",
    rintrojs::introjsUI(),
    # header definition -------------------------------------------------------
    header = shinydashboard::dashboardHeader(
      title = "assessr - BioC2020",
      titleWidth = 350
    ),
    
    # sidebar definition ------------------------------------------------------
    sidebar = shinydashboard::dashboardSidebar(
      width = 250,
      shinydashboard::menuItem(
        text = "Sessions Settings", icon = icon("cog"),
        startExpanded = TRUE,
        selectInput(
          inputId = "submissionType",
          label = "Type of contribution",
          # choices = c("Workshops" = "workshop", "Talks and other sessions" = "contribution"),
          choices = c("Abstracts" = "workshop"),
          selected =  "Abstracts", 
          multiple = FALSE
        ),
        selectInput(
          inputId = "cols_abstract",
          label = "Columns to display",
          choices = colnames(workshop_table),
          selected =  preselected_cols, 
          multiple = TRUE
        ),
        selectInput(
          inputId = "reviewer",
          label = "Reviewer name",
          choices = c("All", reviewers_names),
          selected =  "All", 
          multiple = FALSE
        ),
        
        actionButton(
          inputId = "tour_assessr",
          icon = icon("question-circle"),
          label = "How does assessr work?",
          class = "btn-info"
        ),
        HTML("<!-- version 1.1.0 -->")
      )
    ),
    
    # body definition ---------------------------------------------------------
    body = shinydashboard::dashboardBody(
      id = "main-app",
      
      fluidRow(
        column(
          width = 6,
          DT::dataTableOutput("DT_abstracts")
        ),
        column(
          width = 6,
          hr(),
          uiOutput("session_abstract")
        )
      )
    )
  )

  # Server definition -------------------------------------------------------
  assessr_server <- function(input, output, session) {
    abstract_table <- reactive({
      message("input$submissionType: ", input$submissionType)
      if (input$submissionType == "workshop") {
        abstract_t <- workshop_table
      } else if (input$submissionType == "contribution") {
        abstract_t <- contribution_table
      }
      abstract_t
    })
    
    current_dt <- reactive({
      message("input$reviewer: ", input$reviewer)
      # ToDo: Automate reviewers column selection
      mydt <- abstract_table()[abstract_table()$`Reviewer 1` %in% input$reviewer |
                                 abstract_table()$`Reviewer 2` %in% input$reviewer |
                                 abstract_table()$`Reviewer 3` %in% input$reviewer |
                                 abstract_table()$`Reviewer 4` %in% input$reviewer |
                                 abstract_table()$`Reviewer 5` %in% input$reviewer |
                                 abstract_table()$`Reviewer 6` %in% input$reviewer |
                                 abstract_table()$`Reviewer 7` %in% input$reviewer |
                                 abstract_table()$`Reviewer 8` %in% input$reviewer |
                                 abstract_table()$`Reviewer 9` %in% input$reviewer |
                                 abstract_table()$`Reviewer 10` %in% input$reviewer |
                                 abstract_table()$`Reviewer 11` %in% input$reviewer |
                                 abstract_table()$`Reviewer 12` %in% input$reviewer |
                                 "All" %in% input$reviewer,
                               union(input$cols_abstract, "Timestamp")]
      return(mydt)
    })
    
    output$session_abstract <- renderUI({
      s <- input$DT_abstracts_rows_selected
      if(length(s) == 0)
        return(h3("Select an abstract from the table to display the full info"))
      
      search_id       <- current_dt()[s, ]$Timestamp
      this_submission <- abstract_table()[abstract_table()$Timestamp == search_id, ]
      this_id         <- this_submission$Timestamp
      this_title      <- this_submission$`Title of talk / poster`
      this_authors    <- this_submission$`List of authors`
      this_abstract   <- this_submission$`Abstract (up to 300 words)`
      this_format     <- this_submission$`What are you proposing to present?`
      this_keywords   <- this_submission$`Keywords (up to 5, comma-separated)`
      this_link       <- this_submission$`If you are presenting an R/Bioconductor package, please provide a URL to the package.`

      # form_id:
      s <- input$DT_abstracts_rows_selected
      if(length(s) == 0)
        return(NULL)
      
      this_submission <-  current_dt()[s, ]
      this_author <- this_submission$`Name of presenter`
      this_title  <- this_submission$`Title of talk / poster`
      
      form_id <- gsub( "'", "", paste(this_author, this_title, sep = "|"))
      
      message("type: ", input$submissionType)
      message("form_id(): ", form_id)
      
      return(
        tagList(
          h3("Title: "),
          tags$b(h2(this_title)),
          h4("Contribution identifier: "),
          tags$p(paste(this_author, this_title, sep = "|")),
          h3("Abstract: "),
          tags$p(this_abstract),
          h3("Format:"),
          p(this_format),
          h3("Keywords:"),
          tags$b(this_keywords),
          h3("Link:"),
          p(this_link),

          shiny::actionButton(
            inputId = "launch_gform", label = "Open the Google Form to insert your evaluation", 
            icon = icon("database"), 
            onclick =window_open_eval(form_id, input$submissionType),
            class = "btn-success"
          )
        )
      )
      })
    
    output$DT_abstracts <- DT::renderDataTable({
      DT::datatable(
        current_dt(),
        style = "bootstrap", 
        rownames = FALSE, 
        filter = "top",
        selection = list(mode = "single"),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          lengthMenu = c(5, 10, 25, 50, 100, nrow(current_dt()))
        )
      )
    })
    
    observeEvent(input$tour_assessr, {
      tour <- read.delim("tour_info.txt",
                         sep = ";", stringsAsFactors = FALSE,
                         row.names = NULL, quote = "")
      rintrojs::introjs(session, options = list(steps = tour))
    })
    
  }  
  
  shinyApp(ui = assessr_ui, server = assessr_server)
  
