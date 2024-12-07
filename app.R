# Load required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(config)
library(pdftools)
library(httr)
library(jsonlite)
library(tidyverse)
library(waiter)
library(DT)
library(base64enc)
library(writexl)
library(readxl)
library(yaml)

# Load configuration from the parent directory
config <- yaml::read_yaml(file = "config.yml")  # Adjusted path to config.yml

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = tags$div(
      tags$img(src = "bird.png", style = "height:3em; vertical-align:middle; padding-right:1px;"),
      "AIDE"
    ),
    titleWidth = "230px"  # Adjust the width as needed
  ),
  
  
  
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
    menuItem("Start Here", tabName = "Start"),
    menuItem("Analyze", tabName = "Analyze"),
    menuItem("Cite", tabName = "Cite")
  )
),
  
  # Main content
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/css/custom.css"),
      tags$style(HTML("
        .pdf-container {
          height: 72vh;
          overflow-y: auto; 
          overflow-x: hidden; /* Prevent horizontal scrolling */
          width: 100%;
          padding: 15px;
          border: 1px solid #ddd;
          background-color: #f9f9f9;
          box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
          text-align: center;
          display: flex;
          flex-direction: column; /* Stack images vertically */
          align-items: center; /* Center align each image */
          position: relative;
        }
        .pdf-image {
          max-width: 100%;
          height: auto;
          display: block;
          margin-bottom: 10px; /* Add spacing between pages */
        }
        .pdf-container img {
          width: 100%;
          height: auto;
          display: block;
          margin: 0 auto;
        }
        .modal-dialog {
              position: fixed !important;
              top: 50% !important;
              left: 50% !important;
              transform: translate(-50%, -50%) !important;
              min-width: 400px !important;
          }
          .modal-content {
              background-color: white !important;
              border-radius: 10px !important;
              padding: 20px !important;
          }
          .progress {
              height: 20px !important;
              margin: 15px 0 !important;
              background-color: #f5f5f5 !important;
              border-radius: 4px !important;
              box-shadow: inset 0 1px 2px rgba(0,0,0,.1) !important;
          }
          .progress-bar {
              background-color: #337ab7 !important;
              transition: width .6s ease !important;
          }
          #progress-detail {
              margin-top: 10px !important;
              color: #666 !important;
              font-size: 14px !important;
          }
          #time-remaining {
          font-style: italic;
          color: #666;
          margin-top: 8px;
      }

      #cancelPdfProcess {
          margin-top: 10px;
      }

      .btn-danger {
          background-color: #dc3545;
          border-color: #dc3545;
          color: white;
      }

      .btn-danger:hover {
          background-color: #c82333;
          border-color: #bd2130;
      }
      .highlight-overlay {
        position: absolute !important;
        background-color: yellow !important;
        opacity: 0.5 !important;
        pointer-events: none;
        z-index: 1000;
        border-radius: 3px;
        box-shadow: 0 0 5px rgba(0,0,0,0.2);
        left: 0;
        right: 0;
        margin: 0 auto;
        width: 100%;
      }
      #pdfImage {
        position: relative !important;  /* This is crucial */
        overflow-y: auto;
        height: 72vh;
        }
      #pdfImage img {
        width: 100%;
        height: auto;
        display: block;
        margin: 0 auto;
      }
      .highlight-overlay:hover {
        opacity: 0.7;
      }
      .source-info-box {
        transition: opacity 0.3s ease;
      }
      .source-info-box .close-btn:hover {
        color: #333;
      }
      .validate-btn {
        height: 34px;
        color: #28a745;
        background-color: #fff;
        border-color: #28a745;
        border-radius: 4px;
        padding: 6px 12px;
        font-size: 14px;
      }
      .validate-btn-success {
        color: #fff !important;
        background-color: #28a745 !important;
        border-color: #28a745 !important;
      }
      .btn-primary-disabled {
        background-color: #a8d4f3 !important;  /* light blue */
        border-color: #a8d4f3 !important;
        cursor: not-allowed !important;
        opacity: 0.6;
      }
     "))
    ),
    
    # Loading screen
    use_waiter(),
    
    tabItems(
    
    tabItem("Start",
            
            
            
            # UI component
            box(
              width = 12,
              title = "Welcome to AI-Assisted Data Extraction (AIDE)",
              div(
                style = "display: flex; flex-direction: column; gap: 8px;", # Reduced from 15px to 8px
                p("AIDE was developed to greatly accelerate the data extraction process for systematic review and meta-analysis. It relies on you having a Google Gemini API key (which are free). It is brought to you by Noah Schroeder, Ph.D. of", HTML('<a href="https://www.learnmeta-analysis.com/" target="_blank" style="display: inline;">Learn Meta-Analysis LLC.</a>')),
                h4("How to Use This App"),
                p("1) Set up your API settings below. You can obtain a google API key here:", 
                  HTML('<a href="https://aistudio.google.com/" target="_blank" style="display: inline;">Google AI Studio.</a>')),
                p("2) Set up your coding form. Importantly, your first row will be read as prompts by the large language model. The accuracy of the LLM's responses will be influenced very strongly by your prompts. Better prompts = better results."),
                p("3) Upload your coding form. Your coding form should be in Excel format for best results."),
                p("4) Move to the analyze page and upload the PDF file you want to analyze."),
                p("5) Click Analyze button. Your results will autofill under the appropriate prompt."),
                p("6) Review each response. You can view the source information by clicking the Source button. You can record each result by clicking the record button. This will save it to your coding form on your local machine."),
                p("7) When ready to work on the next PDF, simply upload a new one and the coding form will reset."),
                ),
            ),
            box(
              width = 6,
              title = "API Settings",
              div(
                style = "display: flex; flex-direction: column; gap: 8px;", # Reduced from 15px to 8px
                  # API Key input with validate button
                div(
                  style = "display: flex; gap: 10px;",
                  textInput("apiKey", "API Key", 
                            value = tryCatch({
                              config <- yaml::read_yaml(file = "config.yml")
                              config$api$gemini$api_key
                            }, error = function(e) {
                              cat("Error accessing config:", e$message, "\n")
                              ""
                            }),
                            width = "400px"),
                  tags$div(
                    style = "margin-top: 25px;",  # Adjust this value to align perfectly
                    actionButton("validateKey", 
                                 label = "Validate Key",
                                 icon = icon("check"),
                                 class = "validate-btn")
                  )
                ),
                # Model selection
                selectInput("modelSelect", "Model",
                            choices = c("gemini-1.5-flash", "gemini-1.5-pro"),
                            selected = tryCatch({
                              fresh_config <- yaml::read_yaml(file = "config.yml")
                              fresh_config$api$gemini$model
                            }, error = function(e) {
                              "gemini-1.5-pro"
                            }),
                            width = "400px"),
                # Rate limits
                div(
                  style = "margin-top: -5px;", # Negative margin to reduce space
                  h4(style = "margin: 0 0 2px 0;", "Rate Limits"), # Reduced margins
                  p(style = "margin: 0 0 5px 0; font-size: 0.9em;", # Reduced margins and slightly smaller text
                    "Note: The default rate limits are the maximum of the free tier.")
                ),
                div(
                  style = "display: flex; gap: 10px; margin-top: -5px;", # Added negative top margin
                  numericInput("requestsPerMinute", "Requests per Minute",
                               value = tryCatch({
                                 fresh_config <- yaml::read_yaml(file = "config.yml")
                                 fresh_config$api$gemini$requests_per_minute
                               }, error = function(e) {
                                 if(input$modelSelect == "gemini-1.5-flash") 15 else 2
                               }),
                               min = 1,
                               width = "190px"),
                  numericInput("requestsPerDay", "Requests per Day",
                               value = tryCatch({
                                 fresh_config <- yaml::read_yaml(file = "config.yml")
                                 fresh_config$api$gemini$requests_per_day
                               }, error = function(e) {
                                 if(input$modelSelect == "gemini-1.5-flash") 1500 else 50
                               }),
                               min = 1,
                               width = "190px")
                ),
                # Save button
                div(
                  style = "display: flex; justify-content: flex-start;",
                  actionButton("saveSettings", "Save Settings", 
                               icon = icon("save"),
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
            )
              ),
            ),
    
            # Upload Coding Form Button
            box(
              width = 6,
              title = "Coding Form",
              p("Upload your coding form here. Please note that the LLM with use the first row in your form as the prompts, so make sure they are written as LLM prompts!"),
              div(
              style = "display: flex; gap: 10px; width: 100%;",
              fileInput("codingFormFile", "Upload Coding Form (Excel preferred)", 
                        accept = c(".csv", ".xls", ".xlsx"),
                        width = "100%")
            ),
            ),
    ),

      
    tabItem("Analyze",
      fluidRow(
      # PDF Preview on the left
      column(width = 6,
             box(
               width = NULL,
               title = "Upload PDF",
               status = "primary",
               solidHeader = TRUE,
               fileInput("pdfFile", "Choose PDF File",
                         accept = c("application/pdf")),
               div(
                 actionButton("analyzeBtn", "Analyze PDF",
                              class = "btn-primary btn-primary-disabled",  # Start with disabled style
                              disabled = TRUE),  # Start disabled
               ),
               # Add styles to ensure no extra scrollbars
               div(class = "pdf-container",
                   uiOutput("pdfImage"),
                   div(
                     uiOutput("fileStatus")
                   )
               )
             )
      ),
      
      # Chat interface on the right
      column(
        width = 6,
        fluidRow(
          box(
            width = 12,
            title = "Coding Form",
            status = "success",
            solidHeader = TRUE,
            div(
              style = "height: 86.5vh; overflow-y: auto; padding-right: 10px;",
            
            # Generate coding prompts dynamically
            uiOutput("codingPrompts"))
          )
        )
      )
    )
  )
    )
)
)

# Server logic
server <- function(input, output, session) {
  # Initialize reactive values
  rv <- reactiveValues(
    pdf_text = NULL,
    pdf_images = NULL,
    results = NULL,
    prompts = NULL,
    current_prompt_index = 1,
    total_pages = 0,
    current_page = 1,
    temp_files = NULL,
    coding_form = NULL,
    image_timestamp = NULL,
    sources = list(),
    pages = list(),
    codingForm = NULL,
  )
  current_row <- reactiveVal(NULL)
  validationStatus <- reactiveVal("pending") # Can be "pending", "success", or "failed"
  # Save Properly
  original_file_path <- reactiveVal(NULL)
  #control analyze button state:
  observe({
    # Check if both files are present
    is_ready <- !is.null(input$pdfFile) && !is.null(input$codingFormFile)
    
    if (is_ready) {
      # Enable button and update style
      shinyjs::enable("analyzeBtn")
      shinyjs::removeClass("analyzeBtn", "btn-primary-disabled")
    } else {
      # Disable button and update style
      shinyjs::disable("analyzeBtn")
      shinyjs::addClass("analyzeBtn", "btn-primary-disabled")
    }
  })
  
  # API key validation
  observeEvent(input$validateKey, {
    req(input$apiKey)
    
    # Update button to loading state
    validationStatus("loading")
    updateActionButton(session, "validateKey",
                       label = "Validating...",
                       icon = icon("spinner", class = "fa-spin"))
    
    # Show loading notification
    id <- showNotification(
      "Validating API key...", 
      type = "default",  # Changed from "message" to "default"
      duration = NULL,
      closeButton = FALSE
    )
    
    # Validate the API key using a simple test request
    tryCatch({
      response <- httr::POST(
        url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateContent",
        httr::add_headers(
          "Content-Type" = "application/json"
        ),
        query = list(
          key = input$apiKey
        ),
        body = jsonlite::toJSON(list(
          contents = list(
            list(
              parts = list(
                list(text = "Test")
              )
            )
          )
        ), auto_unbox = TRUE)
      )
      
      # Check response
      if (httr::status_code(response) == 200) {
        removeNotification(id)
        validationStatus("success")
        updateActionButton(session, "validateKey",
                           label = "",
                           icon = icon("check"))
        runjs("$('#validateKey').addClass('validate-btn-success');")
        showNotification(
          "API key is valid!", 
          type = "default",  # Changed from "success" to "default"
          duration = 5
        )
      } else {
        removeNotification(id)
        validationStatus("failed")
        updateActionButton(session, "validateKey",
                           label = "Validate Key",
                           icon = icon("check"))
        runjs("$('#validateKey').removeClass('validate-btn-success');")
        showNotification(
          "Invalid API key. Please check and try again.", 
          type = "error",
          duration = 5
        )
      }
    }, error = function(e) {
      removeNotification(id)
      validationStatus("failed")
      updateActionButton(session, "validateKey",
                         label = "Validate Key",
                         icon = icon("check"))
      runjs("$('#validateKey').removeClass('validate-btn-success');")
      
      # More detailed error handling
      error_message <- if(grepl("Could not resolve host", e$message)) {
        "Network error: Please check your internet connection"
      } else if(grepl("timed out", e$message)) {
        "Request timed out: Please try again"
      } else {
        "Error validating API key. Please try again"
      }
      
      showNotification(
        error_message,
        type = "error",
        duration = 5
      )
    })
  })
  
  # Add this observer to reset button state when API key changes
  observeEvent(input$apiKey, {
    if(validationStatus() != "pending") {
      validationStatus("pending")
      updateActionButton(session, "validateKey",
                         label = "Validate Key",
                         icon = icon("check"))
      runjs("$('#validateKey').removeClass('validate-btn-success');")
    }
  })
  
  
  observeEvent(input$modelSelect, {
    # Update rate limits when model changes
    if(input$modelSelect == "gemini-1.5-flash") {
      updateNumericInput(session, "requestsPerMinute", value = 15)
      updateNumericInput(session, "requestsPerDay", value = 1500)
    } else {
      updateNumericInput(session, "requestsPerMinute", value = 2)
      updateNumericInput(session, "requestsPerDay", value = 50)
    }
  })
  
  # Save settings
  observeEvent(input$saveSettings, {
    # Read the existing config while preserving comments and structure
    current_config <- yaml::read_yaml("config.yml")
    
    # Only update the specific values we want to change
    current_config$api$gemini$api_key <- input$apiKey
    current_config$api$gemini$model <- input$modelSelect  # if you have model selection
    current_config$api$gemini$rate_limits$requests_per_minute <- input$requestsPerMinute
    current_config$api$gemini$rate_limits$requests_per_day <- input$requestsPerDay
    
    # Write back to file, preserving the structure
    writeLines(
      paste0(
        "api:\n",
        "  gemini:\n",
        "    base_url: \"", "https://generativelanguage.googleapis.com/v1beta", "\"\n",
        "    model: \"", current_config$api$gemini$model, "\"\n",
        "    api_key: \"", current_config$api$gemini$api_key, "\"\n",
        "    rate_limits:\n",
        "      requests_per_minute: ", current_config$api$gemini$rate_limits$requests_per_minute, "\n", 
        "      requests_per_day: ", current_config$api$gemini$rate_limits$requests_per_day,
        "\n\n"
      ),
      "config.yml"
    )
    
    showNotification("API Settings saved successfully", type = "message")
  })
  
  # Handle file upload and reading
  observeEvent(input$codingFormFile, {
    req(input$codingFormFile)
    
    # First, read the uploaded file
    tryCatch({
      # Read the file
      if (tools::file_ext(input$codingFormFile$name) == "csv") {
        rv$codingForm <- read.csv(input$codingFormFile$datapath)
      } else {
        rv$codingForm <- read_excel(input$codingFormFile$datapath)
      }
      
      # If file read successfully, proceed with save location
      showNotification("File uploaded successfully!", type = "message")
      
      # Show modal with custom button for file selection
      showModal(modalDialog(
        title = "Select Save Location",
        "Please select where to save the responses. It's recommended to select the same file you just uploaded.",
        footer = actionButton("proceedToFileSelect", "OK")
      ))
      
    }, error = function(e) {
      # Handle file reading errors
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  # Separate observer for the OK button click
  observeEvent(input$proceedToFileSelect, {
    removeModal()  # Remove the modal first
    
    # Wrap file selection in tryCatch
    tryCatch({
      selected_path <- file.choose()
      
      if (!is.null(selected_path)) {
        full_path <- normalizePath(selected_path, winslash = "/")
        
        # Verify file
        if (!file.exists(full_path)) {
          showNotification("Selected file does not exist", type = "error")
          return()
        }
        
        if (file.access(full_path, mode = 2) != 0) {
          showNotification("No write permission for selected file", type = "error")
          return()
        }
        
        if (!tolower(tools::file_ext(full_path)) %in% c("xlsx", "xls", "csv")) {
          showNotification("Please select an Excel or CSV file", type = "error")
          return()
        }
        
        # Store the path and show success
        original_file_path(full_path)
        showNotification("Save location selected successfully", type = "message")
      }
    }, error = function(e) {
      # Handle file selection errors separately
      showNotification("File selection cancelled", type = "warning")
    })
  })
  
  output$fileStatus <- renderUI({
    if (is.null(rv$codingForm)) {
      div(
        style = "color: #f00; position: absolute; top: 10px; left: 50%; transform: translateX(-50%);",
        "Please upload a coding form on the Start Here page before proceeding."
      )
    } else {
      div(
      )
    }
  })
  
  # Initialize current_row when the session starts
  observe({
    req(original_file_path())
    if (is.null(current_row())) {
      tryCatch({
        coding_form_df <- read_excel(original_file_path(), sheet = 1)
        current_row(nrow(coding_form_df) + 1)
      }, error = function(e) {
        current_row(1)
      })
    }
  })
  
  # Reset current_row when a new PDF is uploaded
  observeEvent(input$pdfFile, {
    req(input$pdfFile)
    req(original_file_path())
    
    tryCatch({
      coding_form_df <- read_excel(original_file_path(), sheet = 1)
      current_row(nrow(coding_form_df) + 1)
    }, error = function(e) {
      current_row(1)
    })
  })
  
  # Clear responses and source information when new PDF is uploaded
  observeEvent(input$pdfFile, {
    req(input$pdfFile)
    
    # Clear all response text inputs
    for (i in seq_along(rv$prompts)) {
      updateTextInput(session,
                      inputId = paste0("response_", i),
                      value = "")
    }
    
    # Clear source information using the same JavaScript structure as the source display code
    runjs("
  // Remove all source info boxes and their wrapper divs
  const allInfoBoxes = document.querySelectorAll('[id^=source_info_]');
  allInfoBoxes.forEach(box => {
    if (box.parentElement) {
      box.parentElement.remove();  // Remove the wrapper div
    }
  });
")
    
    # Reset the sources reactive value
    rv$sources <- list()
    
  }, priority = 1)
  
  
  ######################################################
  ##############Gemini calls############################
  
  analyze_with_gemini <- function(pdf_text, prompts, config, progress_callback = NULL) {
    if (is.null(pdf_text) || length(pdf_text) == 0) {
      stop("PDF text is empty or null")
    }
    if (is.null(prompts) || length(prompts) == 0) {
      stop("Prompts are empty or null")
    }
    
    print("Starting Gemini analysis")
    print(paste("Number of prompts:", length(prompts)))
    print(paste("PDF text length:", length(pdf_text)))
    
    base_url <- config$api$gemini$base_url
    model <- config$api$gemini$model
    api_key <- config$api$gemini$api_key
    
    if (is.null(base_url) || is.null(model) || is.null(api_key)) {
      stop("Missing required configuration values")
    }
    
    requests_per_minute <- tryCatch({
      rpm <- as.numeric(config$rate_limits$requests_per_minute)
      if (is.na(rpm) || rpm <= 0) 15 else rpm
    }, error = function(e) {
      print("Using default rate limit of 15 requests per minute")
      15
    })
    
    sleep_time <- 60/requests_per_minute
    url <- sprintf("%s/models/%s:generateContent", base_url, model)
    
    process_prompt <- function(prompt, sleep_time, url, api_key, pdf_text) {
      Sys.sleep(sleep_time)
      print(sprintf("Processing prompt: %s", prompt))
      
      payload <- list(
        contents = list(
          list(
            parts = list(
              list(
                text = sprintf("Analyze this PDF and answer the following prompt. You MUST provide both an answer AND a source for every response.

              IMPORTANT: For each response, you MUST use this EXACT format:
              ANSWER: [your answer here]
              SOURCE: [copy and paste the EXACT supporting text from the PDF]
              PAGE: [specify which page number this text appears on]

              If you cannot find a specific source in the PDF, respond with:
              ANSWER: [your answer here]
              SOURCE: [explain why you answered the way you did. Start with the phrase, I drew this conclusion because]
              PAGE: N/A

              PDF text:
              %s

              Prompt: %s",
                               paste(pdf_text, collapse = "\n"),
                               prompt)
              )
            )
          )
        ),
        generationConfig = list(
          temperature = 0.1,
          topK = 1,
          topP = 1,
          maxOutputTokens = 2048
        )
      )
      
      tryCatch({
        response <- httr::POST(
          url = paste0(url, "?key=", api_key),
          body = jsonlite::toJSON(payload, auto_unbox = TRUE),
          httr::add_headers("Content-Type" = "application/json"),
          encode = "json"
        )
        
        content <- httr::content(response, "text")
        parsed <- jsonlite::fromJSON(content)
        
        if (!is.null(parsed$candidates) && 
            length(parsed$candidates) > 0 && 
            !is.null(parsed$candidates$content$parts[[1]]$text)) {
          
          response_text <- parsed$candidates$content$parts[[1]]$text
          print("Raw response from Gemini:")
          print(response_text)
          
          # Extract answer, source, and page using regex
          answer_match <- regexpr("ANSWER:\\s*(.*?)(?=\\s*SOURCE:|$)", response_text, perl = TRUE)
          source_match <- regexpr("SOURCE:\\s*(.*?)(?=\\s*PAGE:|$)", response_text, perl = TRUE)
          page_match <- regexpr("PAGE:\\s*(.*?)$", response_text, perl = TRUE)
          
          answer <- if(answer_match > 0) {
            trimws(substr(response_text, 
                          answer_match + attr(answer_match, "match.length") - attr(answer_match, "capture.length"),
                          answer_match + attr(answer_match, "match.length")))
          } else {
            response_text  # fallback to full response if parsing fails
          }
          
          source <- if(source_match > 0) {
            trimws(substr(response_text,
                          source_match + attr(source_match, "match.length") - attr(source_match, "capture.length"),
                          source_match + attr(source_match, "match.length")))
          } else {
            NULL
          }
          
          page <- if(page_match > 0) {
            trimws(substr(response_text,
                          page_match + attr(page_match, "match.length") - attr(page_match, "capture.length"),
                          page_match + attr(page_match, "match.length")))
          } else {
            "N/A"
          }
          
          print("Extracted answer:")
          print(answer)
          print("Extracted source:")
          print(source)
          print("Extracted page:")
          print(page)
          
          return(list(
            answer = answer,
            source = source,
            page = page
          ))
        } else {
          return(list(
            answer = "Error: Unable to extract response",
            source = NULL,
            page = NULL
          ))
        }
        
      }, error = function(e) {
        print(sprintf("API call error: %s", e$message))
        return(list(
          answer = sprintf("Error: %s", e$message),
          source = NULL,
          page = NULL
        ))
      })
    }
    
    responses <- lapply(seq_along(prompts), function(i) {
      if (!is.null(progress_callback)) {
        progress_callback(i, length(prompts))
      }
      process_prompt(prompts[i], sleep_time, url, api_key, pdf_text)
    })
    
    # Debug responses
    print("DEBUG: Full responses:")
    for (i in seq_along(responses)) {
      print(paste("Response", i, "---------------"))
      print("Answer:")
      print(responses[[i]]$answer)
      print("Source:")
      print(responses[[i]]$source)
      print("Page:")
      print(responses[[i]]$page)
      print("----------------------------------------")
    }
    
    return(responses)
  }
  
  
  
  
  #analyze button
  observeEvent(input$analyzeBtn, {
    # Debug prints to check values
    print("Analyze button pressed")
    print("PDF text status:")
    print(str(rv$pdf_text))
    print("Prompts status:")
    print(str(rv$prompts))
    
    # Reset previous results with empty lists
    rv$sources <- list()
    rv$pages <- list()
    
    # Explicit validation with safer checks
    if (is.null(rv$pdf_text)) {
      showNotification("PDF text is NULL. Please upload a PDF first.", type = "warning")
      return()
    }
    
    if (length(rv$pdf_text) == 0) {
      showNotification("PDF text is empty. Please upload a valid PDF.", type = "warning")
      return()
    }
    
    if (is.null(rv$prompts)) {
      showNotification("Prompts are NULL. Please upload a coding form first.", type = "warning")
      return()
    }
    
    if (length(rv$prompts) == 0) {
      showNotification("No prompts found. Please upload a valid coding form.", type = "warning")
      return()
    }
    
    # Create and show a modal dialog with progress
    showModal(modalDialog(
      title = NULL,
      footer = NULL,
      size = "s",
      easyClose = FALSE,
      div(
        style = "text-align: center; padding: 20px;",
        h4("Analyzing PDF..."),
        div(
          style = "width: 100%; padding: 10px;",
          div(class = "progress",
              div(
                class = "progress-bar",
                role = "progressbar",
                style = "width: 0%"
              )
          ),
          div(id = "progress-detail", "Preparing..."),
          div(
            id = "time-estimate",
            style = "margin-top: 10px; font-size: 0.9em; color: #666;",
            "Please be patient..."
          )
        )
      )
    ))
    
    # Wrap the entire analysis process in tryCatch
    tryCatch({
      print("Starting analysis...")
      
      total_prompts <- length(rv$prompts)
      
      responses <- analyze_with_gemini(
        pdf_text = rv$pdf_text,
        prompts = rv$prompts,
        config = config,
        progress_callback = function(current, total) {
          # Update progress bar
          progress_pct <- (current/total) * 100
          runjs(sprintf("
          $('.progress-bar').css('width', '%s%%');
          $('#progress-detail').text('Processing prompt %d of %d');
        ", progress_pct, current, total))
        }
      )
      
      # Debug print responses
      print("Received responses:")
      print(str(responses))
      print("DEBUG: All responses:")
      for (i in seq_along(responses)) {
        print(paste("Response", i, "---------------"))
        print("Answer:")
        print(responses[[i]]$answer)
        print("Source:")
        print(responses[[i]]$source)
        print("Page:")
        print(responses[[i]]$page)
        print("----------------------------------------")
      }
      
      # Update text inputs and store sources/pages
      if (!is.null(responses) && length(responses) > 0) {
        for (i in seq_along(responses)) {
          if (!is.null(responses[[i]]) && !is.null(responses[[i]]$answer)) {
            print(sprintf("Updating response_%d with:", i))
            print(responses[[i]]$answer)
            
            # Update text input with answer only
            updateTextAreaInput(
              session,
              inputId = paste0("response_", i),
              value = responses[[i]]$answer
            )
            
            # Store source and page information
            if (!is.null(responses[[i]]$source) && !is.null(responses[[i]]$page)) {
              rv$sources[[i]] <- sprintf("SOURCE: %s\nPAGE: %s", 
                                         responses[[i]]$source, 
                                         responses[[i]]$page)
              rv$pages[[i]] <- responses[[i]]$page
            } else {
              rv$sources[[i]] <- "No source information available"
              rv$pages[[i]] <- "N/A"
            }
            
            print(paste("Stored source for prompt", i, ":", rv$sources[[i]]))
            print(paste("Stored page for prompt", i, ":", rv$pages[[i]]))
          } else {
            print(sprintf("Skipping invalid response for prompt %d", i))
            rv$sources[[i]] <- "Invalid response"
            rv$pages[[i]] <- "N/A"
          }
        }
        
        # Debug prints for final status
        print("Debug: Final sources status:")
        print(paste("Number of sources:", length(rv$sources)))
        for (i in seq_along(rv$sources)) {
          print(paste("Source", i, ":", substr(rv$sources[[i]], 1, 100), "..."))
          print(paste("Page", i, ":", rv$pages[[i]]))
        }
        
        showNotification("Analysis complete!", type = "message")
      } else {
        showNotification("No valid responses received", type = "warning")
      }
      
    }, error = function(e) {
      print(paste("Error in analyze button observer:", e$message))
      showNotification("Error during analysis. Please check the console for details.", 
                       type = "warning")
    }, finally = {
      removeModal()  # Remove the modal when done
    })
  })
  
  ######################################################
  ######################################################
  # Load prompts from Excel file for coding form
  observe({
    req(input$codingFormFile)
    tryCatch({
      # Read Excel file
      coding_form_df <- read_excel(input$codingFormFile$datapath, col_names = TRUE, sheet = 1, .name_repair = "unique")
      # Adjust sheet index if necessary
      print(str(coding_form_df))
      # Debugging output to check the loaded Excel file
      print("Loaded Excel Data:")
      print(coding_form_df)  # Print the entire data frame to inspect
      
      # Force column extraction
      prompts <- as.character(names(coding_form_df))
      
      print("Prompts found:")
      print(prompts)
      
      if (length(prompts) > 0 && !all(is.na(prompts))) {
        rv$prompts <- prompts
      } else {
        showNotification("No valid prompts found.", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Excel reading error:", e$message), type = "error")
    })
  })
  
  # Modify the renderUI for coding prompts
  output$codingPrompts <- renderUI({
    req(rv$prompts)
    
    # Generate UI elements for each prompt
    prompts_ui <- lapply(seq_along(rv$prompts), function(i) {
      prompt_text <- rv$prompts[i]
      
      div(
        class = "coding-prompt",
        h4(paste("Prompt", i, ":", prompt_text)),
        
        # Text entry box for the response
        textAreaInput(
          inputId = paste0("response_", i),
          label = NULL,
          value = "",
          width = "100%"
        ),
        # Buttons for Source and Record
        div(
          class = "coding-buttons",
          actionButton(inputId = paste0("source_", i), label = "Source", class = "btn-info"),
          actionButton(inputId = paste0("record_", i), label = "Record", class = "btn-success")
        ),
        hr()
      )
    })
    
    do.call(tagList, prompts_ui)
  })
  
  
  # Dynamic observers for source buttons
  observe({
    for (i in seq_along(rv$prompts)) {
      local({
        local_i <- i
        
        observeEvent(input[[paste0("source_", local_i)]], {
          print(paste("Source button clicked for prompt", local_i))
          
          # Validate source exists
          if (is.null(rv$sources) || local_i > length(rv$sources)) {
            showNotification("Source information not available for this item", type = "error")
            return()
          }
          
          # Extract source text and page number
          source_info <- rv$sources[[local_i]]
          source_text <- gsub(".*SOURCE:\\s*(.+?)\\s*PAGE:.*", "\\1", source_info)
          page_num <- as.numeric(gsub(".*PAGE:\\s*(\\d+).*", "\\1", source_info))
          
          # First scroll to the correct page
          runjs(sprintf("
        const container = document.querySelector('.pdf-container');
        if (container) {
          const pages = container.getElementsByTagName('img');
          const pageIndex = %d;
          const targetPage = pages[pageIndex];
          if (targetPage) {
            targetPage.scrollIntoView({ behavior: 'smooth', block: 'start' });
          }
        }

        // Close all other source info boxes first
        const allInfoBoxes = document.querySelectorAll('[id^=source_info_]');
        allInfoBoxes.forEach(box => {
          if (box.id !== 'source_info_%d') {
            box.parentElement.remove();
          }
        });

        // Toggle source info box
        const sourceBtn = document.getElementById('source_%d');
        let infoBox = document.getElementById('source_info_%d');
        const buttonContainer = sourceBtn.parentElement;

        if (!infoBox) {
          // Create a wrapper div for the source info box
          const wrapper = document.createElement('div');
          wrapper.style.cssText = `
            width: 100%%;
            position: relative;
            order: 1;
          `;

          // Create info box if it doesn't exist
          infoBox = document.createElement('div');
          infoBox.id = 'source_info_%d';
          infoBox.style.cssText = `
            margin: 5px 0;
            padding: 10px;
            background-color: #f8f8f8;  /* Light gray background */
            border: 1px solid #e8e8e8;  /* Matching border color */
            border-radius: 4px;
            font-size: 14px;
            line-height: 1.4;
            width: 100%%;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            box-sizing: border-box;
          `;
          infoBox.innerHTML = `
            <strong>Source:</strong><br>
            %s<br><br>
            <strong>Page:</strong> %d
          `;

          wrapper.appendChild(infoBox);

          // Make sure the button container is flex
          buttonContainer.style.cssText = `
            display: flex;
            flex-wrap: wrap;
            align-items: center;
            gap: 10px;
          `;

          // Insert the wrapper after the source button but before other elements
          sourceBtn.insertAdjacentElement('afterend', wrapper);
        } else {
          // Remove info box if it exists
          infoBox.parentElement.remove();
        }
      ", page_num - 1, local_i, local_i, local_i, local_i, source_text, page_num))
          
        })
      })
    }
  })
  
 
  
  # Record Buttons
  observe({
    for (i in seq_along(rv$prompts)) {
      local({
        local_i <- i
        
        observeEvent(input[[paste0("record_", local_i)]], {
          current_path <- original_file_path()
          req(current_path)
          req(current_row())  # Make sure we have a current row
          
          tryCatch({
            response <- input[[paste0("response_", local_i)]]
            req(response)
            
            # Print debugging information
            print(paste("Current row:", current_row()))
            print(paste("Writing response:", response))
            
            # Read the Excel file
            coding_form_df <- read_excel(current_path, sheet = 1)
            
            # Make sure we have enough rows
            if (current_row() > nrow(coding_form_df)) {
              # Add new rows if needed
              new_rows <- data.frame(matrix(NA, 
                                            nrow = current_row() - nrow(coding_form_df), 
                                            ncol = ncol(coding_form_df)))
              names(new_rows) <- names(coding_form_df)
              coding_form_df <- rbind(coding_form_df, new_rows)
            }
            
            # Write the response to the current row
            coding_form_df[current_row(), local_i] <- response
            
            # Save directly to the original file
            write_xlsx(coding_form_df, current_path)
            
            showNotification(paste("Response for Prompt", local_i, "recorded and saved successfully in row", current_row()), 
                             type = "message")
            
          }, error = function(e) {
            showNotification(paste("Error recording response:", e$message), type = "error")
          })
        })
      })
    }
  })
  
  
  # Handle PDF upload
  observeEvent(input$pdfFile, {
    req(input$pdfFile)
    
    # Create a reactiveVal for cancel status
    rv$cancel_process <- FALSE
    
    # Show modal with cancel button
    showModal(modalDialog(
      title = NULL,
      footer = tagList(
        actionButton("cancelPdfProcess", "Cancel", 
                     class = "btn-danger",
                     style = "float: right;")
      ),
      size = "s",
      easyClose = FALSE,
      div(
        style = "text-align: center; padding: 20px;",
        h4("Processing PDF..."),
        div(
          style = "width: 100%; padding: 10px;",
          div(class = "progress",
              div(
                class = "progress-bar",
                role = "progressbar",
                style = "width: 0%"
              )
          ),
          div(id = "pdf-progress-detail", "Initializing..."),
          div(id = "time-remaining", style = "margin-top: 5px; color: #666; font-size: 12px;")
        )
      )
    ))
    
    # Cancel button observer
    observeEvent(input$cancelPdfProcess, {
      rv$cancel_process <- TRUE
    })
    
    # Clear existing images
    if (!is.null(rv$temp_files)) {
      runjs("
          $('.progress-bar').css('width', '10%');
          $('#pdf-progress-detail').text('Cleaning up previous files...');
      ")
      unlink(rv$temp_files)
      rv$temp_files <- NULL
    }
    
    tryCatch({
      pdf_file_path <- input$pdfFile$datapath
      total_pages <- pdf_info(pdf_file_path)$pages
      
      # Create temporary directory
      temp_dir <- file.path(tempdir(), paste0("pdf_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
      
      # Update resource path
      removeResourcePath("temp")
      addResourcePath("temp", temp_dir)
      
      # Initialize timing variables
      start_time <- Sys.time()
      processed_pages <- 0
      
      # Custom progress callback with time estimation
      progress_callback <- function(page) {
        if (rv$cancel_process) {
          stop("Process cancelled by user")
        }
        
        processed_pages <<- page
        current_time <- Sys.time()
        elapsed_time <- as.numeric(difftime(current_time, start_time, units = "secs"))
        
        # Calculate average time per page
        avg_time_per_page <- elapsed_time / page
        
        # Estimate remaining time
        pages_remaining <- total_pages - page
        estimated_remaining_secs <- avg_time_per_page * pages_remaining
        
        # Format remaining time
        remaining_text <- if(estimated_remaining_secs > 60) {
          sprintf("%.1f minutes remaining", estimated_remaining_secs/60)
        } else {
          sprintf("%.0f seconds remaining", estimated_remaining_secs)
        }
        
        progress_pct <- (page / total_pages) * 100
        runjs(sprintf("
              $('.progress-bar').css('width', '%s%%');
              $('#pdf-progress-detail').text('Converting page %d of %d');
              $('#time-remaining').text('%s');
          ", progress_pct, page, total_pages, remaining_text))
      }
      
      # Convert PDF to images with progress updates
      image_files <- vector("character", total_pages)
      for(page in 1:total_pages) {
        if (rv$cancel_process) {
          stop("Process cancelled by user")
        }
        
        progress_callback(page)
        
        # Convert single page
        current_file <- file.path(temp_dir, sprintf("page_%d.png", page))
        pdf_convert(pdf_file_path, 
                    format = "png", 
                    pages = page, 
                    dpi = 150, 
                    filenames = current_file)
        
        image_files[page] <- current_file
        
        # Verify file exists
        if (!file.exists(current_file)) {
          stop(paste("Failed to create image file for page", page))
        }
      }
      
      # Final processing
      runjs("
          $('.progress-bar').css('width', '90%');
          $('#pdf-progress-detail').text('Extracting text and preparing viewer...');
          $('#time-remaining').text('Almost done...');
      ")
      
      # Store the files in reactive values
      rv$temp_files <- image_files
      rv$pdf_images <- image_files
      rv$pdf_text <- pdf_text(pdf_file_path)
      rv$total_pages <- total_pages
      rv$current_page <- 1
      
      # Force refresh
      rv$image_timestamp <- format(Sys.time(), "%Y%m%d%H%M%OS6")
      
      # Update slider
      updateSliderInput(session, "pageSlider", min = 1, max = rv$total_pages, value = 1)
      
      # Verify files are accessible
      lapply(rv$pdf_images, function(f) {
        if (!file.exists(f)) {
          warning(paste("File not found:", f))
        }
      })
      
      runjs("
          $('.progress-bar').css('width', '100%');
          $('#pdf-progress-detail').text('Complete!');
          $('#time-remaining').text('');
      ")
      
      showNotification("PDF processed successfully!", type = "message")
      
    }, error = function(e) {
      print(paste("Error:", e$message))  # Debug print
      if (rv$cancel_process) {
        # Clean up temporary files if process was cancelled
        if (!is.null(rv$temp_files)) {
          unlink(rv$temp_files)
          rv$temp_files <- NULL
        }
        showNotification("PDF processing cancelled", type = "warning")
      } else {
        showNotification(paste("Error processing PDF:", e$message), type = "error")
      }
    }, finally = {
      # Reset cancel flag
      rv$cancel_process <- FALSE
      # Small delay before removing modal
      Sys.sleep(0.5)
      removeModal()
    })
  })
  
  # Display all PDF pages as images
  output$pdfImage <- renderUI({
    req(rv$pdf_images)
    req(rv$image_timestamp)
    
    # Debug print
    print("Rendering PDF images:")
    print(rv$pdf_images)
    
    image_tags <- lapply(rv$pdf_images, function(image_path) {
      if (!file.exists(image_path)) {
        warning(paste("Image file not found:", image_path))
        return(NULL)
      }
      
      # Add timestamp to URL to prevent caching
      image_url <- paste0("/temp/", basename(image_path), "?t=", rv$image_timestamp)
      tags$img(src = image_url, 
               class = "pdf-image", 
               style = "width: 100%; margin-bottom: 10px;")
    })
    
    # Filter out NULL entries
    image_tags <- Filter(Negate(is.null), image_tags)
    
    if (length(image_tags) == 0) {
      return(div("No images to display"))
    }
    
    do.call(tagList, image_tags)
  })
}
# Run the Shiny app
shinyApp(ui = ui, server = server)
