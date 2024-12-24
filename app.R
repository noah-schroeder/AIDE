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
library(base64enc)

source("config_helpers.R")

# Load configuration from the parent directory
config <- yaml::read_yaml(file = "config.yml")  # Adjusted path to config.yml

# UI Definition ----
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
      menuItem("Setup", tabName = "LLM"),
      menuItem("Analyze", tabName = "Analyze"),
      menuItem("Cite", tabName = "Cite")
    )
  ),
  
  ## CSS ----
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
      
      #download-status {  
        text-align: center;  
        font-weight: bold;  
        margin-top: 10px;  
        color: #333;  
      } 
      #layer-status {  
      max-height: 150px;  
      overflow-y: auto;  
      padding: 10px;  
      background-color: #f8f9fa;  
      border: 1px solid #dee2e6;  
      border-radius: 4px;  
      margin-top: 10px;  
    }  
    #layer-status div {  
      margin-bottom: 5px;  
      font-family: monospace;  
    }  
    .content-wrapper {  
      background-color: #f8f9fa !important; /* Lighter background color */  
      padding: 15px; /* Optional: Add padding for better spacing */  
    }  

    body {  
      background-color: #f8f9fa !important;  
    }  
    
    .wrapper {  
      background-color: #f8f9fa !important;  
    }
     "))
    ),
    
    # Loading screen
    use_waiter(),
    
    tabItems(
      ##Main Page ----
      tabItem("Start",
              
              
              
              # UI component
              box(
                width = 12,
                title = "Welcome to AI-Assisted Data Extraction (AIDE)",
                div(
                  style = "display: flex; flex-direction: column; gap: 8px;", # Reduced from 15px to 8px
                  p("AIDE was developed to greatly accelerate the data extraction process for systematic review and meta-analysis. It relies on you having an API key for Google Gemini, Mistral, or Open Router (which all offer free tiers as of December 2024).",
                  ),
                ),
              ),
              box(
                width = 6,
                title = "How to Use This App",
                div(
                  style = "display: flex; flex-direction: column; gap: 8px;", # Reduc
                  p("1) You will need an API key for the LLM provider of your choice (Google Gemini, Mistral, or Open Router). At the time this app was created (December, 2024), all three of the following providers offered free API tiers:", 
                    HTML("API Key Providers:  
                      <ul>  
                        <li><a href='https://aistudio.google.com/' target='_blank' style='display: inline;'>Google AI Studio</a></li>  
                        <li><a href='https://mistral.ai/' target='_blank' style='display: inline;'>Mistral AI</a></li>  
                        <li><a href='https://openrouter.ai/' target='_blank' style='display: inline;'>Open Router</a></li>  
                      </ul>")
                    ),
                  p("2) Set up your coding form. Importantly, your first row will be read as prompts by the large language model. The accuracy of the LLM's responses will be influenced very strongly by your prompts. Better prompts = better results."),
                  p("3) Move to the LLM Setup tab of this app where you can continue with the following steps:"),
                  p("4) Upload your coding form. Your coding form should be in Excel format for best results."),
                  p("4) Move to the analyze page and upload the PDF file you want to analyze."),
                  p("5) Click Analyze button. Your results will autofill under the appropriate prompt."),
                  p("6) Review each response. You can view the source information by clicking the Source button. You can record each result by clicking the record button. This will save it to your coding form on your local machine."),
                  p("7) When ready to work on the next PDF, simply upload a new one and the coding form will reset."),
                ),
              ),
              box(
                width = 6,
                title = "Common Questions",
                div(
                  style = "display: flex; flex-direction: column; gap: 8px;", # Reduc
                  strong("What LLM should I use?"),
                  p("We have had excellent results with Google Gemini models. Mistral large also performs very well in our testing, however it has not always responded in a way where we can always parse the source. Open Router tends to offer smaller models, and they have not performed as well in our informal testing. For this reason, we suggest using either Gemini or Mistral models as of December 2024."),
                  strong("What source text is sent to the LLM?"),
                  p("This app uses the pdftools package for R to extract structured text from the PDF. This means images are not sent to the LLM, and some formatting in the PDF may be lost. This is a necessary trade off because at this point (December 2024) you cannot send PDF files directly to many LLMs, many require text data."),
                  strong("What system and user prompts do you use?"),
                  HTML("Prompts used:  
                      <ul>  
                        <li>Google Gemini</li> 
                        <ul>
                          <li>System prompt: None</li>
                          <li>User prompt: Analyze this PDF and answer ALL of the following prompts. For EACH prompt, you MUST provide... [continues into answer, source, page labels]</li>
                        </ul>
                        <li>Mistral</li> 
                        <ul>
                          <li>System prompt: You are an expert PDF analyzer. You will process multiple prompts about this document.</li>
                          <li>User prompt: Analyze this PDF text and answer ALL of the following prompts... [continues into answer, source, page labels]  </li>
                        </ul>
                        <li>Open Router</li> 
                        <ul>
                          <li>System prompt: You are an expert PDF analyzer. You will process multiple prompts about this document.</li>
                          <li>User prompt: Analyze this PDF text and answer ALL of the following prompts... [continues into answer, source, page labels]  </li>
                        </ul>
                      </ul>"),
                  strong("Is each prompt an API call? How do I know how many times I'm sending a request to the API?"),
                  p("Each time you press \"Analyze\" on the analysis page it makes one API request. All prompts and the full text are built into one API request. "),
                  strong("Can I see the direct responses from the API?"),
                  p("A lot of information prints in the R console for debugging purposes."),
                  ),
              ),
      ),
      #LLM Config ----
         tabItem("LLM",     
              # New LLM Method Selection Box  
              box(  
                width = 12,  
                title = "LLM Method",  
                div(  
                  style = "display: flex; flex-direction: column; gap: 8px;",  
                  p("Choose the way you would like to interact with LLMs"),
                  selectInput("llmMethod", "",  
                              choices = c("Google Gemini API", "Mistral API", "OpenRouter API"),  
                              selected = "Google Gemini API",  
                              width = "100%"),  
                )
              ),  
              
              # Conditional UI boxes based on LLM Method selection  
              #Gemini selection ----
              conditionalPanel(    
                condition = "input.llmMethod == 'Google Gemini API'",    
                box(    
                  width = 12,    
                  title = "Google Gemini API Notes",    
                  div(    
                    style = "display: flex; flex-direction: column; gap: 8px;",    
                    p("1) The free tier of the Google Gemini API has rate limits. You can check current rate limits for the free tier here:", HTML('<a href="https://ai.google.dev/pricing" target="_blank" style="display: inline;">Google AI API Pricing.</a>'), "This app enforces a rate limit of 2 requests per minute, which is very conservative."),
                    p("2) There are a variety of different models available from Google, such as various versions of Gemini Flash and Gemini Pro. You can find the list of all models here:", HTML('<a href="https://ai.google.dev/gemini-api/docs/models/gemini" target="_blank" style="display: inline;">Google AI Models.</a>'), "Our experience during testing was that Gemini 1.5 Pro slightly outperformed Gemini 1.5 Flash, however 1.5 Flash was still very good!"),    
                    )
                )
              ),
              
              conditionalPanel(  
                condition = "input.llmMethod == 'Google Gemini API'",  
                #Gemini API settings
                box(
                  width = 6,
                  title = "Gemini API Settings",
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
                      p("Always check to ensure you don't exceed free tier rate limit usage:", HTML('<a href="https://ai.google.dev/pricing" target="_blank" style="display: inline;">Google AI API Pricing.</a>')),
                      ),
                    div(
                      style = "display: flex; gap: 10px; margin-top: -5px;", # Added negative top margin
                      numericInput("requestsPerMinute", "Requests per Minute",
                                   value = tryCatch({
                                     fresh_config <- yaml::read_yaml(file = "config.yml")
                                     fresh_config$api$gemini$requests_per_minute
                                   }, error = function(e) {
                                     if(input$modelSelect == "gemini-1.5-flash") 2 else 2
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
                 
                      p("You must click save settings before you can use this model."),
                      actionButton("saveSettings", "Save Settings", 
                                   icon = icon("save"),
                                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    
                  ),
                ),
              ),
              #Mistral selection ----
              conditionalPanel(    
                condition = "input.llmMethod == 'Mistral API'",    
                box(    
                  width = 12,    
                  title = "Mistral API Notes",    
                  div(    
                    style = "display: flex; flex-direction: column; gap: 8px;",    
                    p("1) Mistral has a very generous experimental/research API at the time this app was created (December, 2024). The full list of models is here:", HTML('<a href="https://docs.mistral.ai/getting-started/models/models_overview/" target="_blank" style="display: inline;">Mistral Models.</a>'), "We have only included models that have a 128K or larger context window."),
                    p("2) Since we are sending text data only, and at least as of December 2024 Mistral allowed those with free API key usage to use all models, we recommend using Mistral Large as we have had very good results with it."),    
                    )
                )
              ),
              conditionalPanel(    
                condition = "input.llmMethod == 'Mistral API'",    
                box(    
                  width = 6,    
                  title = "Mistral API Settings",    
                  div(    
                    style = "display: flex; flex-direction: column; gap: 8px;",    
                    # API Key input with validate button
                    div(
                      style = "display: flex; gap: 10px;",
                      textInput("apiKeyMistral", "API Key", 
                                value = tryCatch({
                                  config <- yaml::read_yaml(file = "config.yml")
                                  config$api$mistral$api_key
                                }, error = function(e) {
                                  cat("Error accessing config:", e$message, "\n")
                                  ""
                                }),
                                width = "400px"),
                      tags$div(
                        style = "margin-top: 25px;",  # Adjust this value to align perfectly
                        actionButton("validateKeyMistral", 
                                     label = "Validate Key",
                                     icon = icon("check"),
                                     class = "validate-btn")
                      )
                    ),
                    # Model selection
                    selectInput("modelSelectMistral", "Model",
                                choices = c("mistral-large-latest", "pixtral-large-latest", "pixtral-12b-2409", "ministral-3b-latest", "ministral-8b-latest"),
                                selected = tryCatch({
                                  fresh_config <- yaml::read_yaml(file = "config.yml")
                                  fresh_config$api$mistral$model
                                }, error = function(e) {
                                  "mistral-large-latest"
                                }),
                                width = "400px"),
          
                    # Save Settings Button    
                    uiOutput("containerStatus"),  
                    p("You must click save settings before you can use this model."),
                    actionButton("saveSettingsMistral", "Save Settings",     
                                 icon = icon("save"),    
                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")    
                  )    
                )    
              ),   
              
              # Open Router Selection ----
              conditionalPanel(    
                condition = "input.llmMethod == 'OpenRouter API'",    
                box(    
                  width = 12,    
                  title = "Open Router API Notes",    
                  div(    
                    style = "display: flex; flex-direction: column; gap: 8px;",    
                    p("1) Not all Open Router Models are free to use. You can check which models are currently free here (make sure pricing is set to 0 and the model name should end with (free):", 
                      HTML('<a href="https://openrouter.ai/models?max_price=0" target="_blank" style="display: inline;">Open Router Model Pricing.</a>')),
                    p("2) Open Router Models has limits on how much you can use free models per minute and per day:", 
                      HTML('<a href="https://openrouter.ai/docs/limits" target="_blank" style="display: inline;">Open Router Limits.</a>')),
                    p("3) You must copy-paste in the Open Router model name in the box below. We do not have a dropdown menu because they may change what models are available."),
                    p("3) Note that different models are made for different things and not all will perform well for data extraction. In fact, of the freely available models from Open Router that we tested during development (December, 2024), none worked as well as the models available through the Gemini or Mistral APIs."),
                    p("4) We have not tested any paid models. Use paid models at your own risk, there is no guarentee they will be compatible with this app."),
                    p("5) Open Router provides access to a large number of models by a number of different companies. Not all conform to the same API standards. Therefore, there is no guarantee that this app can utilize and parse the responses from any specific LLM. At the time of development (December, 2024), we tested the following models:"),
                    HTML("Generally Working Models (not guaranteed)  
                        <ul>  
                          <li>Mythomax-l2-13b</li>  
                          <li>Microsoft Phi 3 Mini 128k Instruct</li>
                          <li>Mistral 7B Instruct (we recommend using Mistral API instead though, which uses larger models and produces better results in our testing.)</li>
                        </ul>  
                      "),
                    HTML("Partially Working Models (source button likely won't work properly)  
                        <ul>  
                          <li>Most Meta Llama 3, 3.1, or 3.2 model</li>  
                          <li>Microsoft Phi 3 Medium 128k Instruct</li>
                          <li>Open Chat 7B</li>
                          <li>Toppy M 7b</li>
                          <li>Qwen 2 6b Instruct</li>
                          <li>Zephyr 7b Beta</li>
                        </ul>  
                      "), 
                    HTML("The following models will not work in this app:  
                        <ul>  
                          <li>Any Google Gemini Model (use the Gemini API in the app instead)</li>
                          <li>Meta Llama 3.2 90b Vision Instruct</li>
                          <li>Meta Llama 3.1 70b Instruct</li>
                        </ul>  
                      ")
                  )
                )
              ),
              
              conditionalPanel(    
                condition = "input.llmMethod == 'OpenRouter API'",    
                box(    
                  width = 6,    
                  title = "Open Router API Settings",    
                  div(    
                    style = "display: flex; flex-direction: column; gap: 8px;",    
                    # API Key input with validate button
                    div(
                      style = "display: flex; gap: 10px;",
                      textInput("apiKeyOpenRouter", "API Key", 
                                value = tryCatch({
                                  config <- yaml::read_yaml(file = "config.yml")
                                  config$api$openrouter$api_key
                                }, error = function(e) {
                                  cat("Error accessing config:", e$message, "\n")
                                  ""
                                }),
                                width = "400px"),
                      tags$div(
                        style = "margin-top: 25px;",  # Adjust this value to align perfectly
                        actionButton("validateKeyOpenRouter", 
                                     label = "Validate Key",
                                     icon = icon("check"),
                                     class = "validate-btn")
                      )
                    ),
                    # Model selection
                    textInput("modelSelectOpenRouter", "Model",
                                value = tryCatch({
                                  fresh_config <- yaml::read_yaml(file = "config.yml")
                                  fresh_config$api$openrouter$model
                                }, error = function(e) {
                                  "enter model name from Open Router"
                                }),
                                width = "400px"),
                    
                   
                    # Save Settings Button    
                    uiOutput("containerStatus"),  
                    p("You must click save settings before you can use this model."),
                    actionButton("saveSettingsOpenRouter", "Save Settings",     
                                 icon = icon("save"),    
                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")    
                  )    
                )    
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
      
      ## Analysis ----
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

# Server logic ----
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
  mistral_key_valid <- reactiveVal(FALSE)
  current_row <- reactiveVal(NULL)
  validationStatus <- reactiveVal("pending") # Can be "pending", "success", or "failed"
  # Reactive value to track Mistral validation status
  validationStatusMistral <- reactiveVal("pending")
  # Reactive value to track Mistral validation status
  validationStatusOpenRouter <- reactiveVal("pending")
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
  
  # Handle coding form upload and reading ----
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
        "Please select where to save the responses (the popup may be in your taskbar if it did not pop up on the screen). It's recommended to select the same file you just uploaded.",
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
        "Please upload a coding form on the Setup page before proceeding."
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
  
  ## Clear responses and source information when new PDF is uploaded ----
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
  
  ## Load prompts from Excel file for coding form ----
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
  
  #Gemini ----
  ### Google API key validation ----
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
  
  ### Gemini observer to reset button state when API key changes ----
  observeEvent(input$apiKey, {
    if(validationStatus() != "pending") {
      validationStatus("pending")
      updateActionButton(session, "validateKey",
                         label = "Validate Key",
                         icon = icon("check"))
      runjs("$('#validateKey').removeClass('validate-btn-success');")
    }
  })
  
  ### Gemini model selection observer----
  observeEvent(input$modelSelect, {
    # Update rate limits when model changes
    if(input$modelSelect == "gemini-1.5-flash") {
      updateNumericInput(session, "requestsPerMinute", value = 2)
      updateNumericInput(session, "requestsPerDay", value = 1500)
    } else {
      updateNumericInput(session, "requestsPerMinute", value = 2)
      updateNumericInput(session, "requestsPerDay", value = 50)
    }
  })
  
  ### Gemini Save settings ----
  observeEvent(input$saveSettings, {  
    # Read the existing config while preserving comments and structure  
    if(file.exists("config.yml")){  
      current_config <- yaml::read_yaml("config.yml")  
    } else {  
      # Default config if file doesn't exist  
      current_config <- list(  
        api = list(  
          gemini = list(  
            model = "gemini-1.5-flash",  
            api_key = "your_gemini_api_key",  
            rate_limits = list(requests_per_minute = 2, requests_per_day = 1500)  
          ),  
          mistral = list(  
            model = "mistral-large-latest",  
            api_key = "your_mistral_api_key",  
            rate_limits = list(requests_per_minute = 2)
          ),
          openrouter = list(  
            model = "enter your model",  
            api_key = "your_open_router_api_key",  
            rate_limits = list(requests_per_minute = 2)
          )  
        )  
      )  
    }  
    
    # Only update the specific values we want to change  
    current_config$api$gemini$api_key <- input$apiKey  
    current_config$api$gemini$model <- input$modelSelect  
    current_config$api$gemini$rate_limits$requests_per_minute <- input$requestsPerMinute  
    current_config$api$gemini$rate_limits$requests_per_day <- input$requestsPerDay  
    
    ### Gemini write config.yml ----  
    write_config(current_config)  
    
    showNotification("API Settings saved successfully", type = "message")  
  })
  
  #### Gemini calls ----
  analyze_with_gemini <- function(pdf_text, prompts, config, progress_callback = NULL) {
    # Basic validation
    if (is.null(pdf_text) || length(pdf_text) == 0) {
      stop("PDF text is empty or null")
    }
    if (is.null(prompts) || length(prompts) == 0) {
      stop("Prompts are empty or null")
    }
    
    print("Starting Gemini analysis")
    print(paste("Number of prompts:", length(prompts)))
    print(paste("PDF text length:", length(pdf_text)))
    
    # Configuration extraction
    base_url <- config$api$gemini$base_url
    model <- config$api$gemini$model
    api_key <- config$api$gemini$api_key
    
    if (is.null(base_url) || is.null(model) || is.null(api_key)) {
      stop("Missing required configuration values")
    }
    
    url <- sprintf("%s/models/%s:generateContent", base_url, model)
    
    # Ensure full PDF text is captured
    full_pdf_text <- paste(pdf_text, collapse = "\n")
    
    # Debug: Print total PDF text length
    total_pdf_text_length <- nchar(full_pdf_text)
    print(paste("Total PDF text length:", total_pdf_text_length, "characters"))
    
    # Print out the exact prompts for debugging
    print("Exact Prompts:")
    for (i in seq_along(prompts)) {
      print(paste(i, ":", prompts[i]))
    }
    
    # Construct payload
    payload <- list(
      contents = list(
        list(
          parts = list(
            list(
              text = sprintf("Analyze this PDF and answer ALL of the following prompts. For EACH prompt, you MUST provide:

PROMPT FORMAT:
### PROMPT: [original prompt]
**ANSWER:** [comprehensive answer]
**SOURCE:** [exact supporting text from PDF, if applicable]
**PAGE:** [page number where source appears]

If no direct source is found, explain your reasoning.

PDF TEXT:
%s

PROMPTS:
%s",
                             full_pdf_text,
                             paste(sapply(prompts, function(p) paste("- ", p)), collapse = "\n"))
            )
          )
        )
      ),
      generationConfig = list(
        temperature = 0.1,
        topK = 1,
        topP = 1,
        maxOutputTokens = 8192
      )
    )
    
    # Comprehensive API call with error handling
    result <- tryCatch({
      # Perform API request
      response <- httr::POST(
        url = paste0(url, "?key=", api_key),
        body = jsonlite::toJSON(payload, auto_unbox = TRUE),
        httr::add_headers("Content-Type" = "application/json"),
        encode = "json"
      )
      
      # Parse response content
      content <- httr::content(response, "text")
      parsed <- jsonlite::fromJSON(content)
      
      # Extract response text
      if (!is.null(parsed$candidates) && 
          length(parsed$candidates) > 0 && 
          !is.null(parsed$candidates$content$parts[[1]]$text)) {
        
        response_text <- parsed$candidates$content$parts[[1]]$text
        print("Response received successfully")
        print("Full response text:")
        print(response_text)
        
        # Process responses
        responses <- lapply(seq_along(prompts), function(i) {
          # Call progress callback if provided
          if (!is.null(progress_callback)) {
            progress_callback(i, length(prompts))
          }
          
          # Get current prompt
          prompt <- prompts[i]
          
          # Print debug information
          print(paste("Processing prompt", i, ":", prompt))
          
          # Split the response text into sections
          response_sections <- strsplit(response_text, "### PROMPT: ")[[1]]
          response_sections <- response_sections[-1] # Remove text before first prompt
          
          # Find matching section
          matching_section <- NULL
          for (section in response_sections) {
            if (startsWith(section, prompt)) {
              matching_section <- section
              break
            }
          }
          
          if (!is.null(matching_section)) {
            print("Matched prompt section:")
            print(matching_section)
            
            # Position-based extraction
            answer_start <- regexpr("\\*\\*ANSWER:\\*\\*", matching_section)
            source_start <- regexpr("\\*\\*SOURCE:\\*\\*", matching_section)
            page_start <- regexpr("\\*\\*PAGE:\\*\\*", matching_section)
            
            # Extract answer
            answer <- if (answer_start > 0) {
              end_pos <- if (source_start > 0) source_start else 
                if (page_start > 0) page_start else nchar(matching_section)
              trimws(substr(matching_section, 
                            answer_start + attr(regexpr("\\*\\*ANSWER:\\*\\*", matching_section), "match.length"),
                            end_pos - 1))
            } else "No answer found"
            
            # Extract source and page
            source <- if (source_start > 0) {
              end_pos <- if (page_start > 0) page_start else nchar(matching_section)
              source_text <- trimws(substr(matching_section, 
                                           source_start + attr(regexpr("\\*\\*SOURCE:\\*\\*", matching_section), "match.length"),
                                           end_pos - 1))
              # Ensure source is returned even if empty
              if (source_text == "") "No specific source provided" else source_text
            } else "No source information available"
            
            # Extract page
            page <- if (page_start > 0) {
              page_text <- trimws(substr(matching_section, 
                                         page_start + attr(regexpr("\\*\\*PAGE:\\*\\*", matching_section), "match.length"),
                                         nchar(matching_section)))
              # Extract the number from the source text if page is not explicitly provided
              if (page_text == "" || page_text == "N/A") {
                # Look for page numbers in the source text
                page_match <- regexpr("page\\s+\\d+", source, ignore.case = TRUE)
                if (page_match > 0) {
                  page_num <- gsub("page\\s+", "", tolower(substr(source, page_match, 
                                                                  page_match + attr(page_match, "match.length") - 1)))
                  page_num
                } else {
                  "N/A"
                }
              } else {
                page_text
              }
            } else {
              # Try to extract page number from source if no explicit page marker
              if (!is.null(source)) {
                page_match <- regexpr("page\\s+\\d+", source, ignore.case = TRUE)
                if (page_match > 0) {
                  page_num <- gsub("page\\s+", "", tolower(substr(source, page_match, 
                                                                  page_match + attr(page_match, "match.length") - 1)))
                  page_num
                } else {
                  "N/A"
                }
              } else {
                "N/A"
              }
            }
            
            # Return the result ensuring source is never NULL
            return(list(
              answer = answer,
              source = if(is.null(source)) "No source information available" else source,
              page = page
            ))
            
            return(list(
              answer = answer,
              source = source,
              page = page
            ))
            
          } else {
            print(paste("No section found for prompt:", prompt))
            print("Debugging details:")
            print("Available sections:")
            for (j in seq_along(response_sections)) {
              print(paste(j, ":", substr(response_sections[j], 1, 100)))
            }
            
            return(list(
              answer = "No response found for this prompt",
              source = NULL,
              page = NULL
            ))
          }
        })
        
        return(responses)
        
      } else {
        stop("No valid response from Gemini API")
      }
    }, error = function(e) {
      # Error handling
      print("Critical error in Gemini API call:")
      print(e$message)
      
      return(lapply(prompts, function(x) list(
        answer = paste("API Error:", e$message),
        source = NULL,
        page = NULL
      )))
    })
    
    return(result)
  }
  
  # Null coalescing operator
  `%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x
  
  
  
  #Analyze Button----
  observeEvent(input$analyzeBtn, {
    # Debug prints to check values
    print("Analyze button pressed")
    print("PDF text status:")
    print(str(rv$pdf_text))
    print("Prompts status:")
    print(str(rv$prompts))
    
    # Choose the appropriate analysis function based on selected LLM  
    analysis_function <- switch(input$llmMethod,  
                                "Google Gemini API" = analyze_with_gemini,  
                                "Mistral API" = analyze_with_mistral,
                                "OpenRouter API" = analyze_with_openrouter)
    
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
                style = "width: 50%"
              )
          ),
          div(id = "progress-detail", "Sending API Request. This usually takes less than 2 minutes. The progress bar above will not move until the response is received."),
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
      
      responses <- analysis_function(  
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
          
          # More robust source text extraction
          source_text <- if (grepl("SOURCE:", source_info)) {
            gsub(".*SOURCE:\\s*(.+?)\\s*(PAGE:|$)", "\\1", source_info)
          } else {
            source_info  # Use full text if no SOURCE: marker
          }
          
          # More robust page number extraction
          page_num <- if (grepl("PAGE:", source_info)) {
            page_text <- gsub(".*PAGE:\\s*([^\\s]+).*", "\\1", source_info)
            if (page_text == "N/A") {
              "N/A"
            } else {
              as.numeric(page_text)
            }
          } else {
            "N/A"
          }
          
          # Modified JavaScript to handle "N/A" pages
          scroll_js <- if (!identical(page_num, "N/A")) {
            sprintf("
              const container = document.querySelector('.pdf-container');
              if (container) {
                const pages = container.getElementsByTagName('img');
                const pageIndex = %d;
                const targetPage = pages[pageIndex];
                if (targetPage) {
                  targetPage.scrollIntoView({ behavior: 'smooth', block: 'start' });
                }
              }", page_num - 1)
          } else {
            "" # Empty string for no scroll
          }
          
          # Combine with the rest of your JavaScript
          runjs(sprintf("
            %s  // Scroll JS (might be empty)

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
              const wrapper = document.createElement('div');
              wrapper.style.cssText = `
                width: 100%%;
                position: relative;
                order: 1;
              `;

              infoBox = document.createElement('div');
              infoBox.id = 'source_info_%d';
              infoBox.style.cssText = `
                margin: 5px 0;
                padding: 10px;
                background-color: #f8f8f8;
                border: 1px solid #e8e8e8;
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
                <strong>Page:</strong> %s
              `;

              wrapper.appendChild(infoBox);

              buttonContainer.style.cssText = `
                display: flex;
                flex-wrap: wrap;
                align-items: center;
                gap: 10px;
              `;

              sourceBtn.insertAdjacentElement('afterend', wrapper);
            } else {
              infoBox.parentElement.remove();
            }
          ", scroll_js, local_i, local_i, local_i, local_i, source_text, page_num))
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
  
  
  ## Handle PDF upload ----
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
  
  ## Display all PDF pages as images ----
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
  
  
  #Mistral ----
  
  ### Mistral API key validation ----
  observeEvent(input$validateKeyMistral, {
    req(input$apiKeyMistral)
    req(input$modelSelectMistral)
    
    # Update button to loading state
    validationStatusMistral("loading")
    updateActionButton(session, "validateKeyMistral",
                       label = "Validating...",
                       icon = icon("spinner", class = "fa-spin"))
    
    # Show loading notification
    id <- showNotification(
      "Validating API key...", 
      type = "default",
      duration = NULL,
      closeButton = FALSE
    )
    
    # Validate the API key using a simple test request
    tryCatch({
      response <- httr::POST(
        url = "https://api.mistral.ai/v1/chat/completions",
        httr::add_headers(
          "Authorization" = paste("Bearer", input$apiKeyMistral),
          "Content-Type" = "application/json"
        ),
        body = jsonlite::toJSON(list(
          model = input$modelSelectMistral,
          messages = list(
            list(
              role = "user",
              content = "Test API connection"
            )
          ),
          max_tokens = 10  # Minimal response to speed up test
        ), auto_unbox = TRUE),
        httr::timeout(10)  # 10-second timeout
      )
      
      # Check response
      if (httr::status_code(response) == 200) {
        removeNotification(id)
        validationStatusMistral("success")
        updateActionButton(session, "validateKeyMistral",
                           label = "",
                           icon = icon("check"))
        
        # More explicit JS to add success class and style
        runjs("
        $('#validateKeyMistral').addClass('validate-btn-success');
        $('#validateKeyMistral').css({
          'background-color': '#28a745',
          'color': 'white',
          'border-color': '#28a745'
        });
      ")
        
        showNotification(
          "API key is valid!", 
          type = "default",
          duration = 5
        )
      } else {
        removeNotification(id)
        validationStatusMistral("failed")
        updateActionButton(session, "validateKeyMistral",
                           label = "Validate Key",
                           icon = icon("check"))
        
        # Reset button style
        runjs("
        $('#validateKeyMistral').removeClass('validate-btn-success');
        $('#validateKeyMistral').css({
          'background-color': '',
          'color': '',
          'border-color': ''
        });
      ")
        
        # Try to get more information about the error
        error_content <- tryCatch(
          httr::content(response, "text", encoding = "UTF-8"),
          error = function(e) "Unable to parse error response"
        )
        
        showNotification(
          paste("Invalid API key. Status:", httr::status_code(response), 
                "Response:", error_content), 
          type = "error",
          duration = 5
        )
      }
    }, error = function(e) {
      removeNotification(id)
      validationStatusMistral("failed")
      updateActionButton(session, "validateKeyMistral",
                         label = "Validate Key",
                         icon = icon("check"))
      
      # Reset button style
      runjs("
      $('#validateKeyMistral').removeClass('validate-btn-success');
      $('#validateKeyMistral').css({
        'background-color': '',
        'color': '',
        'border-color': ''
      });
    ")
      
      # More detailed error handling
      error_message <- if(inherits(e, "error")) {
        if(grepl("Could not resolve host", e$message)) {
          "Network error: Please check your internet connection"
        } else if(grepl("Operation timed out", e$message)) {
          "Request timed out. Please check your internet connection and try again."
        } else {
          paste("Error validating API key:", e$message)
        }
      } else {
        "An unknown error occurred"
      }
      
      showNotification(
        error_message,
        type = "error",
        duration = 5
      )
    })
  })
  
  ##Mistral Analysis----
  analyze_with_mistral <- function(pdf_text, prompts, config, progress_callback = NULL) {  
    # Basic validation  
    if (is.null(pdf_text) || length(pdf_text) == 0) {  
      stop("PDF text is empty or null")  
    }  
    if (is.null(prompts) || length(prompts) == 0) {  
      stop("Prompts are empty or null")  
    }  
    
    # Simple configuration extraction  
    base_url <- config$api$mistral$base_url  
    model <- config$api$mistral$model  
    api_key <- config$api$mistral$api_key  
    
    url <- sprintf("%s/chat/completions", base_url)  
    
    # Ensure full PDF text is captured
    full_pdf_text <- paste(pdf_text, collapse = "\n")
    
    # Debug: Print total PDF text length and first/last characters
    total_pdf_text_length <- nchar(full_pdf_text)  
    print(paste("Total PDF text length:", total_pdf_text_length, "characters"))
    
    # Print out the exact prompts for debugging
    print("Exact Prompts:")
    for (i in seq_along(prompts)) {
      print(paste(i, ":", prompts[i]))
    }
    
    # Construct payload with correct structure  
    payload <- list(  
      model = model,  
      messages = list(  
        # Ensure each message is a list with explicit role and content  
        list(  
          role = "system",   
          content = "You are an expert PDF analyzer. You will process multiple prompts about this document."  
        ),  
        list(  
          role = "user",   
          content = sprintf("Analyze this PDF text and answer ALL of the following prompts:  
  
PDF TEXT:  
%s  
  
PROMPTS:  
%s  
  
For EACH prompt, provide:  
- PROMPT: [original prompt]  
- ANSWER: [comprehensive answer]  
- SOURCE: [exact supporting text from PDF, if applicable]  
- PAGE: [page number where source appears]  
  
If no direct source is found, explain your reasoning.",   
                            full_pdf_text,   
                            paste(prompts, collapse = "\n")  
          )  
        )
      ),  
      temperature = 0.1,  
      max_tokens = 8192  # Increased token limit  
    )
    
    # Comprehensive API call with multiple error handling mechanisms
    result <- tryCatch({
      # Use httr2 for more robust request handling
      req <- httr2::request(url) %>%
        httr2::req_method("POST") %>%
        httr2::req_headers(
          "Content-Type" = "application/json",
          "Authorization" = paste("Bearer", api_key)
        ) %>%
        httr2::req_body_json(payload) %>%
        httr2::req_timeout(300)  # 5-minute timeout
      
      # Perform the request with error tracking
      resp <- httr2::req_perform(req)
      
      # Parse response
      response_content <- httr2::resp_body_json(resp)
      
      # Extract response text
      response_text <- response_content$choices[[1]]$message$content
      
      # Debug print
      print("Response received successfully")
      print("Full response text:")
      print(response_text)
      
      # Process responses with improved parsing
      responses <- lapply(seq_along(prompts), function(i) {
        # Call progress callback if provided
        if (!is.null(progress_callback)) {
          progress_callback(i, length(prompts))
        }
        
        # Improved parsing logic
        prompt <- prompts[i]
        
        # Print debug information for each prompt
        print(paste("Processing prompt", i, ":", prompt))
        
        # Split the response text into sections
        response_sections <- strsplit(response_text, "### PROMPT: ")[[1]]
        
        # Skip the first element (which is empty or contains text before first prompt)
        response_sections <- response_sections[-1]
        
        # Find the matching section
        matching_section <- NULL
        for (section in response_sections) {
          # Check if the section starts with the current prompt
          if (startsWith(section, prompt)) {
            matching_section <- section
            break
          }
        }
        
        if (!is.null(matching_section)) {
          print("Matched prompt section:")
          print(matching_section)
          
          # Manual parsing of answer, source, and page
          # Extract answer
          answer_start <- regexpr("\\*\\*ANSWER:\\*\\*", matching_section)
          source_start <- regexpr("\\*\\*SOURCE:\\*\\*", matching_section)
          page_start <- regexpr("\\*\\*PAGE:\\*\\*", matching_section)
          
          answer <- if (answer_start > 0) {
            end_pos <- if (source_start > 0) source_start else 
              if (page_start > 0) page_start else nchar(matching_section)
            trimws(substr(matching_section, 
                          answer_start + attr(regexpr("\\*\\*ANSWER:\\*\\*", matching_section), "match.length"),
                          end_pos - 1))
          } else "No answer found"
          
          # Extract source
          source <- if (source_start > 0) {
            page_start <- regexpr("\\*\\*PAGE:\\*\\*", matching_section)
            end_pos <- if (page_start > 0) page_start else nchar(matching_section)
            trimws(substr(matching_section, 
                          source_start + attr(regexpr("\\*\\*SOURCE:\\*\\*", matching_section), "match.length"),
                          end_pos - 1))
          } else NULL
          
          # Extract page
          page <- if (page_start > 0) {
            trimws(substr(matching_section, 
                          page_start + attr(regexpr("\\*\\*PAGE:\\*\\*", matching_section), "match.length"),
                          nchar(matching_section)))
          } else "N/A"
          
          return(list(
            answer = answer,
            source = source,
            page = page
          ))
        } else {
          print(paste("No section found for prompt:", prompt))
          print("Debugging details:")
          print("Available sections:")
          for (j in seq_along(response_sections)) {
            print(paste(j, ":", substr(response_sections[j], 1, 100)))
          }
          
          return(list(
            answer = "No response found for this prompt",
            source = NULL,
            page = NULL
          ))
        }
      })
      
      return(responses)
      
    }, error = function(e) {
      # Comprehensive error handling
      print("Critical error in Mistral API call:")
      print(e$message)
      
      # Return placeholder responses
      return(lapply(prompts, function(x) list(
        answer = paste("API Error:", e$message),
        source = NULL,
        page = NULL
      )))
    })
    
    return(result)
  }
  
  ### Mistral Save settings ----
  observeEvent(input$saveSettingsMistral, {  
    # Read the existing config while preserving comments and structure  
    if(file.exists("config.yml")){  
      current_config <- yaml::read_yaml("config.yml")  
    } else {  
      # Default config if file doesn't exist  
      current_config <- list(  
        api = list(  
          gemini = list(  
            model = "gemini-1.5-flash",  
            api_key = "your_gemini_api_key",  
            rate_limits = list(requests_per_minute = 2, requests_per_day = 1500)  
          ),  
          mistral = list(  
            model = "mistral-large-latest",  
            api_key = "your_mistral_api_key",  
            rate_limits = list(requests_per_minute = 2)
          ),
          openrouter = list(  
            model = "enter your model",  
            api_key = "your_open_router_api_key",  
            rate_limits = list(requests_per_minute = 2)
          )  
        )  
      )  
    }  
    
    # Only update the specific values we want to change  
    current_config$api$mistral$api_key <- input$apiKeyMistral  
    current_config$api$mistral$model <- input$modelSelectMistral    
    
    ### Mistral write config.yml ----  
    write_config(current_config)  
    
    showNotification("API Settings saved successfully", type = "message")  
  })  
  
  
### OpenRouter API key validation ----
  observeEvent(input$validateKeyOpenRouter, {
    req(input$apiKeyOpenRouter)
    req(input$modelSelectOpenRouter)
    
    # Update button to loading state
    validationStatusOpenRouter("loading")
    updateActionButton(session, "validateKeyOpenRouter",
                       label = "Validating...",
                       icon = icon("spinner", class = "fa-spin"))
    
    # Show loading notification
    id <- showNotification(
      "Validating API key...",
      type = "default",
      duration = NULL,
      closeButton = FALSE
    )
    
    # Validate the API key using a simple test request
    tryCatch({
      response <- httr::POST(
        url = "https://openrouter.ai/api/v1/chat/completions",
        httr::add_headers(
          "Authorization" = paste("Bearer", input$apiKeyOpenRouter),
          "Content-Type" = "application/json"
        ),
        body = jsonlite::toJSON(list(
          model = input$modelSelectOpenRouter,
          messages = list(
            list(
              role = "user",
              content = "Test API connection"
            )
          ),
          max_tokens = 10  # Minimal response to speed up test
        ), auto_unbox = TRUE),
        httr::timeout(10)  # 10-second timeout
      )
      
      # Check response
      if (httr::status_code(response) == 200) {
        removeNotification(id)
        validationStatusOpenRouter("success")
        updateActionButton(session, "validateKeyOpenRouter",
                           label = "",
                           icon = icon("check"))
        
        # More explicit JS to add success class and style
        runjs("
        $('#validateKeyOpenRouter').addClass('validate-btn-success');
        $('#validateKeyOpenRouter').css({
          'background-color': '#28a745',
          'color': 'white',
          'border-color': '#28a745'
        });
      ")
        
        showNotification(
          "API key is valid!",
          type = "default",
          duration = 5
        )
      } else {
        removeNotification(id)
        validationStatusOpenRouter("failed")
        updateActionButton(session, "validateKeyOpenRouter",
                           label = "Validate Key",
                           icon = icon("check"))
        
        # Reset button style
        runjs("
        $('#validateKeyOpenRouter').removeClass('validate-btn-success');
        $('#validateKeyOpenRouter').css({
          'background-color': '',
          'color': '',
          'border-color': ''
        });
      ")
        
        # Try to get more information about the error
        error_content <- tryCatch(
          httr::content(response, "text", encoding = "UTF-8"),
          error = function(e) "Unable to parse error response"
        )
        
        # Print the full response for debugging
        print("Full Response:")
        print(response)
        print("Error Content:")
        print(error_content)
        
        showNotification(
          paste("Invalid API key. Status:", httr::status_code(response),
                "Response:", error_content),
          type = "error",
          duration = 5
        )
      }
    }, error = function(e) {
      removeNotification(id)
      validationStatusOpenRouter("failed")
      updateActionButton(session, "validateKeyOpenRouter",
                         label = "Validate Key",
                         icon = icon("check"))
      
      # Reset button style
      runjs("
      $('#validateKeyOpenRouter').removeClass('validate-btn-success');
      $('#validateKeyOpenRouter').css({
        'background-color': '',
        'color': '',
        'border-color': ''
      });
    ")
      
      # More detailed error handling
      error_message <- if(inherits(e, "error")) {
        if(grepl("Could not resolve host", e$message)) {
          "Network error: Please check your internet connection"
        } else if(grepl("Operation timed out", e$message)) {
          "Request timed out. Please check your internet connection and try again."
        } else {
          paste("Error validating API key:", e$message, "Class:", class(e))
        }
      } else {
        paste("An unknown error occurred:", e)
      }
      
      # Print the full error object for debugging
      print("Full Error Object:")
      print(e)
      
      showNotification(
        error_message,
        type = "error",
        duration = 5
      )
    })
  })
  
  ##OpenRouter Analysis----
  analyze_with_openrouter <- function(pdf_text, prompts, config, progress_callback = NULL) {
    # Basic validation
    if (is.null(pdf_text) || length(pdf_text) == 0) {
      stop("PDF text is empty or null")
    }
    if (is.null(prompts) || length(prompts) == 0) {
      stop("Prompts are empty or null")
    }
    
    # Simple configuration extraction
    base_url <- "https://openrouter.ai/api/v1" # OpenRouter base URL
    model <- config$api$openrouter$model
    api_key <- config$api$openrouter$api_key
    
    url <- sprintf("%s/chat/completions", base_url)
    
    # Ensure full PDF text is captured
    full_pdf_text <- paste(pdf_text, collapse = "\n")
    
    # Debug: Print total PDF text length and first/last characters
    total_pdf_text_length <- nchar(full_pdf_text)
    print(paste("Total PDF text length:", total_pdf_text_length, "characters"))
    
    # Print out the exact prompts for debugging
    print("Exact Prompts:")
    for (i in seq_along(prompts)) {
      print(paste(i, ":", prompts[i]))
    }
    
    # Construct payload with correct structure
    payload <- list(
      model = model,
      messages = list(
        # Ensure each message is a list with explicit role and content
        list(
          role = "system",
          content = "You are an expert PDF analyzer. You will process multiple prompts about this document."
        ),
        list(
          role = "user",
          content = sprintf("Analyze this PDF text and answer ALL of the following prompts:

PDF TEXT:
%s

PROMPTS:
%s

For EACH prompt, provide:
- PROMPT: [original prompt]
- ANSWER: [comprehensive answer]
- SOURCE: [exact supporting text from PDF, if applicable]
- PAGE: [page number where source appears]

If no direct source is found, explain your reasoning.",
                            full_pdf_text,
                            paste(prompts, collapse = "\n")
          )
        )
      ),
      temperature = 0.1,
      max_tokens = 8192  # Increased token limit
    )
    
    # Comprehensive API call with multiple error handling mechanisms
    result <- tryCatch({
      # Use httr2 for more robust request handling
      req <- httr2::request(url) %>%
        httr2::req_method("POST") %>%
        httr2::req_headers(
          "Content-Type" = "application/json",
          "Authorization" = paste("Bearer", api_key),
        ) %>%
        httr2::req_body_json(payload) %>%
        httr2::req_timeout(300)  # 5-minute timeout
      
      # Perform the request with error tracking
      resp <- httr2::req_perform(req)
      
      # Parse response
      response_content <- httr2::resp_body_json(resp)
      
      # Extract response text
      response_text <- response_content$choices[[1]]$message$content
      
      # Debug print
      print("Response received successfully")
      print("Full response text:")
      print(response_text)
      
      # Process responses with improved parsing
      responses <- lapply(seq_along(prompts), function(i) {
        # Call progress callback if provided
        if (!is.null(progress_callback)) {
          progress_callback(i, length(prompts))
        }
        
        prompt <- prompts[i]
        print(paste("Processing prompt", i, ":", prompt))
        
        # Split the response text into sections, trimming whitespace
        response_sections <- strsplit(trimws(response_text), "\n\nPROMPT: ")[[1]]
        
        # For the first section, remove the "PROMPT: " prefix if it exists
        response_sections[1] <- sub("^PROMPT: ", "", response_sections[1])
        
        # Find the matching section
        matching_section <- NULL
        for (section in response_sections) {
          # Clean up the section text
          clean_section <- gsub("^\\s+|\\s+$", "", section)
          
          # Check if the section starts with the current prompt
          if (startsWith(clean_section, prompt)) {
            matching_section <- clean_section
            break
          }
        }
        
        if (!is.null(matching_section)) {
          # Extract answer (everything between "ANSWER:" and "SOURCE:")
          answer <- sub(".*\nANSWER: ([^\n]*).*", "\\1", matching_section)
          if (answer == matching_section) answer <- "No answer found"
          
          # Extract source (everything between "SOURCE:" and "PAGE:")
          source <- if (grepl("SOURCE:", matching_section)) {
            source_text <- sub(".*SOURCE:\\s*([^\n]*).*", "\\1", matching_section)
            if (source_text != matching_section) source_text else NULL
          } else NULL
          
          # Extract page (everything after "PAGE:")
          page <- if (grepl("PAGE:", matching_section)) {
            page_text <- sub(".*PAGE:\\s*([^\n]*).*", "\\1", matching_section)
            if (page_text != matching_section) page_text else "N/A"
          } else "N/A"
          
          return(list(
            answer = answer,
            source = source,
            page = page
          ))
        } else {
          print(paste("No section found for prompt:", prompt))
          print("Debugging details:")
          print("Available sections:")
          for (j in seq_along(response_sections)) {
            print(paste(j, ":", substr(response_sections[j], 1, 100)))
          }
          
          return(list(
            answer = "No response found for this prompt",
            source = NULL,
            page = NULL
          ))
        }
      })
      
      return(responses)
      
    }, error = function(e) {
      # Comprehensive error handling
      print("Critical error in OpenRouter API call:")
      print(e$message)
      
      # Return placeholder responses
      return(lapply(prompts, function(x) list(
        answer = paste("API Error:", e$message),
        source = NULL,
        page = NULL
      )))
    })
    
    return(result)
  }
  
  ### OpenRouter Save settings ----
  observeEvent(input$saveSettingsOpenRouter, {
    # Read the existing config while preserving comments and structure
    if(file.exists("config.yml")){
      current_config <- yaml::read_yaml("config.yml")
    } else {
      # Default config if file doesn't exist
      current_config <- list(
        api = list(
          gemini = list(
            model = "gemini-1.5-flash",
            api_key = "your_gemini_api_key",
            rate_limits = list(requests_per_minute = 2, requests_per_day = 1500)
          ),
          mistral = list(
            model = "mistral-large-latest",
            api_key = "your_mistral_api_key",
            rate_limits = list(requests_per_minute = 2)
          ),
          openrouter = list(
            model = "enter your model",
            api_key = "your_open_router_api_key",
            rate_limits = list(requests_per_minute = 2)
          )
        )
      )
    }
    
    # Only update the specific values we want to change
    current_config$api$openrouter$api_key <- input$apiKeyOpenRouter
    current_config$api$openrouter$model <- input$modelSelectOpenRouter
    
    ### OpenRouter write config.yml ----
    write_config(current_config)
    
    showNotification("API Settings saved successfully", type = "message")
  })

  
}
  # Run the Shiny app
shinyApp(ui = ui, server = server)