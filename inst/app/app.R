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
library(ollamar)

source(system.file("app/config_helpers.R", package = "AIDE"))

# Load configuration from the parent directory
config <- read_config()

# UI Definition ----
ui <- dashboardPage(title= "AI-Assisted Data Extraction",
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
                  p("AIDE was developed to greatly accelerate the data extraction process for systematic review and meta-analysis. It relies on you either using local models via Ollama or having an API key for Google Gemini, Mistral, or Open Router (which all offer free tiers as of December 2024).",
                  ),
                ),
              ),
              box(
                width = 6,
                title = "How to Use This App",
                div(
                  style = "display: flex; flex-direction: column; gap: 8px;", # Reduc
                  strong("LLMs via API"),
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
                  p("4) Move to the analyze page and upload the PDF file you want to analyze. You will see a context size estimate - ensure it fits with the model you are using."),
                  p("5) Click Analyze button. Your results will autofill under the appropriate prompt."),
                  p("6) Review each response. You can view the source information by clicking the Source button. You can record each result by clicking the record button. This will save it to your coding form on your local machine."),
                  p("7) When ready to work on the next PDF, simply upload a new one and the coding form will reset."),
                  strong("LLMs via Ollama"),
                  p("1) You will Ollama installed, as well as the LLM model you want to use downloaded to Ollama.", 
                    HTML("Ollama:  
                      <ul>  
                        <li><a href='https://ollama.com/' target='_blank' style='display: inline;'>Ollama</a></li>  
                        <li><a href='https://ollama.com/search' target='_blank' style='display: inline;'>Ollama Models</a></li>  
                      </ul>")
                  ),
                  p("2) Set up your coding form. Importantly, your first row will be read as prompts by the large language model. The accuracy of the LLM's responses will be influenced very strongly by your prompts. Better prompts = better results."),
                  p("3) Move to the LLM Setup tab of this app where you can continue with the following steps:"),
                  p("4) Upload your coding form. Your coding form should be in Excel format for best results."),
                  p("4) Move to the analyze page and upload the PDF file you want to analyze. You will see a context size estimate - ensure it fits with the model you are using. Then set your context window size."),
                  p("5) Click Analyze button. The speed of the analysis will depend on a) your computer hardware, b) what is being analyzed, c) the context window size. Your results will autofill under the appropriate prompt."),
                  p("6) Review each response. You can view the source information by clicking the Source button. You can record each result by clicking the record button. This will save it to your coding form on your local machine."),
                  p("7) When ready to work on the next PDF, simply upload a new one and the coding form will reset."),
                  ),
              ),
              box(
                width = 6,
                title = "Common Questions",
                div(
                  style = "display: flex; flex-direction: column; gap: 8px;", # Reduc
                  strong("How big of a context window do I need?"),
                  p("We recommend models with a minimum of 32K context window. We have tried to include models that have large (128K+) context windows (Gemini, Mistral Large 2). Every time you upload a PDF, you'll see an estimated context window size. If you are running local models, you will be able to set your context size."),
                  strong("What LLM should I use?"),
                  p(
                  HTML("General Thoughts:  
                      <ul>  
                      <li>We have had excellent results with Google Gemini models. Mistral large also performs very well in our testing.</li>  
                      <li>Running local models with Ollama requires a bit of knowledge and a fair bit of computational resources. This feature is currently in beta because each local model responds differently, so it is challenging for this software parse the response and source information.</li>  
                      <li>Mistral large performs very well in our testing.</li>  
                      <li>Open Router's free tier tends to offer smaller context windows and consequently, these models have not performed well in our informal testing. For this reason, we suggest using either Gemini or Mistral models as of December 2024.</li>  
                      </ul>")
                    ),
                  p("If you want to use a paid service, like ChatGPT or Claude, they are accessible through Open Router. However, as of December 2024 we have not yet tested if they reply in a way that this app can easily parse."),
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
                        <li>Local Models with Ollama</li> 
                        <ul>
                          <li>System prompt: None</li>
                          <li>User prompt: Analyze this PDF and answer ALL of the following prompts. IMPORTANT: For EACH prompt, you must respond in this format:
 [continues into answer, source, page labels]</li>
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
                              choices = c("Google Gemini API", "Local Models with Ollama", "Mistral API", "OpenRouter API"),  
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
                    p("1) The free tier of the Google Gemini API has rate limits. You can check current rate limits for the free tier here:", HTML('<a href="https://ai.google.dev/pricing" target="_blank" style="display: inline;">Google AI API Pricing.</a>')),
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
                                  config <- read_config()
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
                                  fresh_config <- read_config()
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
                                     fresh_config <- read_config()
                                     fresh_config$api$gemini$requests_per_minute
                                   }, error = function(e) {
                                     if(input$modelSelect == "gemini-1.5-flash") 2 else 2
                                   }),
                                   min = 1,
                                   width = "190px"),
                      numericInput("requestsPerDay", "Requests per Day",
                                   value = tryCatch({
                                     fresh_config <- read_config()
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
                                  config <- read_config()
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
                                  fresh_config <- read_config()
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
                    p("3) Note that different models are made for different things and not all will perform well for data extraction. In fact, of the freely available models from Open Router that we tested during development (December, 2024), none worked as well as the models available through the Gemini or Mistral APIs. However, this is likely due to the free API providers limiting the context window. For example, at the time of development, Meta: Llama 3.1 405B Instruct (free) has a 128k context window, but the provider of the free access only allows 8k context window (input + output). This means that the average journal article may not fit within the context window, meaning the entire article would not be reviewed by the LLM. In our testing, it was common to only get responses based on the first page of the PDF. For this reason, for free models, we recommend Gemini or Mistral APIs at this time."),
                    p("4) We have not tested any paid models. Use paid models at your own risk, there is no guarentee they will be compatible with this app."),
                    p("5) Open Router provides access to a large number of models by a number of different companies. Not all conform to the same API standards. Therefore, there is no guarantee that this app can utilize and parse the responses from any specific LLM. At the time of development (December, 2024), we tested the following models:"),
                    HTML("Generally Working Models (not guaranteed)  
                        <ul>  
                          <li>Mythomax-l2-13b</li>  
                          <li>Microsoft Phi 3 Mini 128k Instruct</li>
                          <li>Mistral 7B Instruct (we recommend using Mistral API instead though, which uses larger models and produces better results in our testing.)</li>
                          <li>Most Meta Llama 3, 3.1, or 3.2 model</li>  
                          <li>Microsoft Phi 3 Medium 128k Instruct</li>
                          <li>Open Chat 7B</li>
                          <li>Toppy M 7b</li>
                          <li>Qwen 2 6b Instruct</li>
                          <li>Zephyr 7b Beta</li>
                        </ul>  
                      "), 
                    HTML("Partially Working Models (source button will not work, but source is shown in R console)  
                        <ul>  
                          <li>Meta Llama 3.2 90b Vision Instruct</li>
                          <li>Meta Llama 3.1 70b Instruct</li>
                        </ul>  
                      "), 
                    HTML("The following models will not work in this app:  
                        <ul>  
                          <li>Any Google Gemini Model (use the Gemini API in the app instead)</li>
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
                                  config <- read_config()
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
                                  fresh_config <- read_config()
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
              
              #Ollama Selection----
              conditionalPanel(    
                condition = "input.llmMethod == 'Local Models with Ollama'",    
                box(    
                  width = 12,    
                  title = "Local LLM with Ollama Notes",    
                  div(    
                    style = "display: flex; flex-direction: column; gap: 8px;",    
                    box(h2("This feature is considered still in beta."),
                    p("It works, but the huge variety of LLMs available means that not all models respond in a way where their response will be correctly parsed. Consequently, some models will place their entire response (prompt, answer, source) in the answer box in this software."),
                    ),
                    p("1) Not all local LLMs perform the same for this task. With this app's backend strategy, you should focus on models with large context windows (32k or higher)."), 
                    p("2) Ollama models should be downloaded in Ollama before starting the app."),
                    p("3) The Ollama models on your computer should appear in the model settings box below in the drop down menu."),
                    p("5) Ollama provides access to a large number of models by a number of different companies. Not all conform to the same standards. Therefore, there is no guarantee that this app can utilize and parse the responses from any specific LLM."),
                    strong("General Considerations Before Using Ollama Models"),
                    HTML("LLM Requirements:
                        <ul>  
                          <li>In our testing, a 32k context window is the absolute minimum that was viable. We would recommend 128k context length, but 32k worked for our use cases.</li>  
                          <li>More parameters does not necessarily mean it will perform better.</li>
                        </ul>  
                      "), 
                    HTML("Hardware Requirements:
                        <ul>  
                          <li>LLMs are very GPU-intensive. We tested small models (2b parameters) with a 32k context window. When we had ~20k total tokens, each prompt took approximately 2 minutes to generate a response when using a RTX GeFORCE 4060 (8gb VRAM) on a PC with 32GB RAM and a i5-13600k processor. This means completing a 5 variable coding form would take approximately 10 minutes on this machine. This is VERY slow compared to, for example, Gemini or Mistral models available through API.</li>
                        </ul>  
                      "), 
                  )
                ),
                
                box(    
                  width = 6,    
                  title = "Ollama Model Settings",    
                  div(    
                    style = "display: flex; flex-direction: column; gap: 8px;",    
                    div(        
                      style = "margin-bottom: 10px; display: flex; align-items: center; gap: 10px;",        
                      h4("Ollama Server Status:", style = "margin: 0;"), # margin: 0 removes default margins  
                      htmlOutput("ollamaStatus")    
                    ),        
                    
                    # Model Selection    
                    div(    
                      style = "margin-bottom: 10px;",    
                      uiOutput("dynamicModelSelect")    
                    ),    
                    
                    # Explore Models Button    
                    div(    
                      style = "margin-bottom: 10px;",    
                      tags$a(    
                        href = "https://ollama.com/search",    
                        target = "_blank",    
                        class = "btn btn-default",    
                        icon("search"),    
                        "Explore Ollama Models"    
                      )    
                    ),    
                    
                    # Save Settings Button    
                    uiOutput("containerStatus"),  
                    
                    uiOutput("saveButtonOllama"),   
                    uiOutput("modelTestStatus"),
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
      ),
      tabItem("Cite",
              
              
              
              # UI component
              box(
                width = 12,
                title = "Please cite AIDE if you use it in your research.",
                div(
                  style = "display: flex; flex-direction: column; gap: 8px;", # Reduced from 15px to 8px
                  h3("bibtext"),
                  p(
                    HTML(
                      "@misc{schroeder2025largelanguagemodelshumanintheloop<br>
    &nbsp;&nbsp;&nbsp;&nbsp;title={Large Language Models with Human-In-The-Loop Validation for Systematic Review Data Extraction},<br>
    &nbsp;&nbsp;&nbsp;&nbsp;author={Noah L. Schroeder and Chris Davis Jaldi and Shan Zhang},<br>
    &nbsp;&nbsp;&nbsp;&nbsp;year={2025},<br>
    &nbsp;&nbsp;&nbsp;&nbsp;eprint={2501.11840},<br>
    &nbsp;&nbsp;&nbsp;&nbsp;archivePrefix={arXiv},<br>
    &nbsp;&nbsp;&nbsp;&nbsp;primaryClass={cs.HC},<br>
    &nbsp;&nbsp;&nbsp;&nbsp;url={https://arxiv.org/abs/2501.11840},<br>
    }"
                    )
                  ),
                  h3("APA"),
                  p("Schroeder, N. L., Jaldi, C. D., & Zhang, S. (2025). Large Language Models with Human-In-The-Loop Validation for Systematic Review Data Extraction. https://doi.org/10.48550/arXiv.2501.11840"),
                ),
              ),
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
    model_activated = FALSE,  
    model_test_status = NULL,
    is_processing = FALSE,
    selected_ollama_model = NULL
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
    # Input validation
    req(input$apiKey, input$modelSelect, 
        input$requestsPerMinute, input$requestsPerDay)
    
    # Debug prints
    message("Attempting to save Gemini settings...")
    message("Config file exists: ", file.exists(get_config_path()))
    message("Config file permissions: ")
    print(file.info(get_config_path()))
    
    tryCatch({
      # Check if we can read the current config
      current_config <- read_config()
      message("Successfully read current config")
      
      # Store original values for comparison
      original_values <- list(
        api_key = current_config$api$gemini$api_key,
        model = current_config$api$gemini$model,
        rpm = current_config$api$gemini$rate_limits$requests_per_minute,
        rpd = current_config$api$gemini$rate_limits$requests_per_day
      )
      
      # Validate input values
      if (is.null(input$apiKey) || input$apiKey == "") {
        showNotification("API key cannot be empty", type = "error")
        return()
      }
      
      if (is.null(input$modelSelect) || input$modelSelect == "") {
        showNotification("Model must be selected", type = "error")
        return()
      }
      
      # Validate rate limits are positive numbers
      if (!is.numeric(input$requestsPerMinute) || input$requestsPerMinute <= 0) {
        showNotification("Requests per minute must be a positive number", type = "error")
        return()
      }
      
      if (!is.numeric(input$requestsPerDay) || input$requestsPerDay <= 0) {
        showNotification("Requests per day must be a positive number", type = "error")
        return()
      }
      
      # Update the specific values
      current_config$api$gemini$api_key <- input$apiKey
      current_config$api$gemini$model <- input$modelSelect
      current_config$api$gemini$rate_limits$requests_per_minute <- input$requestsPerMinute
      current_config$api$gemini$rate_limits$requests_per_day <- input$requestsPerDay
      
      message("About to write config...")
      # Write the updated config using your function
      write_config(current_config)
      message("Config written")
      
      # Verify the changes
      updated_config <- read_config()
      if (identical(updated_config$api$gemini$api_key, input$apiKey) &&
          identical(updated_config$api$gemini$model, input$modelSelect) &&
          identical(updated_config$api$gemini$rate_limits$requests_per_minute, input$requestsPerMinute) &&
          identical(updated_config$api$gemini$rate_limits$requests_per_day, input$requestsPerDay)) {
        
        showNotification("Gemini API Settings saved successfully", type = "message")
        
        # Print confirmation of changes
        message("Settings updated successfully:")
        message("API Key changed from: ", original_values$api_key, " to: ", updated_config$api$gemini$api_key)
        message("Model changed from: ", original_values$model, " to: ", updated_config$api$gemini$model)
        message("Requests per minute changed from: ", original_values$rpm, " to: ", 
                updated_config$api$gemini$rate_limits$requests_per_minute)
        message("Requests per day changed from: ", original_values$rpd, " to: ", 
                updated_config$api$gemini$rate_limits$requests_per_day)
        
      } else {
        showNotification(
          "Settings may not have saved correctly. Please verify.", 
          type = "warning"
        )
        
        # Print what didn't match
        message("Verification failed:")
        message("Expected API key: ", input$apiKey)
        message("Actual API key: ", updated_config$api$gemini$api_key)
        message("Expected model: ", input$modelSelect)
        message("Actual model: ", updated_config$api$gemini$model)
        message("Expected requests per minute: ", input$requestsPerMinute)
        message("Actual requests per minute: ", updated_config$api$gemini$rate_limits$requests_per_minute)
        message("Expected requests per day: ", input$requestsPerDay)
        message("Actual requests per day: ", updated_config$api$gemini$rate_limits$requests_per_day)
      }
      
    }, error = function(e) {
      showNotification(
        paste("Error saving config:", e$message), 
        type = "error"
      )
      message("Error details: ", e$message)
    })
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
                                "OpenRouter API" = analyze_with_openrouter,
                                "Local Models with Ollama" = analyze_with_ollama)
    
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
      
      responses <- if (input$llmMethod == "Local Models with Ollama") {
        analysis_function(  
          pdf_text = rv$pdf_text,  
          prompts = rv$prompts,
          selected_model = input$selectedOllamaModel,
          context_window = input$contextWindow,
          progress_callback = function(current, total) {  
            # Update progress bar  
            progress_pct <- (current/total) * 100  
            runjs(sprintf("  
        $('.progress-bar').css('width', '%s%%');  
        $('#progress-detail').text('Processing prompt %d of %d');  
      ", progress_pct, current, total))  
          }  
        )
      } else {
        analysis_function(  
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
      }
      
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
      
      #modal
      # Calculate context estimate  
      context_est <- estimate_context_size(rv$pdf_text, rv$prompts)  
      
      # Show modal with context information  
      showModal(modalDialog(
        title = "PDF Context Size Analysis",
        size = "l",
        
        # First row with Context Size and Context Window settings
        fluidRow(
          # Left column for Estimated Context Size
          column(
            width = 6,
            div(
              h4("Estimated Context Size"),
              p("*estimated as 4 characters = 1 token."),
              tags$ul(
                tags$li(
                  tags$strong("Total Tokens: "),
                  sprintf("%d", context_est$total_tokens)
                ),
                tags$li(
                  tags$strong("System Message: "),
                  sprintf("%d tokens", context_est$breakdown$system_message)
                ),
                tags$li(
                  tags$strong("PDF Content: "),
                  sprintf("%d tokens", context_est$breakdown$pdf_content)
                ),
                tags$li(
                  tags$strong("Prompts: "),
                  sprintf("%d tokens", context_est$breakdown$prompts)
                ),
                tags$li(
                  tags$strong("Template: "),
                  sprintf("%d tokens", context_est$breakdown$template)
                )
              )
            )
          ),
          # Right column for Context Window settings
          column(
            width = 6,
            conditionalPanel(
              condition = "input.llmMethod == 'Local Models with Ollama'",
              div(
                h4("Set Context Window"),
                p("IMPORTANT: Different models have different context windows. Do not exceed the context window of your model. Changing your context window will also influence the hardware requirements if you are running local models with Ollama."),
                numericInput(
                  "contextWindow",
                  "Context Window Size",
                  value = 32000,
                  min = 2048,
                  max = 4000000,
                  step = 1024
                )
              )
            )
          )
        ),
        
        # Second row for Model Context Window Sizes
        fluidRow(
          column(
            width = 12,  # Full width for the table
            div(
              h4("Model Context Window Sizes"),
              tags$table(
                class = "table table-bordered",
                style = "margin-top: 20px;",
                tags$thead(
                  tags$tr(
                    tags$th("Model"),
                    tags$th("Context Size"),
                    tags$th("Status")
                  )
                ),
                tags$tbody(
                  lapply(context_est$model_compatibility, function(model) {  
                    tags$tr(  
                      tags$td(model$name),  
                      tags$td(model$description),  
                      tags$td(  
                        div(  
                          style = sprintf(  
                            "color: white; background-color: %s; padding: 4px 8px; border-radius: 4px; text-align: center;",  
                            if(model$name %in% c("Local Models with Ollama", "Open Router Models")) {  
                              "#ffc107"  # yellow for Ollama and Open Router  
                            } else if(model$compatible) {  
                              "#28a745"  # green for others that are compatible  
                            } else {  
                              "#dc3545"  # red for others that exceed  
                            }  
                          ),  
                          if(model$name %in% c("Local Models with Ollama", "Open Router Models")) {  
                            "Varies by Model"  
                          } else if(model$compatible) {  
                            "Within Context Window"  
                          } else {  
                            "Exceeds Context Window"  
                          }  
                        )  
                      )  
                    )  
                  })
                )
              )
            )
          )
        ),
        
        # Warning message if context is exceeded
        if(context_est$exceeds_context) {
          div(
            style = "margin-top: 20px; padding: 10px; background-color: #fff3cd; border: 1px solid #ffeeba; border-radius: 4px;",
            icon("exclamation-triangle"),
            "Warning: This document exceeds some model context limits. Consider using a model with larger context window."
          )
        },
        
        footer = tagList(
          modalButton("Close")
        )
      ))
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
      max_tokens = 128000  # Increased token limit  
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
    # Input validation
    req(input$apiKeyMistral, input$modelSelectMistral)
    
    # Debug prints
    message("Attempting to save settings...")
    message("Config file exists: ", file.exists(get_config_path()))
    message("Config file permissions: ")
    print(file.info(get_config_path()))
    
    tryCatch({
      # Check if we can read the current config
      current_config <- read_config()
      message("Successfully read current config")
      
      # Store original values for comparison
      original_key <- current_config$api$mistral$api_key
      original_model <- current_config$api$mistral$model
      
      # Validate input values
      if (is.null(input$apiKeyMistral) || input$apiKeyMistral == "") {
        showNotification("API key cannot be empty", type = "error")
        return()
      }
      
      if (is.null(input$modelSelectMistral) || input$modelSelectMistral == "") {
        showNotification("Model must be selected", type = "error")
        return()
      }
      
      # Update Mistral-specific values
      current_config$api$mistral$api_key <- input$apiKeyMistral
      current_config$api$mistral$model <- input$modelSelectMistral
      
      message("About to write config...")
      # Write the updated config
      write_config(current_config)
      message("Config written")
      
      # Verify the changes
      updated_config <- read_config()
      if (identical(updated_config$api$mistral$api_key, input$apiKeyMistral) &&
          identical(updated_config$api$mistral$model, input$modelSelectMistral)) {
        showNotification("Mistral API Settings saved successfully", type = "message")
        
        # Print confirmation of changes
        message("Settings updated successfully:")
        message("API Key changed from: ", original_key, " to: ", updated_config$api$mistral$api_key)
        message("Model changed from: ", original_model, " to: ", updated_config$api$mistral$model)
      } else {
        showNotification(
          "Settings may not have saved correctly. Please verify.", 
          type = "warning"
        )
        
        # Print what didn't match
        message("Verification failed:")
        message("Expected API key: ", input$apiKeyMistral)
        message("Actual API key: ", updated_config$api$mistral$api_key)
        message("Expected model: ", input$modelSelectMistral)
        message("Actual model: ", updated_config$api$mistral$model)
      }
      
    }, error = function(e) {
      showNotification(
        paste("Error saving config:", e$message), 
        type = "error"
      )
      message("Error details: ", e$message)
    })
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

IMPORTANT: For EACH prompt, you must provide:
- PROMPT: [original prompt]
- ANSWER: [comprehensive answer]
- SOURCE: [exact supporting text from PDF, if no direct text, explain reasoning]
- PAGE: [page number where source appears]",
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
    # Input validation
    req(input$apiKeyOpenRouter, input$modelSelectOpenRouter)
    
    # Debug prints
    message("Attempting to save OpenRouter settings...")
    message("Config file exists: ", file.exists(get_config_path()))
    message("Config file permissions: ")
    print(file.info(get_config_path()))
    
    tryCatch({
      # Check if we can read the current config
      current_config <- read_config()
      message("Successfully read current config")
      
      # Store original values for comparison
      original_values <- list(
        api_key = current_config$api$openrouter$api_key,
        model = current_config$api$openrouter$model
      )
      
      # Validate input values
      if (is.null(input$apiKeyOpenRouter) || input$apiKeyOpenRouter == "") {
        showNotification("OpenRouter API key cannot be empty", type = "error")
        return()
      }
      
      if (is.null(input$modelSelectOpenRouter) || input$modelSelectOpenRouter == "") {
        showNotification("OpenRouter model must be selected", type = "error")
        return()
      }
      
      # Update OpenRouter-specific values
      current_config$api$openrouter$api_key <- input$apiKeyOpenRouter
      current_config$api$openrouter$model <- input$modelSelectOpenRouter
      
      message("About to write config...")
      # Write the updated config using your function
      write_config(current_config)
      message("Config written")
      
      # Verify the changes
      updated_config <- read_config()
      if (identical(updated_config$api$openrouter$api_key, input$apiKeyOpenRouter) &&
          identical(updated_config$api$openrouter$model, input$modelSelectOpenRouter)) {
        
        showNotification("OpenRouter API Settings saved successfully", type = "message")
        
        # Print confirmation of changes
        message("Settings updated successfully:")
        message("API Key changed from: ", original_values$api_key, " to: ", updated_config$api$openrouter$api_key)
        message("Model changed from: ", original_values$model, " to: ", updated_config$api$openrouter$model)
        
      } else {
        showNotification(
          "OpenRouter settings may not have saved correctly. Please verify.", 
          type = "warning"
        )
        
        # Print what didn't match
        message("Verification failed:")
        message("Expected API key: ", input$apiKeyOpenRouter)
        message("Actual API key: ", updated_config$api$openrouter$api_key)
        message("Expected model: ", input$modelSelectOpenRouter)
        message("Actual model: ", updated_config$api$openrouter$model)
      }
      
    }, error = function(e) {
      showNotification(
        paste("Error saving OpenRouter config:", e$message), 
        type = "error"
      )
      message("Error details: ", e$message)
    })
  })

  
  # Context estimate ---- 
  observe({
    req(rv$pdf_text, rv$prompts)
    
    # Calculate context size estimate
    context_estimate <- estimate_context_size(rv$pdf_text, rv$prompts)
    
    # Store in reactive values
    rv$context_estimate <- context_estimate
    
      })
    
  
  estimate_context_size <- function(pdf_text, prompts) {
    # Constants for token estimation
    CHARS_PER_TOKEN <- 4  # Rough estimate, actual ratio varies by model/text
    
    # System message size (fixed)
    system_msg <- "You are an expert PDF analyzer. You will process multiple prompts about this document."
    system_tokens <- nchar(system_msg) / CHARS_PER_TOKEN
    
    # PDF text size
    pdf_text_combined <- paste(pdf_text, collapse = "\n")
    pdf_tokens <- nchar(pdf_text_combined) / CHARS_PER_TOKEN
    
    # Prompts size
    prompts_combined <- paste(prompts, collapse = "\n")
    prompts_tokens <- nchar(prompts_combined) / CHARS_PER_TOKEN
    
    # Template text size (fixed parts of the message)
    template_text <- "Analyze this PDF text and answer ALL of the following prompts:
PDF TEXT:
PROMPTS:
For EACH prompt, provide:
- PROMPT: [original prompt]
- ANSWER: [comprehensive answer]
- SOURCE: [exact supporting text from PDF, if applicable]
- PAGE: [page number where source appears]
If no direct source is found, explain your reasoning."
    template_tokens <- nchar(template_text) / CHARS_PER_TOKEN
    
    # Total token estimate
    total_tokens <- ceiling(system_tokens + pdf_tokens + prompts_tokens + template_tokens)
    
    # Define model context windows
    models <- list(
      "Gemini 1.5 Flash" = list(
        context_size = 1000000,
        description = "1 million tokens"
      ),
      "Gemini 1.5 Pro" = list(
        context_size = 2000000,
        description = "2 million tokens"
      ),
      "Local Models with Ollama" = list(
        context_size = "Varies by model",
        description = "Varies by model"
      ),
      "Mistral Large 2" = list(
        context_size = 128000,
        description = "128k tokens"
      ),
      "Open Router Models" = list(
        context_size = "Varies by model",
        description = "Varies by model"
      )
    )
    
    # Check compatibility with each model
    model_compatibility <- lapply(names(models), function(model_name) {
      list(
        name = model_name,
        description = models[[model_name]]$description,
        compatible = total_tokens <= models[[model_name]]$context_size,
        context_size = models[[model_name]]$context_size
      )
    })
    
    # Create detailed breakdown
    result <- list(
      total_tokens = total_tokens,
      breakdown = list(
        system_message = ceiling(system_tokens),
        pdf_content = ceiling(pdf_tokens),
        prompts = ceiling(prompts_tokens),
        template = ceiling(template_tokens)
      ),
      exceeds_context = total_tokens > 128000,
      model_compatibility = model_compatibility
    )
    
    return(result)
  }
  
  # Add an output to display the context size
  output$contextSize <- renderUI({
    req(rv$context_estimate)
    
    est <- rv$context_estimate
    
    fluidRow(
      column(
        width = 6,
        div(
          h4("Estimated Context Size"),
          tags$ul(
            tags$li(sprintf("Total Tokens: %d", est$total_tokens)),
            tags$li(sprintf("System Message: %d tokens", est$breakdown$system_message)),
            tags$li(sprintf("PDF Content: %d tokens", est$breakdown$pdf_content)),
            tags$li(sprintf("Prompts: %d tokens", est$breakdown$prompts)),
            tags$li(sprintf("Template: %d tokens", est$breakdown$template))
          ),
        )
      ),
      column(
        width = 6,
        div(
          h4("Model Context Window Sizes"),
          tags$table(
            class = "table table-bordered",
            style = "margin-top: 20px;",
            tags$thead(
              tags$tr(
                tags$th("Model"),
                tags$th("Context Size"),
                tags$th("Status")
              )
            ),
            tags$tbody(
              lapply(est$model_compatibility, function(model) {
                tags$tr(
                  tags$td(model$name),
                  tags$td(model$description),
                  tags$td(
                    div(
                      style = sprintf(
                        "color: white; background-color: %s; padding: 4px 8px; border-radius: 4px; text-align: center;",
                        if(model$name %in% c("Local Models with Ollama", "Open Router Models")) {
                          "#ffc107"  # yellow for Ollama and Open Router
                        } else if(model$compatible) {
                          "#28a745"  # green for others that are compatible
                        } else {
                          "#dc3545"  # red for others that exceed
                        }
                      ),
                      if(model$name %in% c("Local Models with Ollama", "Open Router Models")) {
                        "Varies by Model"
                      } else if(model$compatible) {
                        "Within Context Window"
                      } else {
                        "Exceeds Context Window"
                      }
                    )
                  )
                )
              })
            )
          )
        )
      )
    )
  })
  
  #Ollama----
  # Function to check Ollama server status  
  check_ollama_status <- function() {  
    tryCatch({  
      # Use system command to check Ollama server  
      status <- system2("ollama", "list", stdout = TRUE, stderr = TRUE)  
      
      # If command succeeds, server is running  
      if (length(status) > 0) {  
        return(list(  
          status = TRUE,   
          message = "Ollama server is running ✓"  
        ))  
      } else {  
        return(list(  
          status = FALSE,   
          message = "Ollama server is not running ✗"  
        ))  
      }  
    }, error = function(e) {  
      return(list(  
        status = FALSE,   
        message = "Error checking Ollama server ✗"  
      ))  
    })  
  }  
  
  # Function to get available Ollama models  
  get_ollama_models <- function() {
    tryCatch({
      # Use system command to list Ollama models
      models <- system2("ollama", "list", stdout = TRUE, stderr = TRUE)
      
      if (length(models) > 1) {
        # Parse model names, skipping the header line
        parsed_models <- sapply(models[-1], function(model_line) {
          # Split the line by whitespace and take the first element
          model_name <- strsplit(trimws(model_line), "\\s+")[[1]][1]
          return(model_name)
        })
        
        return(parsed_models)
      } else {
        return(c("No models found"))
      }
    }, error = function(e) {
      return(c("Error retrieving models"))
    })
  }
  
  # Reactive Ollama status  
  ollama_status <- reactive({  
    check_ollama_status()  
  })  
  
  # Output Ollama server status  
  output$ollamaStatus <- renderUI({  
    status <- ollama_status()  
    if (status$status) {  
      tags$span(  
        style = "color: green; font-weight: bold;",   
        status$message  
      )  
    } else {  
      tags$span(  
        style = "color: red; font-weight: bold;",   
        status$message  
      )  
    }  
  })  
  
  # Observe model selection changes  
  observeEvent(input$selectedOllamaModel, {  
    # Reset activation status when model changes  
    rv$model_activated <- FALSE    
    rv$model_test_status <- ""  
  })
  
  # Dynamic Model Selection  
  output$dynamicModelSelect <- renderUI({
    req(input$llmMethod == 'Local Models with Ollama')
    
    models <- get_ollama_models()
    
    selectInput(
      inputId = "selectedOllamaModel", 
      label = "Select Ollama Model", 
      choices = setNames(models, models),  # This ensures the displayed text matches the value
      selected = models[1]
    )
  })
  
  # Render the save/test button
  output$saveButtonOllama <- renderUI({
    actionButton(
      "saveOllamaSettings",
      if(rv$is_processing) "Processing..." else "Activate Model and Send Test Message",
      icon = if(rv$is_processing) icon("spinner", class = "fa-spin") else icon("save"),
      style = paste(
        "color: #fff;",
        "background-color:", if(rv$is_processing) "#6c757d" else "#337ab7", ";",
        "border-color:", if(rv$is_processing) "#6c757d" else "#2e6da4", ";"
      ),
      disabled = rv$is_processing
    )
  })
  
  # Save Ollama Settings      
  observeEvent(input$saveOllamaSettings, {    
    req(input$selectedOllamaModel)    
    
    # Set processing state first
    rv$is_processing <- TRUE
    
    Sys.sleep(0.1)
    
    tryCatch({    
      messages <- list(    
        list(    
          role = "user",    
          content = "Hello, can you understand and respond to this message?"    
        )    
      )    
      
      response <- chat(    
        model = input$selectedOllamaModel,    
        messages = messages,    
        output = "text"    
      )    
      
      # Update with actual response  
      if (!is.null(response) && nchar(response) > 0) {    
        rv$model_test_status <- paste("Response:", response)    
      } else {    
        rv$model_test_status <- "No response received from the model"    
      }    
      
    }, error = function(e) {    
      rv$model_test_status <- paste("Error:", conditionMessage(e))    
    }, finally = {  
      # Reset processing state
      rv$is_processing <- FALSE
    })    
  })
  
  # Render the status messages    
  output$modelTestStatus <- renderUI({    
    req(rv$model_test_status)  
    
    div(    
      style = "padding: 10px;     
           background-color: #f0f0f0;     
           border-radius: 5px;     
           margin-top: 10px;",    
      div(    
        tags$strong("Model Response:"),    
        tags$br(),    
        tags$span(  
          style = "color: #333;",   
          rv$model_test_status  
        )    
      )    
    )    
  })
  
  ## Analyze with Ollama ----
  analyze_with_ollama <- function(pdf_text, prompts, selected_model, progress_callback = NULL, context_window = contextWindow) {  
    responses <- list()  
    
    if (is.vector(pdf_text) && length(pdf_text) > 1) {
      pdf_text <- paste(pdf_text, collapse = "\n")
    }
    
    print(paste("Selected model:", selected_model))
    print(paste("Using context window size:", context_window))
    
    for (i in seq_along(prompts)) {  
      if (!is.null(progress_callback)) {  
        progress_callback(i, length(prompts))  
      }  
      
      prompt <- sprintf("Analyze this PDF text and answer ALL of the following prompts:

PDF TEXT:
%s

PROMPTS:
%s

IMPORTANT: For EACH prompt, you must respond in this format:
- PROMPT: [original prompt]
- ANSWER: [comprehensive answer]
- SOURCE: [exact supporting text from PDF, if no direct text, explain reasoning]
- PAGE: [page number where source appears]",  
                        pdf_text,  
                        prompts[i])  
      
      # Create request body with context_window
      request_body <- list(
        model = selected_model,
        prompt = as.character(prompt),
        stream = FALSE,
        options = list(
          num_ctx = context_window
        )
      )
      
      print("Request body structure:")
      print(str(request_body))
      
      api_response <- tryCatch({  
        print("Attempting API call...")
        
        response <- httr::POST(
          url = "http://localhost:11434/api/generate",
          body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
          encode = "json",
          httr::add_headers("Content-Type" = "application/json")
        )
        
        # Debug print the response
        print("Response status:")
        print(httr::status_code(response))
        
        if (httr::status_code(response) != 200) {
          error_content <- httr::content(response, "text")
          print("Error response content:")
          print(error_content)
          stop(paste("HTTP", httr::status_code(response), "-", error_content))
        }
        
        result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
        answer_text <- result$response
        
        # Parse the response to extract answer, source, and page  
        source_match <- regexpr("SOURCE:\\s*([^\\n]+)", answer_text)  
        page_match <- regexpr("PAGE:\\s*([^\\n]+)", answer_text)  
        
        answer <- if (source_match > 0) {  
          substr(answer_text, 1, source_match - 1)  
        } else {  
          answer_text  
        }  
        
        source <- if (source_match > 0) {  
          source_text <- regmatches(answer_text, source_match)  
          gsub("SOURCE:\\s*", "", source_text)  
        } else {  
          "Source not specified"  
        }  
        
        page <- if (page_match > 0) {  
          page_text <- regmatches(answer_text, page_match)  
          gsub("PAGE:\\s*", "", page_text)  
        } else {  
          "N/A"  
        }  
        
        list(  
          answer = trimws(answer),  
          source = trimws(source),  
          page = trimws(page)  
        )  
        
      }, error = function(e) {  
        print("Error details:")
        print(e$message)
        if (!is.null(e$call)) print(paste("Function call:", e$call))
        
        list(  
          answer = paste("Error:", e$message),  
          source = "Error occurred",  
          page = "N/A"  
        )  
      })  
      
      responses[[i]] <- api_response  
      Sys.sleep(1)
    }  
    
    return(responses)
  }
  
  
  
}
  # Run the Shiny app
shinyApp(ui = ui, server = server)