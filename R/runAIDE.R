#' Launch AIDE 
#'  
#' @export  
#' @importFrom shiny runApp  
#' @import tidyverse  
#' @import shinydashboard  
#' @import shinyjs  
#' @importFrom config get  
#' @importFrom httr GET POST  
#' @importFrom jsonlite fromJSON toJSON  
#' @importFrom pdftools pdf_text  
#' @importFrom shiny runApp
#'  
runAIDE <- function() {  
  appDir <- system.file("app", package = "AIDE")  
  if (appDir == "") {  
    stop("Could not find app directory. Try re-installing `AIDE`.", call. = FALSE)  
  }  
  
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)  
}