runApp <- function() {
  # List of required packages
  packages <- c("shiny", "shinydashboard", "shinyjs", "config", "pdftools", "httr", "jsonlite", "tidyverse", "waiter", "DT", "base64enc", "writexl", "readxl", "yaml")

  # Function to install and load packages
  install_and_load <- function(packages) {
    for (pkg in packages) {
      if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
      } else {
        library(pkg, character.only = TRUE)
      }
    }
  }
  
  # Install and load required packages
  install_and_load(packages)
  
  # Run the Shiny app from GitHub
  shiny::runGitHub(repo = "AIDE", username = "noah-schroeder", ref = "main")
}
