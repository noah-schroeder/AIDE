#' @keywords internal  
#' @import utils
.onLoad <- function(libname, pkgname) {
  # Use R_user_dir for proper user data directory
  config_dir <- tools::R_user_dir("AIDE", which = "config")
  config_file <- file.path(config_dir, "config.yml")
  
  # Create directory if it doesn't exist
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
  }
  
  # Only create new config file if one doesn't exist
  if (!file.exists(config_file)) {
    template <- system.file("app/config.yml.template", package = "AIDE")
    if (template == "") {
      stop("Template configuration file not found in package")
    }
    file.copy(template, config_file)
    message("Created new configuration file at: ", config_file)
  }
}