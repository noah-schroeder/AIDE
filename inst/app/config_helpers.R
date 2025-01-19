#' Get the path to the configuration file
#' @return Path to the configuration file
#' @export
get_config_path <- function() {
  config_dir <- tools::R_user_dir("AIDE", which = "config")
  file.path(config_dir, "config.yml")
}

#' Read configuration settings
#' @return List containing configuration settings
#' @export
read_config <- function() {
  config_path <- get_config_path()
  if (!file.exists(config_path)) {
    stop("Configuration file not found.")
  }
  yaml::read_yaml(config_path)
}

#' Write configuration settings
#' @param config List containing configuration settings
#' @keywords internal
write_config <- function(config) {
  config_path <- get_config_path()
  dir.create(dirname(config_path), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(config, config_path)
  
  # Set restrictive file permissions on Unix-like systems
  if (.Platform$OS.type == "unix") {
    Sys.chmod(config_path, mode = "0600")
  }
}

#' Setup initial configuration
#' @export
setup_config <- function() {
  message("Creating configuration file. Never share or commit this file as it will contain sensitive API keys.")
  config_path <- get_config_path()
  
  if (!file.exists(config_path)) {
    write_config(get_default_config())
    message("Configuration file created at: ", config_path)
  } else {
    message("Configuration file already exists at: ", config_path)
  }
}

#' Get default configuration
#' @keywords internal
get_default_config <- function() {
  list(
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