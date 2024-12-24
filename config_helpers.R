# Function to write the config file  
write_config <- function(current_config, file_path = "config.yml") {  
  # 1. Check if the file exists  
  if (!file.exists(file_path)) {  
    # 2. Create default content if the file doesn't exist  
    new_content <- paste0(  
      "api:\n",  
      "  gemini:\n",  
      "    base_url: \"", "https://generativelanguage.googleapis.com/v1beta", "\"\n",  
      "    model: \"", current_config$api$gemini$model, "\"\n",  
      "    api_key: \"", current_config$api$gemini$api_key, "\"\n",  
      "    rate_limits:\n",  
      "      requests_per_minute: ", current_config$api$gemini$rate_limits$requests_per_minute, "\n",  
      "      requests_per_day: ", current_config$api$gemini$rate_limits$requests_per_day, "\n",  
      "  mistral:\n",  
      "    base_url: \"", "https://api.mistral.ai/v1", "\"\n",  
      "    model: \"", current_config$api$mistral$model, "\"\n",  
      "    api_key: \"", current_config$api$mistral$api_key, "\"\n",  
      "    rate_limits:\n",  
      "      requests_per_minute: ", current_config$api$mistral$rate_limits$requests_per_minute, "\n",  
      "\n" # Single newline at the end  
    )  
    writeLines(new_content, file_path)  
    return() # Exit the function after creating the file  
  }  
  
  # 3. Create new content  
  new_content <- paste0(  
    "api:\n",  
    "  gemini:\n",  
    "    base_url: \"", "https://generativelanguage.googleapis.com/v1beta", "\"\n",  
    "    model: \"", current_config$api$gemini$model, "\"\n",  
    "    api_key: \"", current_config$api$gemini$api_key, "\"\n",  
    "    rate_limits:\n",  
    "      requests_per_minute: ", current_config$api$gemini$rate_limits$requests_per_minute, "\n",  
    "      requests_per_day: ", current_config$api$gemini$rate_limits$requests_per_day, "\n",  
    "  mistral:\n",  
    "    base_url: \"", "https://api.mistral.ai/v1", "\"\n",  
    "    model: \"", current_config$api$mistral$model, "\"\n",  
    "    api_key: \"", current_config$api$mistral$api_key, "\"\n",  
    "    rate_limits:\n",  
    "      requests_per_minute: ", current_config$api$mistral$rate_limits$requests_per_minute, "\n",  
    "\n" # Single newline at the end  
  )  
  
  # 4. Write the new content, overwriting the file  
  writeLines(new_content, file_path)  
}