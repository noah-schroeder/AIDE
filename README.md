# AI-Assisted Data Extraction (AIDE)
This app is designed to facilitate human-in-the-loop data extraction from PDFs using large language models (LLMs). Currently (Dec. 2024) the LLMs available are Google Gemini models, Local models via Ollama, Mistral models, and models available through Open Router. 

## Quick Start: To Run AIDE -
### Simple Package Installation
Install R Package: 
```r
#Install package from Github Repo
devtools::install_github("noah-schroeder/AIDE", dependencies = TRUE)

#Load package
library(AIDE)

#Run App
runAIDE()
```

## To use this app, you can use local models using Ollama or you'll need API keys for the LLMs built into it. You can get free* API keys with the links below - 
1) Google Gemini: https://aistudio.google.com/ - Free tier rate limits available here: https://ai.google.dev/pricing
2) Mistral: https://mistral.ai/ - Free tier rate limits available here: https://help.mistral.ai/en/articles/225174-what-are-the-limits-of-the-free-tier 
3) Open Router: https://openrouter.ai/ - Note that not all models on Open Router are free.

*All the APIs listed above have a free tier as of December 2024, as well as a paid tier. If you're intending to use the free tier, be sure that is what you sign up for and use models that are compatable with the free tier.

## New To LLMs or API Keys?
If you're new to API keys, this all may seem intimidating. Long story short, an API key is what lets you access a remote service (in this case, an LLM). There are free and paid versions. If you're new, I recommend using Google or Mistral, because I feel they make it easy to ensure you are staying with a free tier.

## What LLM Should I Use? 
Totally up to you within the bounds of the App. We have had excellent results using Gemini and Mistral Models. The free models from Open Router available to us during testing (December, 2024) did not have large enough context windows to be particularly useful for us. 

## Keep Your API Keys Secure
You should keep your API key secure. The config file stores the API keys as plain text in your local user account (i.e., on your computer). Someone else on the same computer using a different sign on should not have access to your API key with this structure. Never put your config file on Github or any other publicly accessible location. 

## Do NOT Put This App Live As a Public-Facing Web Application Without Modifications
The structure of the current code is not ideal for launching AIDE as a public-facing web application (e.g., on Shinyapps.io) due to the config file structure - it will not keep your API key secure or private. As such, I <strong>strongly recommend not launching AIDE as a public-facing web app (like shinyapps.io) without appropriate changes to the source code to secure API keys.</strong> I may update this later, but for now my personal opinion is that you should run AIDE on your local machine rather than a public-facing web-based application. 
