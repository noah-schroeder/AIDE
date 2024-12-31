# AI-Assisted Data Extraction (AIDE)
This app is designed to facilitate human-in-the-loop data extraction from PDFs using large language models (LLMs). Currently (Dec. 2024) the LLMs available are Google Gemini models, Mistral models, and models available through Open Router. This app is focused on using LLMs that offer free API keys. 

## Quick Start: To Run AIDE -
### Simple Version (no code exposure)
To run AIDE on your local machine without having to see all of the source code: 
1) Open Run_AIDE.R in R Studio
2) Highlight all text and click 'run'. This should install all dependencies and run the latest version.

### Source Version (code exposure)
To run AIDE on your local machine: 
1) Open app.R in R Studio
2) Click 'run'. This should install all dependencies and run the version you have open. This will also allow you to edit any source code (e.g., system prompts or settings) if you choose to.

## To use this app, you'll need API keys for the LLMs built into it. You can get free* API keys with the links below - 
1) Google Gemini: https://aistudio.google.com/ - Free tier rate limits available here: https://ai.google.dev/pricing
2) Mistral: https://mistral.ai/ - Free tier rate limits available here: https://help.mistral.ai/en/articles/225174-what-are-the-limits-of-the-free-tier 
3) Open Router: https://openrouter.ai/ - Note that not all models on Open Router are free.

*All the APIs listed above have a free tier as of December 2024, as well as a paid tier. If you're intending to use the free tier, be sure that is what you sign up for and use models that are compatable with the free tier.

## New To LLMs or API Keys?
If you're new to API keys, this all may seem intimidating. Long story short, an API key is what lets you access a remote service (in this case, an LLM). There are free and paid versions. If you're new, I recommend using Google or Mistral, because I feel they make it easy to ensure you are staying with a free tier.

## What LLM Should I Use? 
Totally up to you within the bounds of the App. We have had excellent results using Gemini and Mistral Models. The free models from Open Router available to us during testing (December, 2024) did not have large enough context windows to be particularly useful for us. 

## Keep Your Config File & API Keys Secure
You should keep your API key secure. Yes, the config file stores the API keys as plain text. Accordingly, you should NOT put your config file on Github or any other publicly accessible location. The gitignore file with this package is set to ignore .yml files, however if you clone/copy etc. this repo, you should ensure your config file stays ignored so your API key is not exposed. 

## Do NOT Put This App Live As a Public-Facing Web Application Without Modifications
The structure of the current code is not ideal for launching AIDE as a public-facing web application (e.g., on Shinyapps.io) due to the config file structure - it will not keep your API key secure or private. As such, I <strong>strongly recommend not launching AIDE as a public-facing web app without appropriate changes to the source code to secure API keys.</strong> I may update this later, but for now my personal opinion is that you should run AIDE on your local machine rather than a public-facing web-based application. 
