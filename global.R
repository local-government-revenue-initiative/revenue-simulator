# global.R
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(readr)
library(fastDummies)  # For one-hot encoding

# Set options
options(shiny.maxRequestSize = 30*1024^2)  # 30MB max file size

# Source all module functions
source("R/module1_functions.R")

# Source UI and server modules
source("modules/module1_ui.R")
source("modules/module1_server.R")