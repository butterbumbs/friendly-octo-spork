# Install packages you dont have

# install.packages("shiny")
# install.packages("bslib")

library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(haven)
library(survey)
library(srvyr)
library(scales)

# shiny is strict about working directories
# make sure you create a project, and place the files needed in the same folder


# Make sure data in same file
survey <- read_sav("IRG1961_CTM2512_W4_20251231_WT_SDA.sav")

#
source("map.R")

# User interface ----
ui <- page_sidebar(
    title = "Immigration",

    sidebar = sidebar(
        "Controls go here"
    ),

    card(
        leafletOutput("map")
    )
)

# Server logic ----
server <- function(input, output) {
    output$map <- renderLeaflet({
        make_map(cities, my.pal)
    })
}

# Run app ----
shinyApp(ui, server)
