# This R script reads in the large data and creates smaller data sets that will be called by the server



# Load packages:
packages <- c("tidyverse", "here", "readxl", "scales", "RColorBrewer", "leaflet",
              "sf", "rnaturalearth", "countrycode", "plotly", "janitor",
              "rnaturalearthdata", "readr", "kableExtra", "paletteer", "shiny")


# Install packages not yet installed
installed_packages <- packages %in% rownames(in