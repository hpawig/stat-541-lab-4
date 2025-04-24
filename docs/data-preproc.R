# This R script reads in the large data and creates smaller data sets that will be called by the server



# Load packages:
packages <- c("tidyverse", "here", "readxl", "scales", "RColorBrewer", "leaflet",
              "sf", "rnaturalearth", "countrycode", "plotly", "janitor",
              "rnaturalearthdata", "readr", "kableExtra", "paletteer", "shiny")


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
## package loading chunk idea from: 
## https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them



# Load 2018 crosstab data
wgm18 <- read_excel(path = here("data", 
                                "wgm2018-dataset-crosstabs-all-countries.xlsx"),
                    sheet = "Crosstabs all countries",
                    skip = 2, 
                    col_names = TRUE) |> 
  fill(Question) |> 
  clean_names() |> 
  select(
    country:response, column_n_percent_4
  )

# create a csv of this dataframe
write_csv(wgm18, here("docs",
                      "data",
                      "wgm18.csv"))




# Load 2018 data dict
data_dict <- read_excel(path = here("data",
                                    "wgm2018-dataset-crosstabs-all-countries.xlsx"),
                        sheet = "Data dictionary",
                        skip = 0,
                        col_names = TRUE) |> 
  filter(
    `Variable Name` %in% c('WP5', 'Regions_Report')
  )

# create a csv of this dataframe
write_csv(data_dict, here("docs",
                          "data",
                      "wgm18_data_dict.csv"))


# Creating a tibble of 2 columns: country codes and country names
# Load 2018 country codes
country_w_codes <- data_dict |> 
  filter(`Variable Name` == "WP5") |> 
  mutate(
    country_code = str_split(
      string = `Variable Type & Codes*`,
      pattern = ",",
      n = length(unique(full_df_2018$WP5)), 
      simplify = TRUE
    )
    
  ) |> 
  janitor::clean_names() |> 
  # drop first few columns; only need country name and code
  select(-variable_type_codes, -variable_name, -variable_type_codes,
         -variable_label_survey_question, -notes) |> 
  unlist() |> # turns list into a column
  as_tibble() |> 
  rename(country = value) |> 
  mutate(
    code = str_split(country, "=", n = 2, simplify = TRUE)[, 1],
    country = str_split(country, "=", n = 2, simplify = TRUE)[, 2],
    country = str_remove(country, ","),
    code = as.numeric(code)
  )


write_csv(country_w_codes, here("docs", "data",
                              "country_w_codes.csv"))



# create tibble with two columns: Region code and region name
regions_codes <- data_dict |> 
  filter(`Variable Name` == "Regions_Report") |> 
  mutate(
    country_code = str_split(
      `Variable Type & Codes*`, 
      ",", 
      n = length(unique(full_df_2018$Regions_Report)), 
      simplify = TRUE)
    
  ) |> 
  
  janitor::clean_names() |> 
  # remove unnecessary columns
  select(-variable_type_codes, -variable_name, -variable_type_codes,
         -variable_label_survey_question, -notes) |> 
  unlist() |> 
  as_tibble() |> 
  # Rename column
  rename(region = value) |> 
  # Separate region name and region code into two columns.
  mutate(
    code = str_split(region, "=", n = 2, simplify = TRUE)[, 1],
    region = str_split(region, "=", n = 2, simplify = TRUE)[, 2],
    region = str_remove(region, ","),
    code = as.numeric(code)
  )


# create a csv of this dataframe
write_csv(regions_codes, here("docs","data",
                       "regions_codes.csv"))


# Read in full 2018 dataset then create df with country and assigned region
country_region <- read_excel(path = here ("data",
                                        "wgm2018-dataset-crosstabs-all-countries.xlsx"),
                           sheet = "Full dataset",
                           skip = 0,
                           col_names = TRUE) |> 
  select(WP5, Regions_Report) |> 
  distinct() |> 
  left_join(country_w_codes, by = c("WP5" = "code")) |> 
  left_join(regions_codes, by = c("Regions_Report" = "code")) |> 
  select(country, region) |> 
  # replace republic of congo and palestine to match Crosstab country list
  mutate(
    country = case_when(
      str_detect(country, "Palestinian") ~ "Palestine",
      country == "Republic of Congo" ~ "Congo, Rep.",
      TRUE ~ country
    ))

# create a country_region csv
write_csv(country_region, here("docs",
                               "data",
                              "country_region.csv"))
    
    



wgm20 <- read_excel(path = here("data",
                                "wgm2020-mentalhealthmodule-crossnational-tabs.xlsx"),
                    sheet = "Socio-economic & demographic",
                    skip = 2, 
                    col_names = TRUE) |> 
  janitor::clean_names() |> 
  # keep only columns of interest
  select(wellcome_tag, question, x3, column_n_percent_4)


# Write into a csv
write_csv(wgm20, here("docs",
                      "data",
                      "wgm20.csv"))







