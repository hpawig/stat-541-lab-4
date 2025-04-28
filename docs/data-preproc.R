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



wd_q25<- wgm18 |>
  janitor::clean_names() |> 
  select(
    country:response, column_n_percent_4
  ) |> 
  filter(
    response %in% c("Strongly agree", "Somewhat agree")
  ) |> 
  filter(
    question == "Q25 Do you strongly or somewhat agree, strongly or somewhat disagree or neither agree nor disagree with the following statement? Vaccines are safe."
  )

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


write_csv(country_w_codes, here("docs", 
                                #"data",
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
write_csv(regions_codes, here("docs",
                              #"data",
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
    
    

# Load 2018 wgm filtered by Question 25

# Filter for the question of interest
# This filters WGM 2018 data for Question 25 responses regarding Vaccines
# This data frame is for the box plot

plot_df <- wd_q25|>
  left_join(country_region, by = "country") |> 
  # create new regions
  mutate(
    continent = case_when(
      str_detect(region, "Asia") ~ "Asia",
      str_detect(region, "America") ~ "Americas",
      str_detect(region, "Europe") ~ "Europe",
      str_detect(region, "Africa") ~ "Africa",
      region == "Middle East" ~ "Middle East and North Africa",
      region == "Aus/NZ" ~ "Oceania",
      TRUE ~ "Not Assigned"
    )
  ) 

plot_df <- plot_df |>
  # calculate percentage of vaccine agree %s by country
  group_by(country) |>
  mutate(
    percentage = sum(column_n_percent_4, na.rm = TRUE)
  ) |>
  ungroup() |>
  
  # calculate median percentage of vaccine agree %s by region
  group_by(continent) |>
  mutate(
    median_percentage = median(percentage, na.rm = TRUE)
  ) |>
  ungroup() |>
  
  # only keep one row for each country (remove dupes)
  filter(response != "Somewhat agree") |> 
  select(country, region, percentage, median_percentage, continent) |>
  # ordering of region and country
  mutate(
    country = fct_reorder(country, percentage)
  ) |> 
  filter(continent != "Not Assigned")


# save as a csv file
write_csv(plot_df, here("docs",
                        "data",
                        "plot_df.csv"))




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







##Vaccine Page Dataframes 

# Create a data frame with the percentage of people that agree vaccines are safe worldwide
worldwide_q6_8 <- wgm18 |>
  select(
    country:response, column_n_percent_4
  ) |> 
  # copy question to fill NAs
  fill(question) |> 
  filter(
    str_detect(question, pattern = "Q6") |
      str_detect(question, pattern = "Q8"),
    response == "Yes"
  ) |> 
  mutate(
    percentage = column_n_percent_4
  ) |> 
  group_by(question, response) |> 
  summarise(
    percentage = round(mean(percentage, na.rm = TRUE), 2),
    region = "World",
    country = "World"
  ) |>
  ungroup() |> 
  mutate(
    question = 
      case_when(
        str_detect(question, pattern = "Q8") ~ "Q8",
        str_detect(question, pattern = "Q6") ~ "Q6"
        
      )) |> 
  select(-response) |> 
  # pivot
  pivot_wider(
    names_from = question,
    values_from = percentage
  )


q6_8_data <- wgm18 |>
  select(
    country:response, column_n_percent_4
  ) |> 
  fill(question) |> 
  filter(
    str_detect(question, pattern = "Q6") |
      str_detect(question, pattern = "Q8"),
    response == "Yes"
  ) |> 
  mutate(
    percentage = column_n_percent_4
  ) |> 
  select(country, question, percentage, response)  |> 
  left_join(country_region, by = "country") |> 
  mutate(
    region = ifelse(
      region == "Aus/NZ", "Australia/New Zealand", region),
    question = case_when(
      str_detect(question, pattern = "Q6") ~ "Q6",
      str_detect(question, pattern = "Q8") ~ "Q8"
    )
  ) |>
  group_by(country, region, question, response) |> 
  summarize(
    percentage = round(mean(percentage, na.rm = TRUE), 2)
  ) |> 
  ungroup() |> 
  select(-response) |> 
  # pivot
  pivot_wider(
    names_from = question,
    values_from = percentage
  )


# join with worldwide summary data
q6_8_data <- q6_8_data |> 
  bind_rows(worldwide_q6_8) |> 
  # create a Boolean for the world observation
  mutate(isWorld = ifelse(country == "World", TRUE, FALSE),
         # mutate proportions so that percentage tooltip labels show as whole #s
         Q6 = 100*Q6,
         Q8 = 100*Q8)

# Save all data for questions 6-8 into a csv
write_csv(q6_8_data, here("docs",
                              "data",
                              "wgm18_q6_8.csv"))

# 2018 medians for question 6 and question 8 

medians_science <- q6_8_data |> 
  filter(region != "World") |>
  summarize(
    Q6 = median(Q6, na.rm = TRUE),
    Q8 = median(Q8, na.rm = TRUE)
  ) |> as_tibble(
    rownames = "median",
    colnames = c("Q6", "Q8")
  )

# save into a csv
write_csv(medians_science, here("docs",
                              "data",
                              "wgm18_q6_8_medians.csv"))



# Create a data frame with the WORLDWIDE percentages of:
# People who have sought information about Health, Medicine, and Disease (Q7)
# People who say they're interested in Health, Medicine, and Disease (Q9)

worldwide_health_interest <- wgm18 |>
  select(
    country:response, column_n_percent_4
  ) |> 
  # copy question to fill NAs
  fill(question) |> 
  filter(
    str_detect(question, pattern = "Q7") |
      str_detect(question, pattern = "Q9"),
    response == "Yes"
  ) |> 
  mutate(
    percentage = column_n_percent_4
  ) |> 
  group_by(question, response) |> 
  summarise(
    percentage = round(mean(percentage, na.rm = TRUE), 2),
    region = "World",
    country = "World"
  ) |>
  ungroup() |> 
  mutate(
    question = 
      case_when(
        str_detect(question, pattern = "Q7") ~ "Q7",
        str_detect(question, pattern = "Q9") ~ "Q9"
        
      )) |> 
  select(-response) |> 
  # pivot
  pivot_wider(
    names_from = question,
    values_from = percentage
  )



##       Health, Disease and Medicine Interest Dataframes       ##


# Create a data frame with the country percentages of:
# People who have sought information about Health, Medicine, and Disease (Q7)
# People who say they're interested in Health, Medicine, and Disease (Q9) 

health_interest <- wgm18 |>
  select(
    country:response, column_n_percent_4
  ) |> 
  fill(question) |> 
  filter(
    str_detect(question, pattern = "Q7") |
      str_detect(question, pattern = "Q9"),
    response == "Yes"
  ) |> 
  mutate(
    percentage = column_n_percent_4
  ) |> 
  select(country, question, percentage, response)  |> 
  left_join(country_region, by = "country") |> 
  mutate(
    region = ifelse(
      region == "Aus/NZ", "Australia/New Zealand", region),
    question = case_when(
      str_detect(question, pattern = "Q7") ~ "Q7",
      str_detect(question, pattern = "Q9") ~ "Q9"
    )
  ) |>
  group_by(country, region, question, response) |> 
  summarize(
    percentage = round(mean(percentage, na.rm = TRUE), 2)
  ) |> 
  ungroup() |> 
  select(-response) |> 
  # pivot
  pivot_wider(
    names_from = question,
    values_from = percentage
  )


# join country percentages with worldwide percentage rows
health_interest <- health_interest |> 
  bind_rows(worldwide_health_interest) |> 
  mutate(isWorld = ifelse(country == "World", TRUE, FALSE),
                                              Q7 = 100*Q7 ,
                                              Q9 = 100*Q9 )



map_df <- wgm18 |>
  select(
    country:response, column_n_percent_4
  ) |> 
  # only filter "Yes" responses because questions of interest are Y/N questions
  filter(response == 'Yes') |> 
  rename(
    percentage_yes = column_n_percent_4
  ) |> 
  mutate(
    percentage_yes = 100*round(percentage_yes, 2)
  ) |> 
  select(country, question, percentage_yes) |> 
  # creating country 3-digit code column
  # with the help of the countrycode package, which I asked Gemini about
  mutate(code = countrycode(country,
                            origin = "country.name",
                            destination = "iso3c"), # want 3-letter code
         code = case_when(
           country == "Kosovo" ~ "XKX",
           TRUE ~ code
         )) 


write_csv(map_df, here("docs",
                       "data",
                       "wgm18_map.csv"))



# Calculate Medians for Health, Medicine, and Disease percentages
medians_hmd <- health_interest |> 
  filter(region != "World") |>
  summarize(
    Q7 = median(Q7, na.rm = TRUE),
    Q9 = median(Q9, na.rm = TRUE)
  ) |> as_tibble(
    rownames = "median",
    colnames = c("Q7", "Q9")
  )

# Save dataframes into a csv
write_csv(health_interest, file = here("docs",
                              "data",
                              "wgm18_q7_9.csv"))

write_csv(medians_hmd, here("docs",
                              "data",
                              "wgm18_q7_9_medians.csv"))


##                Mental Health Page Dataframes                ##


# SOCIAL MEDIA

social_media <- wgm20 |> 
  filter(
    str_detect(question, pattern = "Social")
  )



# TABLE FOR SOCIAL MEDIA
# Create table showing opinion of social media
soc_media_stats <- social_media |> 
  rename(
    Question = question,
    Percentage = column_n_percent_4,
    Response = x3
  ) |> 
  # Keep rows with responses of interest
  filter(
    Response %in% c('Yes',"Almost every hour","Several times a day","Once a day",
                    "A few days a week","All of the time", "Most of the time","Some of the time")
  ) |> 
  # Create new variable: Topic (for displaying purposes)
  mutate(Topic = case_when(
    wellcome_tag == "W27" ~ "Said They Used Social Media in the last 30 days",
    wellcome_tag == "W28" ~ "Said They Used Social Media at least a few times per week",
    wellcome_tag == "W29" ~ "Said They Saw Health Information on Social Media at least some of the time"
  )) |> 
  group_by(Topic) |> 
  summarize(
    Percentage = round(mean(Percentage, na.rm = TRUE), 3)
  ) |> 
  ungroup() |> 
  mutate(
    Percentage = percent_format(
      accuracy = 0.1,
      scale = 100
    )(Percentage)
  ) |> as_tibble() 
# |> kbl(caption = "Social Media") |> 
#   kable_classic(html_font = "Cambria")


write_csv(soc_media_stats, here("docs",
                              "data",
                              "wgm20_social_media.csv"))

# ANXIETY

anxiety_stats <- wgm20 |> 
  filter(
    str_detect(question, pattern = "Anx")
  ) |> 
  rename(
    Question = question,
    Percentage = column_n_percent_4,
    Response = x3
  ) |> 
  # Only keep rows with responses of interest
  filter(
    Response %in% c("A lot", "Some", 
                    "Extremely important", "Somewhat important",
                    "Very comfortable", "Somewhat comfortable",
                    "Very helpful", "Somewhat helpful"))  |> 
  # Create new variable: Topic (for displaying purposes)
  mutate(
    Topic = case_when(
      wellcome_tag == "MH3B" ~ "Believed Science Can Help Anxiety and Depression",
      wellcome_tag == "MH4B" ~ "Believed Government Funded Research in Anxiety and Depression is Important",
      wellcome_tag == "MH5" ~ "Felt Comfortable Talking about Mental Health",
      wellcome_tag == "MH9A" ~ "Agree Mental Health Professionals Can Help With Anxiety and Depression",
      wellcome_tag == "MH9B" ~ "Agree Spiritual/Religious Activities Can Help With Anxiety and Depression",
      wellcome_tag == "MH9C" ~ "Agree Conversations with Family and Friends Can Help With Anxiety and Depression",
      wellcome_tag == "MH9D" ~ "Agree Taking Prescription Medication Can Help With Anxiety and Depression",
      wellcome_tag == "MH9E" ~ "Agree Exercise Can Help With Anxiety and Depression",
      wellcome_tag == "MH9F" ~ "Agree Changing Work Situation Can Help With Anxiety and Depression",
      wellcome_tag == "MH9G" ~ "Agree Changing Personal Relationships Can Help With Anxiety and Depression",
      wellcome_tag == "MH9H" ~ "Agree Time Outdoors Can Help With Anxiety and Depression"
    ) 
  ) |> 
  group_by(Topic) |> 
  summarize(
    Percentage = round(mean(Percentage, na.rm = TRUE), 3)
  ) |> 
  mutate(
    Percentage = percent_format(
      accuracy = 0.1,
      scale = 100
    )(Percentage)
  )
# kbl(caption = "Anxiety") |> 
# kable_classic(html_font = "Cambria")


# write anxiety table as a CSV
write_csv(anxiety_stats, here("docs",
                              "data",
                              "wgm20_anxiety.csv"))





# MENTAL HEALTH
# filter all data and only retain mental health questions
mh_stat <- wgm20 |> 
  filter(
    wellcome_tag == "MH1"
  ) |> 
  rename(
    Question = question,
    Percentage = column_n_percent_4,
    Response = x3
  ) |>
  filter(
    Response %in% c("More important", "As important")
  ) |>
  summarize(
    Percentage = round(mean(Percentage, na.rm = TRUE), 3)
  ) |>
  mutate(
    Percentage = percent_format(
      accuracy = 0.1,
      scale = 100
    )(Percentage)
  )


write_csv(mh_stat, here("docs",
                              "data",
                              "wgm20_mental_health.csv"))


# Comparing 2018 and 2020 dataframes

wgm18_Q21 <- wgm18 |> 
  select(
    country:response, column_n_percent_4
  ) |> 
  filter(
    response %in% c("Some", "A lot"),
    str_detect(question, pattern = "Q21") | str_detect(question, pattern = "Q11B")
  ) |> 
  group_by(question) |> 
  summarize(
    percentage = round(mean(column_n_percent_4, na.rm = TRUE), 3)
  ) |> 
  mutate(
    percentage = percent_format(
      accuracy = 0.1,
      scale = 100
    )(percentage),
    question = case_when(
      str_detect(question, pattern = "Q21") ~ "Trust in government health and medical advice",
      str_detect(question, pattern = "Q11B") ~ "Level of trust in national government"
    )
  ) |> 
  rename(`Question Topic` = question,
         Percentage = percentage)

  

# Save into a csv
write_csv(wgm18_Q21,
          here("docs",
              "data",
              "wgm18_Q21.csv"))

# 2020 data on opinion about national government funding research on cancer and/or anxiety


wgm20_govt <- wgm20 |> 
  filter(
    wellcome_tag %in% c("MH4A", "MH4B"),
    x3 %in% c("Extremely important", "Somewhat important")
  ) |> 
  group_by(question) |> 
  summarize(
    percentage = round(mean(column_n_percent_4, na.rm = TRUE), 3)
  ) |> 
  mutate(
    percentage = percent_format(
      accuracy = 0.1,
      scale = 100
    )(percentage)
  )  |> 
  rename(Question = question,
         Percentage = percentage) 

# save into a csv

write_csv(wgm20_govt,
          here("docs",
              "data",
              "wgm20_govt.csv"))
