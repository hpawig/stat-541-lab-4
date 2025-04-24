# This script holds all the customized functions needed for the UI/Server for my
# Lab 4 shiny app



# Function 1
# This function takes in user input 'question_of_interest' and determines what question to focus on 
find_mapQ <- function(input) {
  question_pattern = ""
  if (input == "Interest in Science") {
    question_pattern = "Q8"
  } else if (input == "Benefit of Science") {
    question_pattern = "Q17"
  } else if (input == "Hope in Scientists") {
    question_pattern = "Q18"
  } else {
    # default is Q9
    question_pattern = "Q9"
  }
  return(question_pattern)
}

# Function 2
# Function takes in question pattern and creates map title.
map_title_maker <- function(question_pattern) {
  if (question_pattern == "Q8") {
    map_title = "<div style='font-size: 16px; font-weight: bold; margin: 5px;'>
      Percentage of People that are interested in science</div>
      \n   (said 'Yes' on Question 8 in Gallup Poll 2018)"
  } else if (question_pattern == "Q9") {
    map_title = "<div style='font-size: 16px; font-weight: bold; margin: 5px;'>
      Percentage of People that are interested in health, disease, or medicine</div>
      \n   (said 'Yes' on Question 9 in Gallup Poll 2018)"
  } else if (question_pattern == "Q17") {
    map_title = "<div style='font-size: 16px; font-weight: bold; margin: 5px;'>
      Percentage of People that say science benefits people like themselves in their country</div>
      \n   (said 'Yes' on Question 17 in Gallup Poll 2018)"
  } else if (question_pattern == "Q18") {
    map_title = "<div style='font-size: 16px; font-weight: bold; margin: 5px;'>
      Percentage of People that agree science and technology will improve life for next generatiions</div>
      \n   (said 'Yes' on Question 18 in Gallup Poll 2018)"
  }
  return (map_title)
  
}



# Function 3
## CITATION (for adding n to boxplot):
## https://waterdata.usgs.gov/blog/boxplots/

# Run this custom function to get continent sizes
# Takes in x, the group of interest
# Returns a dataframe of:
# The Label: group size, found by calculating the length of the vector containing
# The y-coordinate: to position the label next to the corresponding boxplot
n_fun <- function(x){
  return(data.frame(y = 1.1,
                    label = paste0("n = ", length(x))))