# load necessary libraries
library("tidycensus")
library("tidyverse")
library("survey")
library("srvyr")

# pull ACS PUMS data
pums_data <- get_pums(
    year = 2021,
    variables = c("AGEP", "SCHL", "ESR", "SOCP", "HISP", "RAC1P", "CIT"),
    state = "OH",
    puma = c(params$PUMA1, params$PUMA2,
        params$PUMA3, params$PUMA4), # in YAML params$PUMAs = c("00801","00802")
    survey = "acs5",
    recode = TRUE,
    rep_weights = "person",
    show_call = TRUE
)

# format ACS PUMS as survey data
pums_survey_data <- pums_data %>%
    to_survey(
        type = "person",
        design = "rep_weights"
    )
