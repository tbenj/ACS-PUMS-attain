# load necessary libraries
library("tidycensus")
library("tidyverse")
library("survey")
library("srvyr")
library("knitr")

# pull ACS PUMS data
pums_data <- get_pums(
    year = 2021,
    variables = c("AGEP", "SCHL", "ESR", "SOCP", "HISP", "RAC1P", "CIT"),
    state = "OH",
    puma = c("00801","00802"), #c(params$PUMA1, params$PUMA2,
        #params$PUMA3, params$PUMA4), # in YAML params$PUMAs = c("00801","00802")
    survey = "acs5",
    recode = TRUE,
    rep_weights = "person",
    show_call = TRUE
)

# format ACS PUMS as survey data and create derived variables
pums_survey_data <- pums_data %>%
    to_survey(
        type = "person",
        design = "rep_weights"
    ) %>%
    mutate(
        degree = case_when(
            SCHL >= 22 ~ "Graduate degree",
            SCHL >= 21 ~ "Bachelor's degree",
            SCHL == 20 ~ "Associate's degree",
            SCHL >= 18 ~ "Some college, no degree",
            SCHL >= 16 ~ "High school graduate",
            TRUE ~ "Less than high school"
        ),
        degree = factor(degree, c(
                "Less than high school",
                "High school graduate",
                "Some college, no degree",
                "Associate's degree",
                "Bachelor's degree",
                "Graduate degree"
            )
        ),
        has_degree = SCHL >= 20,
        age_gr = case_when(
            AGEP >= 65 ~ "65+",
            AGEP >= 55 ~ "55-64",
            AGEP >= 45 ~ "45-54",
            AGEP >= 35 ~ "35-44",
            AGEP >= 25 ~ "25-34",
            AGEP >= 18 ~ "18-24",
            TRUE ~ "0-17"
        ),
        race_ethn = case_when(
            CIT == "5" ~ "U.S. Nonresident",
            HISP != "01" ~ "Hispanic or Latino",
            RAC1P == "1" ~ "White",
            RAC1P == "2" ~ "Black or African American",
            RAC1P == "9" ~ "Two or more races",
            RAC1P == "6" ~ "Asian",
            RAC1P == "8" ~ "Unknown / some other race",
            RAC1P == "3" | RAC1P == "5" ~ "American Indian or Alaska Native",
            RAC1P == "7" ~ "Native Hawaiian or Pacific Islander",
            TRUE ~ RAC1P
        )
    )

# if export is desired
# write.csv(pums_survey_data, "test.csv")

# table of pulled data from PUMS
pums_survey_data %>%
    group_by(PUMA) %>%
    summarize(
        records = n(),
        weighted = survey_total(vartype = c("ci"), level = 0.95)
    ) %>%
    bind_rows(
        summarize(., across(where(is.numeric), sum),
            across(where(is.character), ~ 'Total')
            )
        ) %>%
    kable(digits = 0, format.args = list(big.mark = ","))


pums_survey_prime_age <- pums_survey_data %>%
    filter(AGEP >= 25 & AGEP < 65) %>%
    group_by(age_gr, degree) %>%
    summarize(
        pct = survey_mean(proportion = TRUE, vartype = c("ci"), level = 0.95),
        n = survey_total(vartype = c("ci"), level = 0.95)
        )


pums_survey_prime_age %>%
    arrange(age_gr, rev(degree)) %>%
    mutate(label_y = cumsum(n)/sum(n)) %>%
    ggplot(aes(fill = degree, x = age_gr, y = pct)) +
        geom_bar(position = "fill", stat = "identity", color = "black") +
        scale_fill_brewer(palette = "Blues") +
        geom_text(aes(y = label_y, label = scales::percent(label_y, accuracy = .1), vjust = 1.5)) +
        scale_y_continuous(name = "population educational attainment rate (cumulative)", labels = scales::percent) +
        labs(x = "age group", fill = "educational attainment")

pums_survey_prime_age %>%
    arrange(age_gr, rev(degree)) %>%
    mutate(label_y = cumsum(n) - .5*n) %>%
    ggplot(aes(fill = degree, x = age_gr, y = n)) +
        geom_bar(position = "stack", stat = "identity", color = "black") +
        scale_fill_brewer(palette = "Blues") +
        geom_text(aes(y = label_y, label = scales::comma(n))) +
        scale_y_continuous(name = "population", labels = scales::comma) +
        labs(x = "age group", fill = "educational attainment")

pums_survey_PUMAs <- pums_survey_data %>%
    filter(AGEP >= 25 & AGEP < 65) %>%
    group_by(PUMA, has_degree) %>%
    summarize(
        pct = survey_mean(proportion = TRUE, vartype = c("ci"), level = 0.95),
        n = survey_total(vartype = c("ci"), level = 0.95)
        )

pums_survey_PUMAs %>%
    filter(has_degree == TRUE) %>%
    ggplot(aes(fill = has_degree, x = PUMA, y = pct)) +
        geom_bar(position = "stack", stat = "identity", color = "black") +
        geom_errorbar(aes(ymin = pct_low, ymax = pct_upp, width = 0.2)) +
        scale_fill_brewer(palette = "Blues", guide = "none") +
        geom_text(aes(y = pct/2, label = scales::percent(pct, accuracy = .1))) +
        scale_y_continuous(name = "educational attainment rate", labels = scales::percent) +
        labs(x = "PUMA")

pums_survey_RE <- pums_survey_data %>%
    filter(AGEP >= 25 & AGEP < 65) %>%
    group_by(race_ethn, has_degree) %>%
    summarize(
        pct = survey_mean(proportion = TRUE, vartype = c("ci"), level = 0.95),
        n = survey_total(vartype = c("ci"), level = 0.95)
        )

pums_survey_RE %>%
    filter(has_degree == TRUE) %>%
    ggplot(aes(fill = has_degree, x = race_ethn, y = pct)) +
        geom_bar(position = "stack", stat = "identity", color = "black") +
        geom_errorbar(aes(ymin = pct_low, ymax = pct_upp, width = 0.2)) +
        scale_fill_brewer(palette = "Blues", guide = "none") +
        geom_text(aes(y = pct/2, label = scales::percent(pct, accuracy = .1))) +
        scale_y_continuous(name = "educational attainment rate", labels = scales::percent) +
        labs(x = "Race/Ethnicity")

pums_survey_by_occ <- pums_survey_data %>%
    filter(AGEP >= 25 & AGEP < 65 & ESR != 6) %>%
    group_by(SOCP, SOCP_label, degree) %>%
    summarize(
        n = survey_total(vartype = c("ci"), level = 0.95)
        )

# adjust for each attainment level
pums_survey_by_occ %>%
    filter(degree == "High school graduate") %>%
    select(SOCP_label, SOCP, n, n_low, n_upp) %>%
    arrange(-n) %>%
    head(10) %>%
    kable(
        digits = 0,
        format.args = list(big.mark = ","),
        col.names = c(
            "Occupation",
            "SOC code (ACS)",
            "# Employed",
            "95%CI LB",
            "95%CI UB")
        )
