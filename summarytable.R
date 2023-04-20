pums_survey_data %>%
    group_by(PUMA) %>%
    summarize(
        records = n(),
        weighted = survey_total(vartype = c("ci"), level = 0.95)
    ) %>%
    bind_rows(
        summarise(., across(where(is.numeric), sum),
            across(where(is.character), ~ 'Total')
            )
        ) %>%
    kable(digits = 0, format.args = list(big.mark = ","))
