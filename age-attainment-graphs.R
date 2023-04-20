library(ggplot2)

pums_survey_data %>%
    filter(AGEP >= 25 & AGEP < 65) %>%
    group_by(age_gr, degree) %>%
    summarize(n = survey_prop(vartype = c("ci"), level = 0.95)) %>%
    arrange(age_gr, rev(degree)) %>%
    mutate(label_y = cumsum(n)/sum(n)) %>%
    ggplot(aes(fill = degree, x = age_gr, y = n)) +
        geom_bar(position = "fill", stat = "identity", color = "black") +
        geom_text(aes(y = label_y, label = scales::percent(label_y, accuracy = .1), vjust = 1.5)) +
        scale_fill_brewer("Blues")


pums_survey_data %>%
    filter(AGEP >= 25 & AGEP < 65) %>%
    group_by(age_gr, degree) %>%
    summarize(n = survey_total(vartype = c("ci"), level = 0.95)) %>%
    arrange(age_gr, rev(degree)) %>%
    mutate(label_y = cumsum(n) - .5*n) %>%
    ggplot(aes(fill = degree, x = age_gr, y = n)) +
        geom_bar(position = "stack", stat = "identity", color = "black") +
        geom_text(aes(y = label_y, label = scales::comma(n))) +
        scale_fill_brewer("Blues")

