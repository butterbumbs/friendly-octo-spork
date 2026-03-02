library(tidyverse)
library(ggplot2)
library(haven)
library(survey)
library(srvyr)
library(scales)


survey <- read_sav("IRG1961_CTM2512_W4_20251231_WT_SDA.sav")

#only use FSA data containing V and
# correspond FSA code to a city

bc_survey <- survey %>%
    filter(substr(FSA, 1, 1) == "V") %>%
    mutate(
        city_fsa = case_when(
            #Surrey (treated)
            FSA %in% c("V1M","V3R","V3S","V3T","V3V","V3W","V3X","V3Z","V4N") ~ "Surrey",

            #controls
            #ABBOTSFORD
            FSA %in% c("V2S","V2T","V3G","V4X") ~ "ABBOTSFORD (control)",

            #Chilliwack
            FSA %in% c("V2P","V2R","V4Z") ~ "Chilliwack (control)",

            #Maple Ridge
            FSA %in% c("V2W","V2X","V3Y","V4R") ~ "Maple Ridge (control)",

            # Richmond FSAs
            FSA %in% c("V6V","V6W","V6X","V6Y","V7A","V7B","V7C","V7E") ~ "Richmond (control)"
        )
    ) %>%
    filter(!is.na(city_fsa))

#city counts
bc_survey_city <- bc_survey %>%
    group_by(city_fsa, A6) %>%
    summarize(count = n(), .groups = "drop")


# include all of BC
bc_survey_bc_overall <- bc_survey %>%
    mutate(city_fsa = "BC overall") %>%
    group_by(city_fsa, A6) %>%
    summarize(count = n(), .groups = "drop")


# pooled control = average of all controls if not surrey
bc_survey_pooled <- bc_survey %>%
    mutate(
        city_fsa = if_else(
            city_fsa == "Surrey",
            "Surrey",
            "Pooled control"
        )
    ) %>%
    group_by(city_fsa, A6) %>%
    summarize(count = n(), .groups = "drop")


# combine cities, pooled control, and BC overall
bc_survey_grouped <- bind_rows(
    bc_survey_city,
    bc_survey_pooled,
    bc_survey_bc_overall
)



# convert counts to share proportion
bc_survey_grouped <- bc_survey_grouped %>%
    group_by(city_fsa) %>%
    mutate(share = count / sum(count)) %>%
    ungroup()

# correspond each  A6 level with actual party
bc_survey_grouped <- bc_survey_grouped %>%
    mutate(A6_label = case_when(
        A6 == 1  ~ "Conservative",
        A6 == 2  ~ "Liberal",
        A6 == 3  ~ "Democratic Party",
        A6 == 4  ~ "Bloc Québécois",
        A6 == 5  ~ "Green Party",
        A6 == 6  ~ "Not Conservative",
        A6 == 7  ~ "Communist",
        A6 == 8  ~ "Libertarian",
        A6 == 9  ~ "Independent",
        A6 == 88 ~ "Another Party",
        A6 == 98 ~ "Undecided",
        A6 == 99 ~ "Would Not Answer",
        TRUE ~ NA_character_
    )
    ) %>%
    filter(!is.na(A6_label)) %>%
    mutate(A6_label = factor(A6_label))


# order
party_order <- bc_survey_grouped %>%
    group_by(A6_label) %>%
    summarize(total = sum(count), .groups = "drop") %>%
    arrange(desc(total))

bc_survey_grouped$A6_label <- factor(
    bc_survey_grouped$A6_label,
    levels = party_order$A6_label
)

ggplot(bc_survey_grouped, aes(x = city_fsa, y = count, fill = A6_label)) +
    geom_bar(stat = "identity", position = "fill") +  scale_y_continuous(labels = percent) +
    labs(
        title = "Voting Distribution by Surrey and control cities",
        x = "City / Region",
        y = "Share of Respondents",
        fill = "Political Party"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )


library(leaflet)
library(dplyr)

cities <- tibble(
    city_fsa = c("Surrey", "Abbotsford", "Chilliwack",
                 "Maple Ridge", "Richmond"),
    treated_status = c("Treated", "Control", "Control", "Control", "Control"),
    lat = c(49.1913, 49.0504, 49.1579, 49.2194, 49.1666),
    lng = c(-122.8490, -122.3045, -121.9520, -122.5983, -123.1336),
    syrian_gar_count = c((1082/2507)*100, (11/2507)*100, (27/2507)*100, (13/2507)*100, (13/2507)*100)
)

my.pal <- colorFactor(
    palette = c("red", "green"),
    levels = c("Treated", "Control")
)



make_map <- function(cities, pal) {

    cities %>%
        leaflet() %>%
        addProviderTiles("CartoDB") %>%
        addCircleMarkers(
            lng = ~lng,
            lat = ~lat,
            radius = 8,
            color = ~my.pal(treated_status),
            label = ~paste0(treated_status,
                            " | Percentage Total of Syrian GAR's in B.C: ",
                            round(syrian_gar_count, 3),"%")
        ) %>%
        addLegend(
            position = "topright",
            pal = my.pal,
            values = ~treated_status
        )
}
