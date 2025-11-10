# ==============================================================================
# FINAL DATA CLEANING AND DESCRIPTIVE ANALYSIS: NON-FATAL RECORDS
# ==============================================================================
#
# Purpose:
#   Perform final adjustments to focus on gender-based violence (GBV) typologies
#   and generate descriptive analyses of interest. This script applies filters,
#   recategorizes variables, and creates visualizations for exploratory analysis.
#
# Input:
#   - INMLCF_non_fatal_cleaned_2015_2024_v2.csv (cleaned dataset from 00_debugging_dataset.R)
#
# Main Operations:
#   1. Apply filters for GBV-specific analysis
#   2. Recategorize and group variables
#   3. Generate descriptive statistics
#   4. Create visualization plots
#
# Filters Applied:
#   - Violence type: Sexual, interpersonal, domestic, sociopolitical
#   - Victim's country: Colombia only
#   - Aggressor: Partner, family, ex-partner, acquaintance, conflict actors
#   - Age group: Excludes elderly adults
#
# Groupings Created:
#   - Life cycle: Children/adolescents (0-19), Youth (20-34), Adults (35-59)
#   - Event scenario: Home vs. social spaces
#   - Education: Basic vs. higher education
#   - Time of day: Dawn, day, night
#   - Day of week: Weekday vs. weekend
#
# Output:
#   - Filtered dataset for modeling: data_nf_model.csv
#   - Multiple visualization plots (PDF format)
#
# Dependencies:
#   - dplyr: Data manipulation
#   - ggplot2: Data visualization
#   - patchwork: Combine multiple plots
#   - forcats, stringr: Factor and string manipulation
# ==============================================================================

## # Load libraries required 
library(dplyr)
library(forcats)
library(stringr)
library(ggplot2)
library(patchwork)

# Suppress scientific notation for better readability
options(scipen = 999)

## Load cleaned data

non_fatal_data <-  read.csv("data/interim/INMLCF_non_fatal_cleaned.csv", stringsAsFactors = TRUE)


## Apply filters for GBV analysis

# Filter 1: Keep only specific violence types relevant to GBV
violence_types_to_keep <- c(
  "Violencia sexual",
  "Violencia interpersonal",
  "Violencia intrafamiliar",
  "Violencia sociopolítica"
)

non_fatal_data <- non_fatal_data %>%
  filter(violence_nature %in% violence_types_to_keep)


# Filter 2: Exclude circumstances not corresponding to GBV
# (stray bullet, brawls)
non_gbv_circumstances <- c("Bala perdida", "Riña")

non_fatal_data <- non_fatal_data %>%
  filter(!event_circumstance %in% non_gbv_circumstances)

# Filter 3: Keep only victims born in Colombia
non_fatal_data <- non_fatal_data %>%
  filter(victim_country_of_birth == "Colombia")


# Recategorize agressor relationship

# Group conflict actors and delincuency into a single category
non_fatal_data <- non_fatal_data %>%
  mutate(
    aggressor_grouped_final = case_when(
      # Conflict actors and organized crime
      aggressor_grouped %in% c(
        "Delincuencia común",
        "Miembro de grupos alzados al margen de la ley",
        "Miembro de un grupo de la delincuencia organizada",
        "Miembros de las fuerzas armadas, de policía, policía judicial y servicios de inteligencia"
      ) ~ "Actores del conflicto",
      
      # Caregivers and friends grouped as acquaintances
      aggressor_grouped %in% c("Encargado del cuidado", "Amigo(a)") ~ "Conocido",
      
      # Keep other categories as is
      TRUE ~ as.character(aggressor_grouped)
    )
  )

# Filter 4: Keep only relevant aggressor categories
relevant_aggressors <- c(
  "Pareja",
  "Familiar",
  "Ex-pareja",
  "Conocido",
  "Actores del conflicto"
)

non_fatal_data <- non_fatal_data %>%
  filter(aggressor_grouped_final %in% relevant_aggressors)


## Recategorize education level

# Group education levels into: basic education vs. higher education
non_fatal_data <- non_fatal_data %>%
  mutate(
    education_level_grouped = case_when(
      # Basic education
      education_level %in% c(
        "Educación básica primaria",
        "Educación básica secundaria o secundaria baja"
      ) ~ "Educación básica primaria y secundaria",
      
      # Higher education
      education_level %in% c(
        "Doctorado o equivalente",
        "Educación técnica profesional y tecnológica",
        "Especialización, maestría o equivalente",
        "Universitario",
        "Educación media o secundaria alta"
      ) ~ "Educación superior",
      
      # Keep other categories unchanged
      TRUE ~ as.character(education_level)
    )
  )

## Recategorize event scenario 

# Simplify event scenario: home vs. social spaces
non_fatal_data <- non_fatal_data %>%
  mutate(
    event_scenario_grouped = if_else(
      event_scenario == "vivienda",
      "vivienda",
      "Espacios sociales"
    )
  )

## Create age group variables 

# Create detailed age groups (10-year ranges, except for 0-9 and 10-19)
non_fatal_data <- non_fatal_data %>%
  mutate(
    age_group_detailed = case_when(
      victim_age_group %in% c("00-04", "05-09") ~ "00-09",
      victim_age_group %in% c("10-14", "15-17", "18-19") ~ "10-19",
      victim_age_group %in% c("20-24", "25-29") ~ "20-29",
      victim_age_group %in% c("30-34", "35-39") ~ "30-39",
      victim_age_group %in% c("40-44", "45-49") ~ "40-49",
      victim_age_group %in% c("50-54", "55-59") ~ "50-59",
      TRUE ~ as.character(victim_age_group)
    )
  )

# Create life cycle categories
non_fatal_data <- non_fatal_data %>%
  mutate(
    life_cycle_grouped = case_when(
      victim_age_group %in% c(
        "00-04", "05-09", "10-14", "15-17", "18-19"
      ) ~ "Niñas y adolescentes",
      
      victim_age_group %in% c("20-24", "25-29", "30-34") ~ "Jovenes",
      
      victim_age_group %in% c(
        "35-39", "40-44", "45-49", "50-54", "55-59"
      ) ~ "Adultez",
      
      TRUE ~ "Adulto mayor"
    )
  )

# Filter 5: Exclude elderly adults from analysis
non_fatal_data <- non_fatal_data %>%
  filter(life_cycle_grouped %in% c(
    "Niñas y adolescentes",
    "Jovenes",
    "Adultez"
  ))


## Recategorize time od day 

# Group time ranges into: dawn, day, night
non_fatal_data <- non_fatal_data %>%
  mutate(
    time_of_day = case_when(
      time_range_3h == "Sin información" ~ "Sin información",
      
      # Dawn: 00:00 - 05:59
      time_range_3h %in% c("00:00 - 02:59", "03:00 - 05:59") ~ "Madrugada",
      
      # Day: 06:00 - 17:59
      time_range_3h %in% c(
        "06:00 - 08:59", "09:00 - 11:59",
        "12:00 - 14:59", "15:00 - 17:59"
      ) ~ "Dia",
      
      # Night: 18:00 - 23:59
      TRUE ~ "Noche"
    )
  )

## Recategorize day of week 

# Group into weekday vs. weekend
non_fatal_data <- non_fatal_data %>%
  mutate(
    day_category = if_else(
      day_of_event %in% c("Sábado", "Domingo"),
      "Fin de semana",
      "Semana"
    )
  )

#  Prepare data for visualization

# Convert relevant variables to factors with appropriate levels
non_fatal_data <- non_fatal_data %>%
  mutate(
    year_of_event = as.factor(year_of_event),
    life_cycle_grouped = factor(
      life_cycle_grouped,
      levels = c("Niñas y adolescentes", "Jovenes", "Adultez")
    ),
    education_level_grouped = factor(
      education_level_grouped,
      levels = c(
        "Sin escolaridad",
        "Educación inicial y educación preescolar",
        "Educación básica primaria y secundaria",
        "Educación superior",
        "Sin información"
      )
    ),
    time_of_day = factor(
      time_of_day,
      levels = c("Madrugada", "Dia", "Noche", "Sin información")
    ),
    month_of_event = factor(
      month_of_event,
      levels = c(
        "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
        "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
      )
    )
  )


# Custom color palette for violence types
violence_colors <- c(
  "Violencia interpersonal" = "#fc9272",
  "Violencia intrafamiliar" = "#78c679",
  "Violencia sexual" = "#1f9bcf",
  "Violencia sociopolítica" = "#c994c7"
)

# Custom theme for consistent plot styling
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

## Create visualization filters

#' Create a frequency bar plot with percentages
#'
#' @param data Data frame containing the data
#' @param x_var Variable for x-axis (unquoted)
#' @param title Plot title
#' @param x_label X-axis label (default: empty string)
#' @param angle_x Angle for x-axis text (default: 45)
#'
#' @return A ggplot object
create_frequency_plot <- function(data, x_var, title, 
                                  x_label = "", angle_x = 45) {
  data %>%
    count({{ x_var }}) %>%
    mutate(proportion = n / sum(n) * 100) %>%
    ggplot(aes(x = {{ x_var }}, y = n)) +
    geom_col(fill = "#AE86D3", alpha = 0.8) +
    geom_text(
      aes(label = paste0(round(proportion, 1), "%")),
      vjust = -0.2,
      color = "#4B0082",
      fontface = "bold",
      size = 3
    ) +
    labs(title = title, x = x_label, y = "Frecuencia") +
    custom_theme +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
}

#' Create a grouped bar plot by violence type
#'
#' @param data Data frame containing the data
#' @param group_var Grouping variable (unquoted)
#' @param title Plot title
#'
#' @return A ggplot object
create_violence_comparison_plot <- function(data, group_var, title) {
  data %>%
    count({{ group_var }}, violence_nature) %>%
    group_by(violence_nature) %>%
    mutate(proportion = round(n / sum(n) * 100, 1)) %>%
    ungroup() %>%
    ggplot(aes(x = proportion, y = {{ group_var }}, fill = violence_nature)) +
    geom_col(position = "dodge") +
    geom_text(
      aes(label = proportion),
      position = position_dodge(width = 1),
      hjust = 0.5,
      size = 2.5
    ) +
    scale_fill_manual(values = violence_colors) +
    labs(
      x = "%",
      y = NULL,
      fill = "Tipo de violencia",
      title = title
    ) +
    custom_theme +
    coord_flip()
}

## Generate descriptive plots

# Plot 1: Violence cases by type
plot_violence_type <- create_frequency_plot(
  non_fatal_data,
  violence_nature,
  "Casos de violencia por tipo",
  angle_x = 45
)

# Plot 2: Cases by year
plot_year <- create_frequency_plot(
  non_fatal_data,
  year_of_event,
  "Casos de violencia por año",
  x_label = "Año",
  angle_x = 0
)

# Plot 3: Cases by month
plot_month <- create_frequency_plot(
  non_fatal_data,
  month_of_event,
  "Casos de violencia por mes",
  x_label = "Mes"
)

# Plot 4: Cases by day category (weekday vs weekend)
plot_day <- create_frequency_plot(
  non_fatal_data,
  day_category,
  "Casos de violencia por día",
  x_label = "Día",
  angle_x = 0
)

# Plot 5: Cases by time of day
plot_time <- create_frequency_plot(
  non_fatal_data,
  time_of_day,
  "Casos de violencia por hora",
  x_label = "Hora"
)

# Plot 6: Cases by detailed age group
plot_age_detailed <- create_frequency_plot(
  non_fatal_data,
  age_group_detailed,
  "Casos de violencia por grupo de edad",
  x_label = "Edad"
)

# Plot 7: Cases by life cycle
plot_life_cycle <- create_frequency_plot(
  non_fatal_data,
  life_cycle_grouped,
  "Casos de violencia por ciclo vital",
  angle_x = 45
)

# Plot 8: Cases by education level
plot_education <- create_frequency_plot(
  non_fatal_data,
  education_level_grouped,
  "Casos de violencia por escolaridad"
)

# Plot 9: Cases by marital status
plot_marital <- create_frequency_plot(
  non_fatal_data,
  marital_status,
  "Casos de violencia por estado civil"
)

# Plot 10: Cases by aggressor type
plot_aggressor <- create_frequency_plot(
  non_fatal_data,
  aggressor_grouped_final,
  "Casos de violencia por presunto agresor",
  angle_x = 0
)

# Plot 11: Cases by event scenario
plot_scenario <- create_frequency_plot(
  non_fatal_data,
  event_scenario_grouped,
  "Casos de violencia por escenario del hecho",
  angle_x = 0
)

# Plot 12: Life cycle by violence type
plot_lifecycle_violence <- create_violence_comparison_plot(
  non_fatal_data,
  life_cycle_grouped,
  "Ciclo vital"
)

# Plot 13: Day category by violence type
plot_day_violence <- create_violence_comparison_plot(
  non_fatal_data,
  day_category,
  "Semana del hecho"
)

# Plot 14: Event scenario by violence type (detailed)
plot_scenario_violence <- non_fatal_data %>%
  count(event_scenario_grouped, violence_nature) %>%
  group_by(violence_nature) %>%
  mutate(proportion = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  ggplot(aes(
    x = proportion,
    y = event_scenario_grouped,
    fill = violence_nature
  )) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = proportion),
    position = position_dodge(width = 0.9),
    hjust = -0.2,
    size = 3
  ) +
  scale_fill_manual(values = violence_colors) +
  labs(
    x = "%",
    y = NULL,
    fill = "Tipo de violencia",
    title = "Escenario del hecho"
  ) +
  custom_theme +
  coord_flip()

## Conbine and save plots 

# Combined temporal plots
temporal_combined <- (plot_year | plot_month) / (plot_day | plot_time)

# Combined age plots
age_combined <- plot_age_detailed | plot_life_cycle

# Combined sociodemographic plots
sociodem_combined <- plot_marital | plot_education

# Combined life cycle comparison
lifecycle_comparison <- plot_life_cycle | plot_lifecycle_violence

# Combined day comparison
day_comparison <- plot_day | plot_day_violence

ggsave("lifecycle_comparison.eps", plot = lifecycle_comparison, width = 10, height = 4.5)
# Save plots (uncomment to save)
# ggsave("temporal_plots.pdf", temporal_combined, width = 11, height = 6.5)
# ggsave("age_plots.pdf", age_combined, width = 9, height = 4)
# ggsave("sociodem_plots.pdf", sociodem_combined, width = 9, height = 5)
# ggsave("lifecycle_comparison.pdf", lifecycle_comparison, width = 10, height = 4.5)
# ggsave("day_comparison.pdf", day_comparison, width = 10, height = 4.5)


# Select only relevant variables for statistical modeling
modeling_data <- non_fatal_data %>%
  select(
    violence_nature,
    life_cycle_grouped,
    education_level_grouped,
    day_category,
    time_of_day,
    event_scenario_grouped,
    event_zone,
    aggressor_grouped_final
  )

# Rename variables for consistency
modeling_data <- modeling_data %>%
  rename(
    violence_type = violence_nature,
    life_cycle = life_cycle_grouped,
    education_level = education_level_grouped,
    day_type = day_category,
    time_category = time_of_day,
    event_scenario = event_scenario_grouped,
    aggressor_type = aggressor_grouped_final
  )


## Generate summary statistics 
# Summary by violence type
summary_violence <- modeling_data %>%
  count(violence_type) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 2),
    cumulative = cumsum(percentage)
  )

# Summary by life cycle and violence type
summary_lifecycle_violence <- modeling_data %>%
  count(life_cycle, violence_type) %>%
  group_by(violence_type) %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  ungroup()

# Summary by aggressor type
summary_aggressor <- modeling_data %>%
  count(aggressor_type, violence_type) %>%
  group_by(violence_type) %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  ungroup()

# Print summaries
message("\n=== SUMMARY STATISTICS ===\n")

message("Violence type distribution:")
print(summary_violence)

message("\nLife cycle by violence type:")
print(summary_lifecycle_violence)

message("\nAggressor type by violence type:")
print(summary_aggressor)

