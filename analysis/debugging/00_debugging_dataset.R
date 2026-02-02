# ==============================================================================
# NON-FATAL INJURIES DATA CLEANING AND PROCESSING
# ==============================================================================
# 
# Purpose: 
#   Clean, standardize, and process non-fatal injury data from the Colombian
#   National Institute of Legal Medicine and Forensic Sciences (INMLCF) for
#   the period 2015-2024. This script focuses specifically on cases of violence
#   against women.
#
# Data Source:
#   Instituto Nacional de Medicina Legal y Ciencias Forenses (INMLCF)
#   Official statistics on gender-based violence and injuries
#   URL: https://www.medicinalegal.gov.co
#
# Scope:
#   - Filters records for female victims only
#   - Standardizes variables across different reporting periods
#   - Classifies violence types: domestic, interpersonal, sexual, economic,
#     and sociopolitical
#   - Harmonizes geographic coding with DIVIPOLA standards
#   - Creates analytical variables for aggressor relationships and violence
#     subtypes
#
# Main Outputs:
#   1. Cleaned dataset: INMLCF_non_fatal_cleaned
#
# Key Variables Created:
#   - violence_nature: Main classification of violence type
#   - aggressor_grouped: Standardized aggressor relationship categories
#
# Notes:
#   - Records with undetermined age are excluded
#   - Geographic names standardized to official DIVIPOLA nomenclature
#   - Variables not applicable to sexual crimes are set to "No aplica"
#   - All text variables converted to factors for analysis
#
# Dependencies:
#   - dplyr: Data manipulation
#   - stringr: String operations
#   - DIVIPOLA.xlsx: Official geographic codes (auxiliary data)
#
# ==============================================================================


# Load libraries required 

library(dplyr)
library(lubridate)
library(stringr)
library(stringi)
library(janitor)

## Load and prepare non-fatal injury datasets

# Load non-fatal injury datasets from INMLCF
# Each dataset corresponds to a different type or year range
non_fatal_2024 <- read.csv("data/raw/non_fatal_INMLCF_2024.csv")
sexual_2015_2023 <- read.csv("data/raw/sexual_INMLCF_2015_2023.csv")
interpersonal_2015_2023 <- read.csv("data/raw/interpersonal_INMLCF_2015_2023.csv")
intrafamily_2015_2023 <- read.csv("data/raw/intrafamily_INMLCF_2015_2023.csv")

# The non-fatal injury datasets are composed as follows:
cat("Sexual violence dataset:", nrow(sexual_2015_2023), "rows and", ncol(sexual_2015_2023), "columns.\n")
cat("Interpersonal violence dataset:", nrow(interpersonal_2015_2023), "rows and", ncol(interpersonal_2015_2023), "columns.\n")
cat("Intrafamily violence dataset:", nrow(intrafamily_2015_2023), "rows and", ncol(intrafamily_2015_2023), "columns.\n")
 
# Regarding the 2024 dataset, it includes records that are not related to the Gender-Based Violence (GBV) indicator.  
# The GBV indicator comprises cases of alleged sexual offenses, interpersonal violence, and intrafamily violence.  
# Therefore, only records corresponding to these three categories were selected for further analysis.

## Data cleaning and filtering for 2024 dataset

# Keep only relevant categories for the VGB indicator
non_fatal_2024 <- non_fatal_2024 %>%
  filter(!violence_context %in% c(
    "7 Lesiones por Eventos de Transporte",
    "8 Lesiones Accidentales"
  ))

## Check for inconsistent columns between datasets

# Get column names for all datasets
cols_2024 <- colnames(non_fatal_2024)
cols_sexual <- colnames(sexual_2015_2023)
cols_interpersonal <- colnames(interpersonal_2015_2023)
cols_intrafamily <- colnames(intrafamily_2015_2023)

# Identify columns that appear only in some datasets
unique_cols <- setdiff(
  union(cols_2024, union(cols_sexual, union(cols_interpersonal, cols_intrafamily))),
  intersect(intersect(intersect(cols_2024, cols_sexual), cols_interpersonal), cols_intrafamily)
)

# Display unique columns
cat("Columns unique to some datasets:\n")
print(unique_cols)

# Since all unique columns are only present in 2024 data,
# remove them to ensure consistent structure when merging
non_fatal_2024 <- non_fatal_2024 %>% select(-all_of(unique_cols))

# Add a source indicator for each dataset
non_fatal_2024 <- non_fatal_2024 %>% mutate(source = "2024")
sexual_2015_2023 <- sexual_2015_2023 %>% mutate(source = "Sexual violence")
interpersonal_2015_2023 <- interpersonal_2015_2023 %>% mutate(source = "Interpersonal violence")
intrafamily_2015_2023 <- intrafamily_2015_2023 %>% mutate(source = "Intrafamily violence")


## Merge all datasets

# Merge all non-fatal injury datasets into a single dataframe
non_fatal_data <- bind_rows(
  non_fatal_2024,
  sexual_2015_2023,
  interpersonal_2015_2023,
  intrafamily_2015_2023
)

cat("Final merged dataset:", nrow(non_fatal_data), "rows and", ncol(non_fatal_data), "columns.\n")

## Standardize text information

#Function to replace "Sin Información" with "Sin información" in all text columns

replace_info <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col) | is.factor(col)) {
      col <- ifelse(col == "Sin Información", "Sin información", col)
    }
    return(col)
  })
  return(df)
}

non_fatal_data <- replace_info(non_fatal_data)


## Selection of relevant variables

# Remove variables that only apply to accidental deaths or traffic incidents
# Also remove 'id' as it's not relevant for analysis
columns_to_remove <- c(
  "victim_condition",
  "transport_or_displacement_means",
  "vehicle_service_type",
  "accident_class_or_type",
  "collision_object",
  "collision_object_service",
  "id"
)

non_fatal_data <- non_fatal_data %>% 
  select(-all_of(columns_to_remove))

## Filter by gender

# Filter cases where victim is female (based on biological sex, not gender identity)
# Note: Gender identity is not available for all records
non_fatal_data <- non_fatal_data %>% 
  filter(victim_sex == "Mujer")

# Remove victim_sex column as all remaining records are female
non_fatal_data <- non_fatal_data %>% 
  select(-victim_sex)


## Create domestic violence subcategory variable

# Create 'domestic_violence_subtype' variable to classify domestic violence cases
# Categories: partner violence, elder abuse, child/adolescent abuse, other family violence
non_fatal_data <- non_fatal_data %>% 
  mutate(
    domestic_violence_subtype = case_when(
      # Elder abuse
      violence_context %in% c(
        "5 Lesiones no Fatales contra el Adulto Mayor por Violencia Intrafamiliar",
        "Violencia Contra el Adulto Mayor (VIF)"
      ) ~ "Violencia contra el adulto mayor",
      
      # Partner violence
      violence_context %in% c(
        "6 Lesiones no Fatales por Violencia de Pareja",
        "Violencia de Pareja"
      ) ~ "Violencia de pareja",
      
      # Child and adolescent abuse
      violence_context %in% c(
        "3 Lesiones no Fatales contra Niños, Niñas y Adolescentes por Violencia Intrafamiliar",
        "Violencia Contra Niños, Niñas y Adolescentes (VIF)"
      ) ~ "Violencia contra niños, niñas y adolescentes",
      
      # Violence between other family members
      violence_context %in% c(
        "4 Lesiones no Fatales por Violencia entre otros Familiares",
        "Violencia Entre Otros Familiares (VIF)"
      ) ~ "Violencia entre otros familiares",
      
      # Missing data
      is.na(violence_context) ~ NA_character_,
      
      # Not applicable (other types of violence)
      TRUE ~ "No aplica"
    )
  )


## Standardize violence context variable

# Rename violence_context levels to main categories
non_fatal_data <- non_fatal_data %>% 
  mutate(
    violence_context = case_when(
      # Domestic violence
      violence_context %in% c(
        "5 Lesiones no Fatales contra el Adulto Mayor por Violencia Intrafamiliar",
        "4 Lesiones no Fatales por Violencia entre otros Familiares",
        "3 Lesiones no Fatales contra Niños, Niñas y Adolescentes por Violencia Intrafamiliar",
        "Violencia Entre Otros Familiares (VIF)",
        "Violencia Contra el Adulto Mayor (VIF)",
        "Violencia Contra Niños, Niñas y Adolescentes (VIF)",
        "Violencia de Pareja",
        "6 Lesiones no Fatales por Violencia de Pareja"
      ) ~ "Violencia intrafamiliar",
      
      # Interpersonal violence
      violence_context %in% c(
        "1 Lesiones no Fatales por Violencia Interpersonal",
        "Violencia Interpersonal"
      ) ~ "Violencia interpersonal",
      
      # Alleged sexual crime
      violence_context %in% c(
        "2 Exámenes Medicolegales por Presunto Delito Sexual",
        "Presunto Delito Sexual"
      ) ~ "Presunto delito sexual",
      
      TRUE ~ NA_character_
    )
  )


## Standardize temporal variables

# Standardize month names 
non_fatal_data <- non_fatal_data %>% 
  mutate(month_of_event = str_to_title(month_of_event))

# Standardize day names 
non_fatal_data <- non_fatal_data %>% 
  mutate(day_of_event = str_to_title(day_of_event))

# Standardize time range format (replace " a " with " - ")
non_fatal_data <- non_fatal_data %>% 
  mutate(time_range_3h = str_replace(time_range_3h, " a ", " - "))


## Standardize geographic variables

# Load official DIVIPOLA codes for validation
DIVIPOLA <- read_excel("DIVIPOLA.xlsx", col_types = c("numeric", "text", "numeric", 
                                    "text", "text", "text"))

# Standardize department names
non_fatal_data <- non_fatal_data %>% 
  mutate(
    department_name_dane = case_when(
      str_to_lower(department_name_dane) == "quindio" ~ "Quindío",
      str_to_lower(department_name_dane) == "san andrés y providencia" ~ 
        "Archipiélago de San Andrés, Providencia y Santa Catalina",
      TRUE ~ department_name_dane
    )
  )

# Standardize municipality names
municipality_replacements <- c(
  "Cuaspúd" = "Cuaspud Carlosama",
  "Barranco Minas" = "Barrancominas",
  "Belén de Bajirá" = "Nuevo belén de Bajirá",
  "Guapí" = "Guapi",
  "Ancuyá" = "Ancuya",
  "Cali" = "Santiago de Cali",
  "Mompós" = "Santa cruz de Mompox",
  "Güicán" = "Güicán de la Sierra",
  "Piendamó" = "Piendamó - Tunía",
  "Sotara" = "Sotará - Paispamba",
  "San Luis de Cubarral" = "Cubarral",
  "Cúcuta" = "San José de Cúcuta",
  "Coloso" = "Colosó",
  "Tolú Viejo" = "San José de toluviejo",
  "Armero Guayabal" = "Armero"
)

non_fatal_data <- non_fatal_data %>% 
  mutate(
    municipality_name_dane = case_when(
      municipality_name_dane %in% names(municipality_replacements) ~ 
        municipality_replacements[municipality_name_dane],
      TRUE ~ municipality_name_dane
    )
  )

# Standardize locality names (Bogotá districts)
locality_replacements <- c(
  "San Cristobal" = "San Cristóbal",
  "Fontibon" = "Fontibón",
  "Los Martires" = "Los Mártires",
  "Santafé" = "Santa Fe",
  "Usaquen" = "Usaquén",
  "Engativa" = "Engativá"
)

non_fatal_data <- non_fatal_data %>% 
  mutate(
    event_locality = case_when(
      event_locality %in% names(locality_replacements) ~ 
        locality_replacements[event_locality],
      TRUE ~ event_locality
    )
  )

# Remove locality variable
non_fatal_data <- non_fatal_data %>% 
  select(-event_locality)

# Standardize event zone variable
non_fatal_data <- non_fatal_data %>% 
  mutate(
    event_zone = case_when(
      event_zone == "Centro poblado(corregimiento, inspección de policía y caserío)" ~ 
        "Centro poblado (corregimiento, inspección de policía y caserío)",
      TRUE ~ event_zone
    )
  )

## Standardize event scenario variable

non_fatal_data <- non_fatal_data %>% 
  mutate(event_scenario = str_to_lower(event_scenario)) %>% 
  mutate(
    event_scenario = case_when(
      event_scenario == "vehículo de servicio particular" ~ 
        "vehículo servicio particular",
      event_scenario == "calle (autopista, avenida, dentro de la ciudad)" ~ 
        "vía pública",
      event_scenario == "piscina, jacuzzi (establecimientos turísticos)" ~ 
        "piscina y jacuzzi (establecimientos turísticos, recreativos, deportivos)",
      event_scenario == "medio de transporte masivo" ~ 
        "transporte masivo",
      TRUE ~ event_scenario
    )
  )

## Standardize activity during event variable

non_fatal_data <- non_fatal_data %>% 
  mutate(
    activity_during_event = case_when(
      activity_during_event == "Actividades relacionadas con el aprendizaje" ~ 
        "Actividades relacionadas con el estudio y el aprendizaje",
      activity_during_event %in% c(
        "Actividades relacionadas con manifestaciones públicas (marchas, protestas, etc.)",
        "Actividades relacionadas con manifestaciones públicas (Marchas, protestas, etc)"
      ) ~ "Actividades relacionadas con manifestaciones públicas (marchas, protestas, etc)",
      activity_during_event == "Quehacer habitualmente no remunerado" ~ 
        "Actividades de trabajo doméstico no pagado para el uso del propio hogar",
      activity_during_event == "Actividades de tiempo libre" ~ 
        "Actividad de tiempo libre",
      TRUE ~ activity_during_event
    )
  )


## Standardize causal mechanism variable

non_fatal_data <- non_fatal_data %>% 
  mutate(
    causal_mechanism = case_when(
      causal_mechanism %in% c(
        "Mecanismo o agente explosivo",
        "Agente o mecanismo explosivo"
      ) ~ "Agentes y mecanismo explosivo",
      causal_mechanism == "Otros" ~ "Otro",
      causal_mechanism == "Corto contundente" ~ "Cortocontundente",
      causal_mechanism == "Corto punzante" ~ "Cortopunzante",
      causal_mechanism == "Proyectil de Arma de Fuego" ~ 
        "Proyectil de arma de fuego",
      causal_mechanism == "Caústico" ~ "Cáustico",
      causal_mechanism %in% c(
        "Agente químico corrosivo",
        "Agente químico irritante",
        "Agente químico tóxico"
      ) ~ "Agente químico",
      TRUE ~ causal_mechanism
    )
  )

# Set causal_mechanism to "No aplica" for alleged sexual crimes
non_fatal_data <- non_fatal_data %>% 
  mutate(
    causal_mechanism = if_else(
      violence_context == "Presunto delito sexual",
      "No aplica",
      causal_mechanism
    )
  )

## Standardize injury topographic diagnosis variable

non_fatal_data <- non_fatal_data %>% 
  mutate(injury_topographic_diagnosis = str_to_lower(injury_topographic_diagnosis))

# Set to "No aplica" for alleged sexual crimes (per data dictionary)
non_fatal_data <- non_fatal_data %>% 
  mutate(
    injury_topographic_diagnosis = if_else(
      violence_context == "Presunto delito sexual",
      "no aplica",
      injury_topographic_diagnosis
    )
  )

## Standardize days of medical incapacity variable

non_fatal_data <- non_fatal_data %>% 
  mutate(
    days_of_medical_incapacity = case_when(
      days_of_medical_incapacity == "00Sin incapacidad médico legal" ~ 
        "Sin días de incapacidad",
      TRUE ~ days_of_medical_incapacity
    )
  )

# Set to "No aplica" for alleged sexual crimes (per data dictionary)
non_fatal_data <- non_fatal_data %>% 
  mutate(
    days_of_medical_incapacity = if_else(
      violence_context == "Presunto delito sexual",
      "No aplica",
      days_of_medical_incapacity
    )
  )

## Standardize alleged aggressor variable

# Define replacement patterns for alleged aggressor standardization
aggressor_replacements <- c(
  "(?i)^ex esposo ?\\(a\\)$" = "Ex-esposo(a)",
  "(?i)^ex compañero ?\\(a\\) permanente$" = "Ex-compañero(a) permanente",
  "(?i)^compañero ?\\(a\\) permanente$" = "Compañero(a) permanente",
  "(?i)^esposo ?\\(a\\)$" = "Esposo(a)",
  "(?i)^novio ?\\(a\\)$" = "Novio(a)",
  "(?i)^ex novio ?\\(a\\)$" = "Ex-novio(a)",
  "(?i)^ex amante$" = "Ex-amante",
  "(?i)^amigo ?\\(a\\)$" = "Amigo(a)",
  "(?i)^hijo ?\\(a\\)$" = "Hijo(a)",
  "(?i)^hermano ?\\(a\\)$" = "Hermano(a)",
  "(?i)^suegro ?\\(a\\)$" = "Suegro(a)",
  "(?i)^cuñado ?\\(a\\)$" = "Cuñado(a)",
  "(?i)^primo ?\\(a\\)$" = "Primo(a)",
  "(?i)^empleado ?\\(a\\)$" = "Empleado(a)",
  "(?i)^tío ?\\(a\\)$" = "Tío(a)",
  "(?i)^conocido sin ningun trato$" = "Conocido sin ningún trato",
  "(?i)^compañero ?\\(a\\) de trabajo$" = "Compañero(a) de trabajo",
  "(?i)^abuelo ?\\(a\\)$" = "Abuelo(a)",
  "(?i)^profesor ?\\(a\\)$" = "Profesor(a)",
  "(?i)^ex compañero ?\\(a\\) sentimental$" = "Ex-compañero(a) sentimental",
  "(?i)^compañero ?\\(a\\) de estudio$" = "Compañero(a) de estudio",
  "(?i)^ejercito$" = "Fuerzas Militares",
  "(?i)^sobrino ?\\(a\\)$" = "Sobrino(a)",
  "(?i)^compañero ?\\(a\\) de celda$" = "Compañero(a) de celda",
  "(?i)^compañero de celda$" = "Compañero(a) de celda",
  "(?i)^nieto ?\\(a\\)$" = "Nieto(a)",
  "(?i)^barra\\(s\\) futbolera\\(s\\)$" = "Barra(s) futbolera(s)",
  "(?i)^barras futboleras$" = "Barra(s) futbolera(s)",
  "(?i)^eln$" = "ELN",
  "(?i)^farc$" = "FARC",
  "(?i)^epl$" = "EPL",
  "(?i)^cti$" = "CTI",
  "(?i)^miembro del inpec$" = "Miembro del INPEC",
  "(?i)^hermanastro ?\\(a\\)$" = "Hermanastro(a)",
  "(?i)^hijastro ?\\(a\\)$" = "Hijastro(a)"
)

non_fatal_data <- non_fatal_data %>% 
  mutate(
    alleged_aggressor = str_replace_all(
      str_to_lower(alleged_aggressor), 
      aggressor_replacements
    ),
    alleged_aggressor = str_replace(
      alleged_aggressor, 
      "^(.)", 
      ~str_to_upper(.x)
    )
  )

## Create grouped aggressor variable

# Group aggressors by relationship type (based on INMLCF bulletins)
non_fatal_data <- non_fatal_data %>% 
  mutate(
    aggressor_grouped = case_when(
      # Current partner
      alleged_aggressor %in% c(
        "Amante", "Novio(a)", "Compañero(a) permanente", 
        "Esposo(a)", "Pareja o expareja"
      ) ~ "Pareja",
      
      # Ex-partner
      alleged_aggressor %in% c(
        "Ex-esposo(a)", "Ex-compañero(a) permanente", 
        "Ex-amante", "Ex-novio(a)", "Ex-compañero(a) sentimental"
      ) ~ "Ex-pareja",
      
      # Family member
      alleged_aggressor %in% c(
        "Madrastra", "Padre", "Hijo(a)", "Tío(a)", "Primo(a)", 
        "Abuelo(a)", "Cuñado(a)", "Hermanastro(a)", "Hijastro(a)", 
        "Hermano(a)", "Padrastro", "Madre", 
        "Otros familiares civiles o consanguíneos", 
        "Sobrino(a)", "Suegro(a)", "Nuera", "Yerno", "Nieto(a)"
      ) ~ "Familiar",
      
      # Acquaintance
      alleged_aggressor %in% c(
        "Compañero(a) de celda", "Compañero(a) de estudio", 
        "Compañero(a) de trabajo", "Conocido sin ningun trato", 
        "Curandero", "Empleador", "Profesor(a)", "Vecino", 
        "Conocido sin ningún trato", "Otros conocidos"
      ) ~ "Conocido",
      
      # Armed forces and police
      alleged_aggressor %in% c(
        "Servicios de inteligencia", "Policía", "Fuerzas militares", 
        "Armada", "CTI", "Personal de custodia", "Miembro del INPEC", 
        "Policía judicial", "Fuerzas Militares"
      ) ~ "Miembros de las fuerzas armadas, de policía, policía judicial y servicios de inteligencia",
      
      # Illegal armed groups
      alleged_aggressor %in% c(
        "Otras guerrillas", "ELN", "FARC", 
        "Fuerzas irregulares", "EPL", "Paramilitares"
      ) ~ "Miembro de grupos alzados al margen de la ley",
      
      # Organized crime
      alleged_aggressor %in% c(
        "Narcotraficantes", "Pandillas", "Bandas criminales"
      ) ~ "Miembro de un grupo de la delincuencia organizada",
      
      # Caregiver
      alleged_aggressor %in% c(
        "Encargado del niño, niña o adolescente",
        "Encargado de la persona mayor"
      ) ~ "Encargado del cuidado",
      
      # Other
      alleged_aggressor %in% c(
        "Barra(s) futbolera(s)", "Curandero", "Cabezas rapadas", 
        "Metaleros", "Hoppers", "Punks"
      ) ~ "Otro",
      
      TRUE ~ alleged_aggressor
    )
  )

## Standardize aggressor sex variable

non_fatal_data <- non_fatal_data %>% 
  mutate(
    alleged_aggressor_sex = case_when(
      alleged_aggressor_sex == "HOMBRE" ~ "Hombre",
      alleged_aggressor_sex == "MUJER" ~ "Mujer",
      alleged_aggressor_sex == "Transgenero" ~ "Transgénero",
      TRUE ~ alleged_aggressor_sex
    )
  )

## Standardize alleged aggressor variable

# Major/minor age classification
non_fatal_data <- non_fatal_data %>% 
  mutate(
    victim_age_major_minor_group = case_when(
      victim_age_major_minor_group == "a) Menores de Edad (<18 años)" ~ 
        "Menor de edad",
      victim_age_major_minor_group == "b) Mayores de Edad (>18 años)" ~ 
        "Mayor de edad",
      TRUE ~ victim_age_major_minor_group
    )
  )

# Remove records with undetermined age
non_fatal_data <- non_fatal_data %>% 
  filter(victim_age_major_minor_group != "Por determinar")

# Remove judicial_age variable (redundant with victim_age_group)
non_fatal_data <- non_fatal_data %>% 
  select(-judicial_age)

# Standardize age group format
non_fatal_data <- non_fatal_data %>% 
  mutate(
    victim_age_group = if_else(
      grepl("\\(\\d+\\s*(a|y)\\s*\\d+|más\\)", victim_age_group),
      gsub(
        "[()]", "", 
        gsub(
          "\\s*a\\s*", "-", 
          gsub("\\s*y\\s*más", "-más", victim_age_group)
        )
      ),
      victim_age_group
    )
  )

# Standardize life cycle format
non_fatal_data <- non_fatal_data %>% 
  mutate(life_cycle = gsub("\\(.*?\\) ", "", life_cycle))

## Standardize sociedemographic variables 

# Group affiliation
non_fatal_data <- non_fatal_data %>% 
  mutate(
    group_affiliation = case_when(
      group_affiliation == "Persona recluída en establecimientos de rehabilitación y pabellones psiquiátricos" ~ 
        "Persona recluida en establecimiento de rehabilitación y pabellones psiquiátricos",
      group_affiliation == "Persona mayores en hogares de cuidado" ~ 
        "Persona mayor en hogares de cuidado",
      group_affiliation == "Sector social LGBT (OSIGD)" ~ 
        "Sector social LGBTI (OSIGD)",
      group_affiliation %in% c(
        "Maestro / Educador", "Maestro / educador", "Maestro/Educador"
      ) ~ "Maestro-Educador",
      group_affiliation == "Ex convictos (as)" ~ "Ex-convictos(as)",
      group_affiliation == "Campesinos (as) y/o trabajadores (as) del campo" ~ 
        "Campesinos(as) y/o trabajadores(as) del campo",
      group_affiliation == "Persona que ejercen actividades de periodismo" ~ 
        "Persona que ejerce actividades de periodismo",
      group_affiliation == "Personas que ejercen actividades gremiales o sindicales" ~ 
        "Persona que ejerce actividades gremiales o sindicales",
      group_affiliation == "Otra" ~ "Otro",
      group_affiliation == "Persona privada de la libertad" ~ 
        "Persona bajo custodia",
      TRUE ~ group_affiliation
    )
  )

# Ethnic affiliation
non_fatal_data <- non_fatal_data %>% 
  mutate(
    ethnic_affiliation = case_when(
      ethnic_affiliation == "Sin Pertenencia Étnica" ~ 
        "Sin pertenencia étnica",
      ethnic_affiliation == "Rom (Gitano)" ~ "ROM (Gitano)",
      TRUE ~ ethnic_affiliation
    )
  )

# Education level
non_fatal_data <- non_fatal_data %>% 
  mutate(
    education_level = case_when(
      education_level == "Especialización, Maestría o equivalente" ~ 
        "Especialización, maestría o equivalente",
      TRUE ~ education_level
    )
  )

# Marital status
non_fatal_data <- non_fatal_data %>% 
  mutate(
    marital_status = case_when(
      marital_status %in% c("Soltero(a)", "Soltero (a)") ~ "Soltero(a)",
      marital_status %in% c("Viudo(a)", "Viudo (a)") ~ "Viudo(a)",
      marital_status %in% c(
        "Separado(a), divorciado(a)", 
        "Separado (a), Divorciado (a)",
        "Separado (a), divorciado (a)"
      ) ~ "Separado(a), divorciado(a)",
      marital_status %in% c("Casado(a)", "Casado (a)") ~ "Casado(a)",
      TRUE ~ marital_status
    )
  )

## Standardize event circumstance variable

non_fatal_data <- non_fatal_data %>% 
  mutate(
    event_circumstance = case_when(
      event_circumstance == "Embriaguez (Alcohólica y no alcohólica)" ~ 
        "Embriaguez (alcohólica y no alcohólica)",
      event_circumstance == "Acto sexual violento con persona protegida" ~ 
        "Acceso carnal violento/acto sexual violento con persona protegida",
      event_circumstance == "Hurto" ~ "Atraco callejero o intento de",
      event_circumstance == "Ajuste de cuentas" ~ 
        "Venganza o ajuste de cuentas",
      event_circumstance == "Violencia Económica" ~ "Violencia económica",
      event_circumstance %in% c(
        "Presunta esclavitud sexual o prostitución forzada",
        "Presunta explotación sexual"
      ) ~ "Presunta explotación sexual y comercial",
      event_circumstance == "Violencia Sociopolitica" ~ 
        "Violencia sociopolítica",
      event_circumstance == "Desplazamiento forzado" ~ 
        "Abandono o despojo forzado de tierra",
      event_circumstance %in% c(
        "Retención ilegal", "Secuestro", "secuestro"
      ) ~ "Retención ilegal - secuestro",
      TRUE ~ event_circumstance
    )
  )

## Create violence nature classification variable

# Create comprehensive violence nature variable based on event circumstances
non_fatal_data <- non_fatal_data %>% 
  mutate(
    violence_nature = case_when(
      # Domestic violence
      event_circumstance %in% c(
        "Conflicto de pareja", "Conflicto familiar", 
        "Violencia intrafamiliar", 
        "Violencia a niños, niñas y adolescentes",
        "Violencia al adulto mayor", "Violencia de pareja",
        "Violencia entre otros familiares"
      ) ~ "Violencia intrafamiliar",
      
      # Interpersonal violence
      event_circumstance %in% c(
        "Venganza o ajuste de cuentas", "Ajuste de cuentas", "Celos",
        "Abuso de autoridad", "Amenaza o intimidación", "Intolerancia",
        "Embriaguez (Alcohólica y no alcohólica)",
        "Embriaguez (alcohólica y no alcohólica)",
        "Contacto engañoso vía internet", "Ejercicio de actividades ilícitas",
        "Matoneo", "Retención legal", "Riña", "Tortura",
        "Aglomeración de público", "Bala perdida", "Sicariato",
        "Disturbios civiles", "Intervención Legal", "Linchamiento",
        "Minería ilegal", "Homofobia", "Incendio", "Negligencia"
      ) ~ "Violencia interpersonal",
      
      # Economic violence
      event_circumstance %in% c(
        "Violencia Económica", "Violencia económica", "Económicas",
        "Pillaje", "Hurto", "Atraco callejero o intento de"
      ) ~ "Violencia económica",
      
      # Sociopolitical violence
      event_circumstance %in% c(
        "Abandono o despojo forzado de tierra",
        "Acción bandas criminales",
        "Acción grupos alzados al margen de la ley",
        "Acción militar", "Acto terrorista",
        "Agresión contra grupos marginales o descalificados",
        "Artefacto explosivo", "Artefacto explosivo improvisado",
        "Asesinato político",
        "Ataque a instalación de las fuerzas armadas estatales",
        "Ataque contra misión médica", "Atentado terrorista",
        "Bombardeo", "Combate", "Cultivo ilícito",
        "Desaparición forzada", "Desplazamiento forzado",
        "Emboscada", "Enfrentamiento armado", "Explosión",
        "Hostigamiento", "Marcha o protesta social", "Masacre",
        "Mina Antipersonal - Munición sin Explotar",
        "Reclutamiento de niños, niñas y adolescentes",
        "Retención ilegal", "Retención ilegal - secuestro",
        "Violencia Sociopolítica", "Machismo",
        "Mina Antipersona - Munición sin Explotar",
        "Despojo forzado de tierra", "Violencia Sociopolitica"
      ) ~ "Violencia sociopolítica",
      
      # Sexual violence
      event_circumstance %in% c(
        "Acceso carnal violento",
        "Acceso carnal violento/acto sexual violento con persona protegida",
        "Agresión o ataque sexual", "Explotación sexual y comercial",
        "Abuso sexual", "Acoso sexual", "Acto sexual",
        "Acto sexual violento", "Acto sexual violento con persona protegida",
        "Asalto sexual", "Desnudez forzada",
        "Obligar a presenciar actos sexuales",
        "Obligar a realizar actos sexuales", "Pornografía",
        "Presunta esclavitud sexual o prostitución forzada",
        "Presunta explotación sexual",
        "Presunta trata de personas con fines de explotación sexual",
        "Tráfico o trata de personas",
        "Violencia sexual en el contexto del conflicto armado",
        "Presunta explotación sexual de niños, niñas o adolescentes",
        "Presunta explotación sexual y comercial",
        "Abuso dentro de establecimiento prestador de servicios de salud",
        "Aborto forzado", "Embarazo forzado"
      ) ~ "Violencia sexual",
      
      # Missing information
      event_circumstance == "Sin información" ~ "Sin informacion",
      
      # Other types
      event_circumstance %in% c(
        "Abandono", "Consumo de alcohol y/o sustancias psicoactivas",
        "Enfermedad física o mental", "Infidelidad"
      ) ~ "Otra",
      
      TRUE ~ event_circumstance
    )
  )

## Convert all variables to factors

non_fatal_data <- non_fatal_data %>% 
  mutate(across(everything(), as.factor))

## Reorder and select final columns 
non_fatal_data <- non_fatal_data %>% 
  select(
    # Victim demographics
    victim_country_of_birth,
    victim_age_group,
    victim_age_major_minor_group,
    life_cycle,
    marital_status,
    education_level,
    group_affiliation,
    ethnic_affiliation,
    
    # Temporal variables
    year_of_event,
    month_of_event,
    day_of_event,
    time_range_3h,
    
    # Geographic variables
    department_code_dane,
    department_name_dane,
    municipality_code_dane,
    municipality_name_dane,
    event_zone,
    
    # Violence context
    event_circumstance,
    violence_nature,
    violence_context,
    domestic_violence_subtype,
    
    # Event details
    causal_mechanism,
    event_scenario,
    activity_during_event,
    injury_topographic_diagnosis,
    days_of_medical_incapacity,
    
    # Aggressor information
    alleged_aggressor_sex,
    alleged_aggressor,
    aggressor_grouped,
    
    # Data source
    source
  )

## Save debugged data

write.csv(non_fatal_data, "data/interim/INMLCF_non_fatal_cleaned.csv", row.names = FALSE)


