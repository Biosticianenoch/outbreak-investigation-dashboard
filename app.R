
## DATAQUEST SOLUTIONS
## CHOLERA OUTBREAK INVESTIGATION DASHBOARD
## Epidemiological Intelligence and Decision Support System
## Dashboard framework: shinydashboard
## Dataset: Synthetic Mwangaza County Cholera Outbreak

## 1. INSTALL AND LOAD PACKAGES

## Load required packages
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(lubridate)
  library(scales)
  library(broom)
  library(stringr)
  library(forcats)
})
## 2. APPLICATION CONFIGURATION

APP_TITLE <- "Outbreak Investigation and Epidemiological Intelligence"
ORGANIZATION <- "DATAQUEST SOLUTIONS"
OUTBREAK_NAME <- "Cholera Outbreak Investigation"

DATA_DIR <- "."

BRAND_NAVY <- "#14213D"
BRAND_NAVY_LIGHT <- "#263F68"
BRAND_ORANGE <- "#F97316"
BRAND_WHITE <- "#FFFFFF"
BRAND_GREY <- "#64748B"
BRAND_BACKGROUND <- "#F4F6FA"
BRAND_GREEN <- "#15803D"
BRAND_RED <- "#DC2626"

WEBSITE_URL <- "https://www.dataquestsolutions.co.ke/"
GITHUB_URL <- "https://github.com/Biosticianenoch"
LINKEDIN_URL <- "https://www.linkedin.com/in/enock-bereka/"

## 3. DATA IMPORT

read_dataset <- function(filename, required = TRUE) {
  
  filepath <- file.path(DATA_DIR, filename)
  
  if (!file.exists(filepath)) {
    
    if (required) {
      stop(
        paste0(
          "Required dataset not found: ",
          filename,
          "\nCheck the DATA_DIR setting and ensure the CSV files are extracted."
        )
      )
    }
    
    return(NULL)
  }
  
  read.csv(
    filepath,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "N/A", "NULL"),
    check.names = FALSE
  )
}

cases_raw <- read_dataset("case_line_list.csv")
population_raw <- read_dataset("population_denominators.csv")
case_control_raw <- read_dataset(
  "case_control_exposures.csv",
  required = FALSE
)

daily_raw <- read_dataset(
  "daily_epidemic_curve.csv",
  required = FALSE
)

dictionary_raw <- read_dataset(
  "data_dictionary.csv",
  required = FALSE
)

## 4. VALIDATE THE CORE CASE LINE LIST

required_columns <- c(
  "case_id",
  "subcounty",
  "village",
  "age_years",
  "age_group",
  "sex",
  "date_of_symptom_onset",
  "date_reported",
  "laboratory_result",
  "dehydration_status",
  "hospitalized",
  "outcome",
  "water_source_usual"
)

missing_columns <- setdiff(
  required_columns,
  names(cases_raw)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "The case line list is missing these required variables:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

if (nrow(cases_raw) == 0) {
  stop("The case line list contains no records.")
}

if (anyDuplicated(cases_raw$case_id)) {
  stop("Duplicate case identifiers were detected.")
}

## 5. DATA PREPROCESSING

to_date <- function(x) {
  
  if (inherits(x, "Date")) {
    return(x)
  }
  
  suppressWarnings(
    as.Date(as.character(x))
  )
}

to_numeric <- function(x) {
  
  suppressWarnings(
    as.numeric(as.character(x))
  )
}

to_binary <- function(x) {
  
  if (is.logical(x)) {
    return(as.integer(x))
  }
  
  value <- tolower(trimws(as.character(x)))
  
  result <- ifelse(
    value %in% c("1", "yes", "true", "positive", "present"),
    1,
    ifelse(
      value %in% c("0", "no", "false", "negative", "absent"),
      0,
      NA_real_
    )
  )
  
  as.integer(result)
}

cases <- cases_raw

date_variables <- c(
  "date_of_symptom_onset",
  "date_reported",
  "date_admitted",
  "date_specimen_collected",
  "date_outcome_recorded"
)

for (v in intersect(date_variables, names(cases))) {
  cases[[v]] <- to_date(cases[[v]])
}

if ("age_years" %in% names(cases)) {
  cases$age_years <- to_numeric(cases$age_years)
}

binary_variables <- c(
  "hospitalized",
  "drank_untreated_water",
  "ate_street_vended_food_7d",
  "attended_large_gathering_7d",
  "household_contact_of_case",
  "travel_outside_village_7d",
  "diarrhoea",
  "vomiting",
  "fever",
  "abdominal_cramps"
)

for (v in intersect(binary_variables, names(cases))) {
  cases[[v]] <- to_binary(cases[[v]])
}

cases <- cases |>
  mutate(
    reporting_delay = as.numeric(
      date_reported - date_of_symptom_onset
    )
  )

if ("date_admitted" %in% names(cases)) {
  cases$onset_to_admission <- as.numeric(
    cases$date_admitted - cases$date_of_symptom_onset
  )
} else {
  cases$onset_to_admission <- NA_real_
}

cases <- cases |>
  mutate(
    onset_week = lubridate::floor_date(
      date_of_symptom_onset,
      unit = "week",
      week_start = 1
    ),
    onset_month = lubridate::floor_date(
      date_of_symptom_onset,
      unit = "month"
    )
  ) |>
  filter(
    !is.na(case_id),
    !is.na(date_of_symptom_onset)
  )

## 6. POPULATION DENOMINATORS

if (!all(
  c("subcounty", "village", "population_at_risk") %in%
  names(population_raw)
)) {
  stop(
    paste(
      "population_denominators.csv must contain:",
      "subcounty, village and population_at_risk"
    )
  )
}

population <- population_raw |>
  mutate(
    population_at_risk = to_numeric(population_at_risk)
  )

if ("households" %in% names(population)) {
  population$households <- to_numeric(population$households)
} else {
  population$households <- NA_real_
}

## 7. CASE-CONTROL DATA PREPARATION

if (is.null(case_control_raw)) {
  
  case_control <- data.frame()
  
} else {
  
  case_control <- case_control_raw
  
  if ("age_years" %in% names(case_control)) {
    case_control$age_years <- to_numeric(case_control$age_years)
  }
  
  if ("interview_date" %in% names(case_control)) {
    case_control$interview_date <- to_date(
      case_control$interview_date
    )
  }
  
  case_control_binary <- c(
    "drank_untreated_water_7d",
    "ate_street_vended_food_7d",
    "attended_large_gathering_7d",
    "household_contact_of_case",
    "travel_outside_village_7d"
  )
  
  for (v in intersect(case_control_binary, names(case_control))) {
    case_control[[v]] <- to_binary(case_control[[v]])
  }
}

## 8. HELPER FUNCTIONS

safe_count <- function(data, variable) {
  
  if (!variable %in% names(data)) {
    return(data.frame())
  }
  
  data |>
    filter(!is.na(.data[[variable]])) |>
    count(
      category = .data[[variable]],
      name = "cases",
      sort = TRUE
    )
}

format_number <- function(x) {
  scales::comma(x, accuracy = 1)
}

format_percent <- function(x, denominator) {
  
  if (is.na(denominator) || denominator <= 0) {
    return("N/A")
  }
  
  scales::percent(
    x / denominator,
    accuracy = 0.1
  )
}

safe_divide <- function(x, y) {
  
  if (is.na(y) || y <= 0) {
    return(NA_real_)
  }
  
  x / y
}

empty_plot <- function(message = "No records available") {
  
  ggplot() +
    annotate(
      "text",
      x = 0,
      y = 0,
      label = message,
      size = 5,
      colour = BRAND_GREY
    ) +
    theme_void()
}

dashboard_plot_theme <- function() {
  
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(
        face = "bold",
        colour = BRAND_NAVY,
        size = 14
      ),
      plot.subtitle = element_text(
        colour = BRAND_GREY
      ),
      axis.title = element_text(
        face = "bold",
        colour = BRAND_NAVY
      ),
      axis.text = element_text(
        colour = "#475569"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        colour = "#E2E8F0"
      ),
      legend.position = "bottom",
      plot.background = element_rect(
        fill = BRAND_WHITE,
        colour = NA
      )
    )
}

make_plotly <- function(plot_object) {
  
  ggplotly(
    plot_object,
    tooltip = "text"
  ) |>
    layout(
      font = list(
        family = "Arial",
        color = BRAND_NAVY
      ),
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      margin = list(
        l = 55,
        r = 20,
        b = 55,
        t = 45
      )
    ) |>
    config(
      displaylogo = FALSE,
      responsive = TRUE
    )
}

## 9. CASE-CONTROL ODDS RATIO FUNCTION

calculate_odds_ratio <- function(data, variable) {
  
  required <- c("participant_type", variable)
  
  if (!all(required %in% names(data))) {
    return(NULL)
  }
  
  dat <- data |>
    filter(
      tolower(as.character(participant_type)) %in%
        c("case", "control"),
      !is.na(.data[[variable]])
    )
  
  if (nrow(dat) == 0) {
    return(NULL)
  }
  
  dat$participant_type <- ifelse(
    tolower(as.character(dat$participant_type)) == "case",
    "Case",
    "Control"
  )
  
  dat$exposure <- to_binary(dat[[variable]])
  
  dat <- dat |>
    filter(!is.na(exposure))
  
  tab <- table(
    factor(
      dat$participant_type,
      levels = c("Case", "Control")
    ),
    factor(
      dat$exposure,
      levels = c(0, 1)
    )
  )
  
  a <- as.numeric(tab["Case", "1"])
  b <- as.numeric(tab["Case", "0"])
  c <- as.numeric(tab["Control", "1"])
  d <- as.numeric(tab["Control", "0"])
  
  correction <- if (any(c(a, b, c, d) == 0)) 0.5 else 0
  
  OR <- (
    (a + correction) * (d + correction)
  ) / (
    (b + correction) * (c + correction)
  )
  
  SE <- sqrt(
    1 / (a + correction) +
      1 / (b + correction) +
      1 / (c + correction) +
      1 / (d + correction)
  )
  
  data.frame(
    exposure = variable,
    cases_exposed = a,
    cases_unexposed = b,
    controls_exposed = c,
    controls_unexposed = d,
    odds_ratio = OR,
    lower_ci = exp(log(OR) - 1.96 * SE),
    upper_ci = exp(log(OR) + 1.96 * SE),
    p_value = tryCatch(
      fisher.test(tab)$p.value,
      error = function(e) NA_real_
    )
  )
}

## 10. FILTER CHOICES

date_min <- min(
  cases$date_of_symptom_onset,
  na.rm = TRUE
)

date_max <- max(
  cases$date_of_symptom_onset,
  na.rm = TRUE
)

subcounty_choices <- sort(
  unique(na.omit(cases$subcounty))
)

village_choices <- sort(
  unique(na.omit(cases$village))
)

sex_choices <- sort(
  unique(na.omit(cases$sex))
)

age_choices <- sort(
  unique(na.omit(cases$age_group))
)

lab_choices <- sort(
  unique(na.omit(cases$laboratory_result))
)

## 11. SHINYDASHBOARD USER INTERFACE

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = tags$span(
      "OUTBREAK INTELLIGENCE",
      style = paste(
        "font-weight:800;",
        "font-size:15px;",
        "color:white;"
      )
    ),
    titleWidth = 330
  ),
  
  dashboardSidebar(
    
    width = 270,
    
    sidebarMenu(
      id = "main_tabs",
      
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Epidemic Curve",
        tabName = "epidemic",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Person Analysis",
        tabName = "person",
        icon = icon("users")
      ),
      
      menuItem(
        "Place Analysis",
        tabName = "place",
        icon = icon("map-marker-alt")
      ),
      
      menuItem(
        "Laboratory",
        tabName = "laboratory",
        icon = icon("flask")
      ),
      
      menuItem(
        "Clinical Outcomes",
        tabName = "clinical",
        icon = icon("heartbeat")
      ),
      
      menuItem(
        "Risk Factors",
        tabName = "risk",
        icon = icon("exclamation-triangle")
      ),
      
      menuItem(
        "Case-Control Analysis",
        tabName = "casecontrol",
        icon = icon("balance-scale")
      ),
      
      menuItem(
        "Data Explorer",
        tabName = "data",
        icon = icon("table")
      ),
      
      menuItem(
        "About DataQuest",
        tabName = "about",
        icon = icon("info-circle")
      )
    ),
    
    br(),
    
    div(
      style = paste(
        "padding: 12px;",
        "background: #1E293B;",
        "margin: 10px;",
        "border-radius: 6px;",
        "color: white;"
      ),
      
      h4(
        "INVESTIGATION FILTERS",
        style = "font-weight:800; color:#F97316;"
      ),
      
      dateRangeInput(
        "date_range",
        "Symptom onset period",
        start = date_min,
        end = date_max,
        min = date_min,
        max = date_max,
        format = "yyyy-mm-dd",
        separator = " to ",
        width = "100%"
      ),
      
      selectizeInput(
        "subcounty",
        "Subcounty",
        choices = subcounty_choices,
        selected = subcounty_choices,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Select subcounties"
        )
      ),
      
      selectizeInput(
        "village",
        "Village",
        choices = village_choices,
        selected = village_choices,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Select villages"
        )
      ),
      
      selectInput(
        "sex",
        "Sex",
        choices = c("All", sex_choices),
        selected = "All"
      ),
      
      selectInput(
        "age_group",
        "Age group",
        choices = c("All", age_choices),
        selected = "All"
      ),
      
      selectInput(
        "lab_status",
        "Laboratory result",
        choices = c("All", lab_choices),
        selected = "All"
      ),
      
      actionButton(
        "reset",
        "Reset all filters",
        icon = icon("refresh"),
        class = "btn-warning btn-block",
        style = "width:100%; margin-top:10px;"
      ),
      
      hr(),
      
      h5(
        "DATASET INFORMATION",
        style = "color:#F97316; font-weight:bold;"
      ),
      
      textOutput("total_records"),
      textOutput("total_villages"),
      textOutput("total_subcounties"),
      
      p(
        "Synthetic outbreak investigation dataset.",
        style = "font-size:11px; color:#CBD5E1;"
      )
    )
  ),
  
  dashboardBody(
    
    tags$head(
      
      tags$title(APP_TITLE),
      
      tags$style(HTML(paste0(
        
        "body { background-color:", BRAND_BACKGROUND, "; }",
        
        ".content-wrapper, .right-side { background-color:",
        BRAND_BACKGROUND, "; }",
        
        ".main-header .logo { background-color:",
        BRAND_NAVY, " !important; }",
        
        ".main-header .navbar { background-color:",
        BRAND_NAVY, " !important; }",
        
        ".main-sidebar { background-color:",
        BRAND_NAVY, " !important; }",
        
        ".sidebar-menu>li.active>a {",
        "border-left-color:", BRAND_ORANGE, ";",
        "background-color:#263F68; }",
        
        ".sidebar-menu>li>a { color:white; }",
        
        ".box { border-top:3px solid ",
        BRAND_ORANGE, "; border-radius:5px; }",
        
        ".box-title { font-weight:700; color:",
        BRAND_NAVY, "; }",
        
        ".small-box { border-radius:6px; }",
        
        ".content-header h1 {",
        "font-weight:800; color:", BRAND_NAVY, "; }",
        
        ".main-footer {",
        "background:#14213D; color:white;",
        "padding:20px; }",
        
        ".main-footer a {",
        "color:#F97316; font-weight:bold;",
        "margin-right:18px; }",
        
        ".data-notice {",
        "background:#FFF7ED; color:#7C2D12;",
        "border-left:5px solid #F97316;",
        "padding:12px; margin:12px 0;",
        "border-radius:4px; }",
        
        ".about-panel {",
        "background:white; padding:20px;",
        "border-radius:6px; margin-bottom:15px; }"
        
      )))
    ),
    
    tabItems(
      
      ## 11.1 OVERVIEW
      
      tabItem(
        tabName = "overview",
        
        h2(
          "Outbreak Situation Overview",
          style = paste0("color:", BRAND_NAVY, "; font-weight:800;")
        ),
        
        fluidRow(
          valueBoxOutput("kpi_cases", width = 3),
          valueBoxOutput("kpi_confirmed", width = 3),
          valueBoxOutput("kpi_deaths", width = 3),
          valueBoxOutput("kpi_hospitalized", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Daily Epidemic Curve",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("overview_epicurve", height = "330px")
          ),
          
          box(
            title = "Cases by Subcounty",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput("overview_subcounty", height = "330px")
          )
        ),
        
        fluidRow(
          box(
            title = "Epidemiological Situation Summary",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            uiOutput("situation_summary")
          )
        ),
        
        div(
          class = "data-notice",
          strong("Data notice: "),
          "This dashboard uses synthetic epidemiological data. ",
          "It is designed for demonstration, training, research and portfolio development. ",
          "It is not a live public health surveillance system."
        )
      ),
      
      ## 11.2 EPIDEMIC CURVE
      
      tabItem(
        tabName = "epidemic",
        
        h2("Temporal Epidemiological Analysis"),
        
        fluidRow(
          box(
            title = "Daily Cases by Symptom Onset",
            status = "primary",
            solidHeader = TRUE,
            width = 7,
            plotlyOutput("epidemic_curve", height = "350px")
          ),
          
          box(
            title = "Cumulative Cases",
            status = "primary",
            solidHeader = TRUE,
            width = 5,
            plotlyOutput("cumulative_curve", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Weekly Cases",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("weekly_cases", height = "300px")
          ),
          
          box(
            title = "Reporting Delay Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("reporting_delay_plot", height = "300px")
          )
        )
      ),
      
      ## 11.3 PERSON ANALYSIS
      
      tabItem(
        tabName = "person",
        
        h2("Person-Level Epidemiological Analysis"),
        
        fluidRow(
          box(
            title = "Cases by Age Group",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("age_plot", height = "300px")
          ),
          
          box(
            title = "Cases by Sex",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("sex_plot", height = "300px")
          )
        ),
        
        fluidRow(
          box(
            title = "Clinical Symptoms",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("symptom_plot", height = "320px")
          ),
          
          box(
            title = "Dehydration Severity",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("dehydration_plot", height = "320px")
          )
        ),
        
        box(
          title = "Filtered Case Line List",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          DTOutput("person_table")
        )
      ),
      
      ## 11.4 PLACE ANALYSIS
      
      tabItem(
        tabName = "place",
        
        h2("Geographical Epidemiological Analysis"),
        
        fluidRow(
          box(
            title = "Cases by Subcounty",
            status = "primary",
            solidHeader = TRUE,
            width = 5,
            plotlyOutput("subcounty_plot", height = "350px")
          ),
          
          box(
            title = "Village Attack Rates",
            status = "primary",
            solidHeader = TRUE,
            width = 7,
            plotlyOutput("attack_rate_plot", height = "350px")
          )
        ),
        
        box(
          title = "Village-Level Epidemiological Indicators",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          DTOutput("village_table")
        ),
        
        p(
          "Attack rates are calculated using the supplied population denominators. ",
          "These denominators are synthetic estimates."
        )
      ),
      
      ## 11.5 LABORATORY ANALYSIS
      
      tabItem(
        tabName = "laboratory",
        
        h2("Laboratory Investigation"),
        
        fluidRow(
          box(
            title = "Laboratory Result Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("lab_plot", height = "320px")
          ),
          
          box(
            title = "Cholera Serogroup Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("serogroup_plot", height = "320px")
          )
        ),
        
        box(
          title = "Laboratory Records",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          DTOutput("lab_table")
        )
      ),
      
      ## 11.6 CLINICAL OUTCOMES
      
      tabItem(
        tabName = "clinical",
        
        h2("Clinical Severity and Patient Outcomes"),
        
        fluidRow(
          box(
            title = "Patient Outcomes",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("outcome_plot", height = "300px")
          ),
          
          box(
            title = "Hospitalization by Dehydration Status",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("hospitalization_plot", height = "300px")
          )
        ),
        
        fluidRow(
          box(
            title = "Treatment Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("treatment_plot", height = "300px")
          ),
          
          box(
            title = "Outcomes by Age Group",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("outcome_age_plot", height = "300px")
          )
        )
      ),
      
      ## 11.7 RISK FACTORS
      
      tabItem(
        tabName = "risk",
        
        h2("Risk Factor Investigation"),
        
        fluidRow(
          box(
            title = "Usual Household Water Source",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("water_plot", height = "320px")
          ),
          
          box(
            title = "Reported Exposure Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("exposure_plot", height = "320px")
          )
        ),
        
        box(
          title = "Exposure Summary",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          DTOutput("exposure_table")
        )
      ),
      
      ## 11.8 CASE-CONTROL ANALYSIS
      
      tabItem(
        tabName = "casecontrol",
        
        h2("Analytical Epidemiology"),
        
        p(
          "Crude odds ratios compare exposure among cases and controls. ",
          "Interpret estimates cautiously because the supplied data are synthetic."
        ),
        
        fluidRow(
          box(
            title = "Crude Odds Ratios and 95% Confidence Intervals",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("or_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Exposure Association Results",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("or_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Exploratory Adjusted Logistic Regression",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("adjusted_table")
          )
        )
      ),
      
      ## 11.9 DATA EXPLORER
      
      tabItem(
        tabName = "data",
        
        h2("Data Management and Exploration"),
        
        tabBox(
          width = 12,
          
          tabPanel(
            "Case Line List",
            br(),
            DTOutput("all_cases")
          ),
          
          tabPanel(
            "Case-Control Dataset",
            br(),
            DTOutput("case_control_table")
          ),
          
          tabPanel(
            "Data Dictionary",
            br(),
            DTOutput("dictionary_table")
          )
        ),
        
        br(),
        
        downloadButton(
          "download_cases",
          "Download Filtered Cases",
          class = "btn-primary"
        ),
        
        downloadButton(
          "download_village",
          "Download Village Summary",
          class = "btn-warning"
        ),
        
        downloadButton(
          "download_controls",
          "Download Case-Control Data",
          class = "btn-success"
        )
      ),
      
      ## 11.10 ABOUT DATAQUEST
      
      tabItem(
        tabName = "about",
        
        h2("About DataQuest Solutions"),
        
        fluidRow(
          box(
            title = "Our Organisation",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            h3(
              "DATAQUEST SOLUTIONS",
              style = paste0(
                "color:", BRAND_NAVY,
                "; font-weight:800;"
              )
            ),
            
            p(
              "DataQuest Solutions is a data science, analytics and technology organisation ",
              "focused on transforming data into actionable insights and better decisions."
            ),
            
            p(
              "We work across data science, biostatistics, epidemiology, ",
              "business intelligence, machine learning and digital solutions."
            ),
            
            p(
              "Our goal is to empower researchers, organisations and business leaders ",
              "to unlock the value of their data."
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Connect with DataQuest Solutions",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            tags$h4("Official Website"),
            
            tags$a(
              href = WEBSITE_URL,
              target = "_blank",
              WEBSITE_URL
            ),
            
            tags$hr(),
            
            tags$h4("GitHub Portfolio"),
            
            tags$a(
              href = GITHUB_URL,
              target = "_blank",
              GITHUB_URL
            ),
            
            tags$hr(),
            
            tags$h4("LinkedIn Profile"),
            
            tags$a(
              href = LINKEDIN_URL,
              target = "_blank",
              LINKEDIN_URL
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Dashboard Information",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            p(strong("Application: "), APP_TITLE),
            p(strong("Organisation: "), ORGANIZATION),
            p(strong("Investigation: "), OUTBREAK_NAME),
            p(strong("Framework: "), "R Shiny and shinydashboard"),
            p(strong("Data: "), "Synthetic outbreak investigation data")
          )
        )
      )
    ),
    
    tags$footer(
      class = "main-footer",
      
      tags$div(
        style = "text-align:center;",
        
        tags$p(
          strong("DATAQUEST SOLUTIONS"),
          " | Epidemiological Intelligence and Decision Support"
        ),
        
        tags$a(
          href = WEBSITE_URL,
          target = "_blank",
          "Website"
        ),
        
        tags$a(
          href = GITHUB_URL,
          target = "_blank",
          "GitHub"
        ),
        
        tags$a(
          href = LINKEDIN_URL,
          target = "_blank",
          "LinkedIn"
        ),
        
        tags$p(
          style = "font-size:11px; color:#CBD5E1;",
          paste0(
            "Synthetic data demonstration | ",
            format(Sys.Date(), "%Y")
          )
        )
      )
    )
  )
)

## 12. SERVER LOGIC

server <- function(input, output, session) {
  
  ## 12.1 FILTER RESET
  
  observeEvent(input$reset, {
    
    updateDateRangeInput(
      session,
      "date_range",
      start = date_min,
      end = date_max
    )
    
    updateSelectizeInput(
      session,
      "subcounty",
      selected = subcounty_choices
    )
    
    updateSelectizeInput(
      session,
      "village",
      selected = village_choices
    )
    
    updateSelectInput(
      session,
      "sex",
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "age_group",
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "lab_status",
      selected = "All"
    )
  })
  
  ## 12.2 CENTRAL FILTER FUNCTION
  ## Each chart can exclude its own dimension to prevent one-category charts.
  
  filter_cases <- function(exclude = character()) {
    
    dat <- cases
    
    if (
      !"date_range" %in% exclude &&
      !is.null(input$date_range) &&
      length(input$date_range) == 2
    ) {
      
      dat <- dat |>
        filter(
          date_of_symptom_onset >= as.Date(input$date_range[1]),
          date_of_symptom_onset <= as.Date(input$date_range[2])
        )
    }
    
    if (!"subcounty" %in% exclude) {
      
      if (length(input$subcounty) > 0) {
        dat <- dat |>
          filter(subcounty %in% input$subcounty)
      } else {
        dat <- dat[0, ]
      }
    }
    
    if (!"village" %in% exclude) {
      
      if (length(input$village) > 0) {
        dat <- dat |>
          filter(village %in% input$village)
      } else {
        dat <- dat[0, ]
      }
    }
    
    if (
      !"sex" %in% exclude &&
      !is.null(input$sex) &&
      input$sex != "All"
    ) {
      dat <- dat |>
        filter(sex == input$sex)
    }
    
    if (
      !"age_group" %in% exclude &&
      !is.null(input$age_group) &&
      input$age_group != "All"
    ) {
      dat <- dat |>
        filter(age_group == input$age_group)
    }
    
    if (
      !"lab_status" %in% exclude &&
      !is.null(input$lab_status) &&
      input$lab_status != "All"
    ) {
      dat <- dat |>
        filter(laboratory_result == input$lab_status)
    }
    
    dat
  }
  
  ## 12.3 GENERAL FILTERED DATA
  
  filtered_cases <- reactive({
    filter_cases()
  })
  
  ## 12.4 FILTERED DATA FOR EACH CHART DIMENSION
  ## A chart excludes the filter matching the variable being plotted.
  
  age_chart_data <- reactive({
    filter_cases(exclude = "age_group")
  })
  
  sex_chart_data <- reactive({
    filter_cases(exclude = "sex")
  })
  
  lab_chart_data <- reactive({
    filter_cases(exclude = "lab_status")
  })
  
  subcounty_chart_data <- reactive({
    filter_cases(exclude = "subcounty")
  })
  
  village_chart_data <- reactive({
    filter_cases(exclude = "village")
  })
  
  ## 12.5 SIDEBAR DATASET INFORMATION
  
  output$total_records <- renderText({
    paste(
      "Total records:",
      format_number(nrow(cases))
    )
  })
  
  output$total_villages <- renderText({
    paste(
      "Villages:",
      length(unique(cases$village))
    )
  })
  
  output$total_subcounties <- renderText({
    paste(
      "Subcounties:",
      length(unique(cases$subcounty))
    )
  })
  
  ## 12.6 KPI VALUE BOXES
  
  output$kpi_cases <- renderValueBox({
    
    dat <- filtered_cases()
    
    valueBox(
      value = format_number(nrow(dat)),
      subtitle = "Reported cases",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$kpi_confirmed <- renderValueBox({
    
    dat <- filtered_cases()
    
    n_confirmed <- sum(
      tolower(as.character(dat$laboratory_result)) == "confirmed",
      na.rm = TRUE
    )
    
    valueBox(
      value = format_number(n_confirmed),
      subtitle = "Laboratory-confirmed cases",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$kpi_deaths <- renderValueBox({
    
    dat <- filtered_cases()
    
    n_deaths <- sum(
      tolower(as.character(dat$outcome)) %in%
        c("died", "death", "deceased"),
      na.rm = TRUE
    )
    
    valueBox(
      value = format_number(n_deaths),
      subtitle = "Reported deaths",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  output$kpi_hospitalized <- renderValueBox({
    
    dat <- filtered_cases()
    
    n_hospitalized <- sum(
      dat$hospitalized == 1,
      na.rm = TRUE
    )
    
    valueBox(
      value = format_number(n_hospitalized),
      subtitle = "Hospitalized cases",
      icon = icon("hospital"),
      color = "yellow"
    )
  })
  
  ## 12.7 EPIDEMIOLOGICAL SITUATION SUMMARY
  
  output$situation_summary <- renderUI({
    
    dat <- filtered_cases()
    
    total_cases <- nrow(dat)
    
    confirmed <- sum(
      tolower(as.character(dat$laboratory_result)) == "confirmed",
      na.rm = TRUE
    )
    
    deaths <- sum(
      tolower(as.character(dat$outcome)) %in%
        c("died", "death", "deceased"),
      na.rm = TRUE
    )
    
    hospitalized <- sum(
      dat$hospitalized == 1,
      na.rm = TRUE
    )
    
    confirmation_rate <- format_percent(
      confirmed,
      total_cases
    )
    
    case_fatality <- format_percent(
      deaths,
      total_cases
    )
    
    hospitalization_rate <- format_percent(
      hospitalized,
      total_cases
    )
    
    tagList(
      
      p(
        strong("Cases in current selection: "),
        format_number(total_cases)
      ),
      
      p(
        strong("Laboratory confirmation proportion: "),
        confirmation_rate
      ),
      
      p(
        strong("Case fatality proportion: "),
        case_fatality
      ),
      
      p(
        strong("Hospitalization proportion: "),
        hospitalization_rate
      ),
      
      p(
        "Interpret these indicators alongside the date and geographic filters. ",
        "The figures are based on synthetic data."
      )
    )
  })
  
  ## 12.8 EPIDEMIC CURVE PLOT FUNCTION
  
  make_epidemic_plot <- function(dat, cumulative = FALSE) {
    
    if (nrow(dat) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    daily <- dat |>
      count(
        date_of_symptom_onset,
        name = "cases"
      ) |>
      arrange(date_of_symptom_onset)
    
    if (cumulative) {
      
      daily <- daily |>
        mutate(
          cumulative_cases = cumsum(cases)
        )
      
      p <- ggplot(
        daily,
        aes(
          x = date_of_symptom_onset,
          y = cumulative_cases,
          text = paste0(
            "Date: ", date_of_symptom_onset,
            "<br>Cumulative cases: ", cumulative_cases
          )
        )
      ) +
        geom_line(
          colour = BRAND_ORANGE,
          linewidth = 1.2
        ) +
        geom_point(
          colour = BRAND_NAVY,
          size = 2
        ) +
        labs(
          x = "Date of symptom onset",
          y = "Cumulative cases"
        )
      
    } else {
      
      p <- ggplot(
        daily,
        aes(
          x = date_of_symptom_onset,
          y = cases,
          text = paste0(
            "Date: ", date_of_symptom_onset,
            "<br>Cases: ", cases
          )
        )
      ) +
        geom_col(
          fill = BRAND_ORANGE,
          alpha = 0.9
        ) +
        labs(
          x = "Date of symptom onset",
          y = "Number of cases"
        )
    }
    
    p <- p +
      dashboard_plot_theme()
    
    make_plotly(p)
  }
  
  output$overview_epicurve <- renderPlotly({
    make_epidemic_plot(filtered_cases())
  })
  
  output$epidemic_curve <- renderPlotly({
    make_epidemic_plot(filtered_cases())
  })
  
  output$cumulative_curve <- renderPlotly({
    make_epidemic_plot(
      filtered_cases(),
      cumulative = TRUE
    )
  })
  
  ## 12.9 WEEKLY CASES
  
  output$weekly_cases <- renderPlotly({
    
    dat <- filtered_cases()
    
    if (nrow(dat) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    weekly <- dat |>
      count(onset_week, name = "cases")
    
    p <- ggplot(
      weekly,
      aes(
        x = onset_week,
        y = cases,
        text = paste0(
          "Week beginning: ", onset_week,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(fill = BRAND_NAVY) +
      labs(
        x = "Week beginning",
        y = "Cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.10 REPORTING DELAY
  
  output$reporting_delay_plot <- renderPlotly({
    
    dat <- filtered_cases() |>
      filter(
        !is.na(reporting_delay),
        reporting_delay >= 0
      )
    
    if (nrow(dat) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    p <- ggplot(
      dat,
      aes(
        x = reporting_delay,
        text = paste0(
          "Reporting delay: ",
          reporting_delay,
          " days"
        )
      )
    ) +
      geom_histogram(
        binwidth = 1,
        fill = BRAND_ORANGE,
        colour = "white"
      ) +
      labs(
        x = "Days between onset and reporting",
        y = "Number of cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.11 AGE DISTRIBUTION
  ## The age filter is excluded so all age groups remain visible.
  
  output$age_plot <- renderPlotly({
    
    dat <- age_chart_data()
    
    if (nrow(dat) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    result <- dat |>
      count(age_group, name = "cases") |>
      arrange(desc(cases))
    
    p <- ggplot(
      result,
      aes(
        x = reorder(age_group, cases),
        y = cases,
        text = paste0(
          "Age group: ", age_group,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(fill = BRAND_ORANGE) +
      coord_flip() +
      labs(
        x = "Age group",
        y = "Cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.12 SEX DISTRIBUTION
  ## The sex filter is excluded so all sex categories remain visible.
  
  output$sex_plot <- renderPlotly({
    
    dat <- sex_chart_data()
    
    if (nrow(dat) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    result <- dat |>
      count(sex, name = "cases")
    
    p <- ggplot(
      result,
      aes(
        x = sex,
        y = cases,
        fill = sex,
        text = paste0(
          "Sex: ", sex,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(
        values = c(
          "#F97316",
          "#14213D",
          "#0F766E",
          "#64748B"
        )
      ) +
      labs(
        x = "Sex",
        y = "Cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.13 CLINICAL SYMPTOMS
  
  output$symptom_plot <- renderPlotly({
    
    dat <- filtered_cases()
    
    symptom_variables <- c(
      "diarrhoea",
      "vomiting",
      "fever",
      "abdominal_cramps"
    )
    
    available <- intersect(
      symptom_variables,
      names(dat)
    )
    
    if (length(available) == 0) {
      return(
        make_plotly(
          empty_plot("No symptom variables available")
        )
      )
    }
    
    result <- data.frame(
      symptom = available,
      cases = sapply(
        available,
        function(v) {
          sum(dat[[v]] == 1, na.rm = TRUE)
        }
      )
    )
    
    result$symptom <- recode(
      result$symptom,
      diarrhoea = "Diarrhoea",
      vomiting = "Vomiting",
      fever = "Fever",
      abdominal_cramps = "Abdominal cramps"
    )
    
    p <- ggplot(
      result,
      aes(
        x = reorder(symptom, cases),
        y = cases,
        text = paste0(
          "Symptom: ", symptom,
          "<br>Cases reporting symptom: ", cases
        )
      )
    ) +
      geom_col(fill = BRAND_ORANGE) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Cases reporting symptom"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.14 DEHYDRATION DISTRIBUTION
  
  output$dehydration_plot <- renderPlotly({
    
    dat <- filtered_cases()
    
    if (!"dehydration_status" %in% names(dat)) {
      return(make_plotly(empty_plot()))
    }
    
    result <- dat |>
      count(dehydration_status, name = "cases")
    
    p <- ggplot(
      result,
      aes(
        x = reorder(dehydration_status, cases),
        y = cases,
        text = paste0(
          "Dehydration: ", dehydration_status,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(fill = BRAND_NAVY) +
      coord_flip() +
      labs(
        x = "Dehydration status",
        y = "Cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.15 SUBCOUNTY DISTRIBUTION
  ## The subcounty filter is excluded so the chart retains all subcounties.
  
  output$overview_subcounty <- renderPlotly({
    
    dat <- subcounty_chart_data()
    
    if (nrow(dat) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    result <- dat |>
      count(subcounty, name = "cases") |>
      arrange(cases)
    
    p <- ggplot(
      result,
      aes(
        x = reorder(subcounty, cases),
        y = cases,
        text = paste0(
          "Subcounty: ", subcounty,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(fill = BRAND_ORANGE) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  output$subcounty_plot <- renderPlotly({
    
    dat <- subcounty_chart_data()
    
    if (nrow(dat) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    result <- dat |>
      count(subcounty, name = "cases") |>
      arrange(cases)
    
    p <- ggplot(
      result,
      aes(
        x = reorder(subcounty, cases),
        y = cases,
        text = paste0(
          "Subcounty: ", subcounty,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(fill = BRAND_NAVY) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.16 VILLAGE ATTACK RATES
  ## The village filter is excluded to retain the full village comparison.
  
  village_summary <- reactive({
    
    dat <- village_chart_data()
    
    result <- dat |>
      count(
        subcounty,
        village,
        name = "cases"
      ) |>
      left_join(
        population,
        by = c("subcounty", "village")
      ) |>
      mutate(
        attack_rate = ifelse(
          !is.na(population_at_risk) &
            population_at_risk > 0,
          100 * cases / population_at_risk,
          NA_real_
        ),
        attack_rate_per_1000 = ifelse(
          !is.na(population_at_risk) &
            population_at_risk > 0,
          1000 * cases / population_at_risk,
          NA_real_
        )
      ) |>
      arrange(desc(attack_rate))
    
    result
  })
  
  output$attack_rate_plot <- renderPlotly({
    
    dat <- village_summary() |>
      filter(!is.na(attack_rate))
    
    if (nrow(dat) == 0) {
      return(
        make_plotly(
          empty_plot("Population denominators unavailable")
        )
      )
    }
    
    dat <- dat |>
      slice_max(
        order_by = attack_rate,
        n = 20,
        with_ties = FALSE
      )
    
    p <- ggplot(
      dat,
      aes(
        x = reorder(village, attack_rate),
        y = attack_rate,
        text = paste0(
          "Village: ", village,
          "<br>Subcounty: ", subcounty,
          "<br>Cases: ", cases,
          "<br>Population: ", population_at_risk,
          "<br>Attack rate: ",
          round(attack_rate, 2), "%"
        )
      )
    ) +
      geom_col(fill = BRAND_ORANGE) +
      coord_flip() +
      labs(
        x = "Village",
        y = "Attack rate (%)"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  output$village_table <- renderDT({
    
    dat <- village_summary()
    
    dat <- dat |>
      mutate(
        attack_rate = round(attack_rate, 2),
        attack_rate_per_1000 = round(
          attack_rate_per_1000,
          2
        )
      )
    
    datatable(
      dat,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      ),
      extensions = "Buttons"
    )
  })
  
  ## 12.17 LABORATORY RESULT DISTRIBUTION
  ## The laboratory filter is excluded so all result categories remain visible.
  
  output$lab_plot <- renderPlotly({
    
    dat <- lab_chart_data()
    
    if (nrow(dat) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    result <- dat |>
      count(laboratory_result, name = "cases")
    
    p <- ggplot(
      result,
      aes(
        x = laboratory_result,
        y = cases,
        fill = laboratory_result,
        text = paste0(
          "Laboratory result: ", laboratory_result,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = "Laboratory result",
        y = "Cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.18 SEROGROUP DISTRIBUTION
  
  output$serogroup_plot <- renderPlotly({
    
    dat <- lab_chart_data()
    
    if (!"vibrio_cholerae_serogroup" %in% names(dat)) {
      return(
        make_plotly(
          empty_plot("Serogroup variable is not available")
        )
      )
    }
    
    dat <- dat |>
      filter(!is.na(vibrio_cholerae_serogroup))
    
    if (nrow(dat) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    result <- dat |>
      count(
        vibrio_cholerae_serogroup,
        name = "cases"
      )
    
    p <- ggplot(
      result,
      aes(
        x = vibrio_cholerae_serogroup,
        y = cases,
        fill = vibrio_cholerae_serogroup,
        text = paste0(
          "Serogroup: ", vibrio_cholerae_serogroup,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = "Serogroup",
        y = "Cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.19 LABORATORY TABLE
  
  output$lab_table <- renderDT({
    
    dat <- lab_chart_data()
    
    lab_columns <- intersect(
      c(
        "case_id",
        "subcounty",
        "village",
        "date_specimen_collected",
        "laboratory_result",
        "vibrio_cholerae_serogroup",
        "specimen_type"
      ),
      names(dat)
    )
    
    datatable(
      dat[, lab_columns, drop = FALSE],
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  ## 12.20 PATIENT OUTCOMES
  
  output$outcome_plot <- renderPlotly({
    
    dat <- filtered_cases()
    
    result <- dat |>
      count(outcome, name = "cases")
    
    if (nrow(result) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    p <- ggplot(
      result,
      aes(
        x = outcome,
        y = cases,
        fill = outcome,
        text = paste0(
          "Outcome: ", outcome,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = "Outcome",
        y = "Cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.21 HOSPITALIZATION BY DEHYDRATION
  
  output$hospitalization_plot <- renderPlotly({
    
    dat <- filtered_cases()
    
    if (!all(
      c("dehydration_status", "hospitalized") %in% names(dat)
    )) {
      return(make_plotly(empty_plot()))
    }
    
    result <- dat |>
      mutate(
        hospitalization = ifelse(
          hospitalized == 1,
          "Hospitalized",
          "Not hospitalized"
        )
      ) |>
      count(
        dehydration_status,
        hospitalization,
        name = "cases"
      )
    
    p <- ggplot(
      result,
      aes(
        x = dehydration_status,
        y = cases,
        fill = hospitalization,
        text = paste0(
          "Dehydration: ", dehydration_status,
          "<br>Hospitalization: ", hospitalization,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(position = "dodge") +
      scale_fill_manual(
        values = c(
          "Hospitalized" = BRAND_ORANGE,
          "Not hospitalized" = BRAND_NAVY
        )
      ) +
      labs(
        x = "Dehydration status",
        y = "Cases",
        fill = "Hospitalization"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.22 TREATMENT DISTRIBUTION
  
  output$treatment_plot <- renderPlotly({
    
    dat <- filtered_cases()
    
    if (!"treatment_received" %in% names(dat)) {
      return(
        make_plotly(
          empty_plot("Treatment variable is not available")
        )
      )
    }
    
    result <- dat |>
      filter(!is.na(treatment_received)) |>
      count(treatment_received, name = "cases")
    
    if (nrow(result) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    p <- ggplot(
      result,
      aes(
        x = reorder(treatment_received, cases),
        y = cases,
        text = paste0(
          "Treatment: ", treatment_received,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(fill = BRAND_NAVY) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.23 OUTCOMES BY AGE GROUP
  
  output$outcome_age_plot <- renderPlotly({
    
    dat <- age_chart_data()
    
    result <- dat |>
      count(
        age_group,
        outcome,
        name = "cases"
      )
    
    if (nrow(result) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    p <- ggplot(
      result,
      aes(
        x = age_group,
        y = cases,
        fill = outcome,
        text = paste0(
          "Age group: ", age_group,
          "<br>Outcome: ", outcome,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(position = "dodge") +
      labs(
        x = "Age group",
        y = "Cases",
        fill = "Outcome"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.24 USUAL WATER SOURCE
  
  output$water_plot <- renderPlotly({
    
    dat <- filtered_cases()
    
    result <- dat |>
      count(water_source_usual, name = "cases") |>
      filter(!is.na(water_source_usual))
    
    if (nrow(result) == 0) {
      return(make_plotly(empty_plot()))
    }
    
    p <- ggplot(
      result,
      aes(
        x = reorder(water_source_usual, cases),
        y = cases,
        text = paste0(
          "Water source: ", water_source_usual,
          "<br>Cases: ", cases
        )
      )
    ) +
      geom_col(fill = "#0F766E") +
      coord_flip() +
      labs(
        x = "Usual water source",
        y = "Cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.25 EXPOSURE DISTRIBUTION
  
  output$exposure_plot <- renderPlotly({
    
    dat <- filtered_cases()
    
    exposure_variables <- c(
      "drank_untreated_water",
      "ate_street_vended_food_7d",
      "attended_large_gathering_7d",
      "household_contact_of_case",
      "travel_outside_village_7d"
    )
    
    exposure_labels <- c(
      drank_untreated_water = "Drank untreated water",
      ate_street_vended_food_7d = "Street-vended food",
      attended_large_gathering_7d = "Large gathering",
      household_contact_of_case = "Household contact",
      travel_outside_village_7d = "Travel outside village"
    )
    
    available <- intersect(
      exposure_variables,
      names(dat)
    )
    
    if (length(available) == 0) {
      return(
        make_plotly(
          empty_plot("No exposure variables are available")
        )
      )
    }
    
    result <- data.frame(
      variable = available,
      cases = sapply(
        available,
        function(v) {
          sum(dat[[v]] == 1, na.rm = TRUE)
        }
      )
    )
    
    result$exposure <- unname(
      exposure_labels[result$variable]
    )
    
    p <- ggplot(
      result,
      aes(
        x = reorder(exposure, cases),
        y = cases,
        text = paste0(
          "Exposure: ", exposure,
          "<br>Exposed cases: ", cases
        )
      )
    ) +
      geom_col(fill = BRAND_ORANGE) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Exposed cases"
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  ## 12.26 EXPOSURE SUMMARY TABLE
  
  output$exposure_table <- renderDT({
    
    dat <- filtered_cases()
    
    exposure_variables <- c(
      "drank_untreated_water",
      "ate_street_vended_food_7d",
      "attended_large_gathering_7d",
      "household_contact_of_case",
      "travel_outside_village_7d"
    )
    
    available <- intersect(
      exposure_variables,
      names(dat)
    )
    
    if (length(available) == 0) {
      return(
        datatable(
          data.frame(Message = "No exposure variables available")
        )
      )
    }
    
    result <- data.frame(
      exposure = available,
      exposed_cases = sapply(
        available,
        function(v) {
          sum(dat[[v]] == 1, na.rm = TRUE)
        }
      ),
      non_exposed_cases = sapply(
        available,
        function(v) {
          sum(dat[[v]] == 0, na.rm = TRUE)
        }
      )
    )
    
    result$exposure <- recode(
      result$exposure,
      drank_untreated_water = "Drank untreated water",
      ate_street_vended_food_7d = "Street-vended food",
      attended_large_gathering_7d = "Attended large gathering",
      household_contact_of_case = "Household contact",
      travel_outside_village_7d = "Travel outside village"
    )
    
    result$total_non_missing <- (
      result$exposed_cases +
        result$non_exposed_cases
    )
    
    result$exposed_percent <- round(
      100 * result$exposed_cases /
        ifelse(result$total_non_missing == 0, NA,
               result$total_non_missing),
      1
    )
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  ## 12.27 CASE-CONTROL ODDS RATIOS
  
  exposure_cc_variables <- c(
    "drank_untreated_water_7d",
    "ate_street_vended_food_7d",
    "attended_large_gathering_7d",
    "household_contact_of_case",
    "travel_outside_village_7d"
  )
  
  odds_ratio_results <- reactive({
    
    if (nrow(case_control) == 0) {
      return(data.frame())
    }
    
    available <- intersect(
      exposure_cc_variables,
      names(case_control)
    )
    
    if (length(available) == 0) {
      return(data.frame())
    }
    
    results <- lapply(
      available,
      function(v) {
        calculate_odds_ratio(case_control, v)
      }
    )
    
    results <- Filter(
      Negate(is.null),
      results
    )
    
    if (length(results) == 0) {
      return(data.frame())
    }
    
    bind_rows(results)
  })
  
  output$or_plot <- renderPlotly({
    
    dat <- odds_ratio_results()
    
    if (nrow(dat) == 0) {
      return(
        make_plotly(
          empty_plot("Case-control exposure data unavailable")
        )
      )
    }
    
    dat <- dat |>
      mutate(
        exposure = recode(
          exposure,
          drank_untreated_water_7d = "Untreated water",
          ate_street_vended_food_7d = "Street-vended food",
          attended_large_gathering_7d = "Large gathering",
          household_contact_of_case = "Household contact",
          travel_outside_village_7d = "Travel outside village"
        )
      )
    
    p <- ggplot(
      dat,
      aes(
        x = odds_ratio,
        y = reorder(exposure, odds_ratio),
        text = paste0(
          "Exposure: ", exposure,
          "<br>Odds ratio: ", round(odds_ratio, 2),
          "<br>95% CI: ",
          round(lower_ci, 2), " to ",
          round(upper_ci, 2),
          "<br>p-value: ",
          signif(p_value, 3)
        )
      )
    ) +
      geom_vline(
        xintercept = 1,
        linetype = "dashed",
        colour = BRAND_RED
      ) +
      geom_errorbar(
        aes(
          xmin = lower_ci,
          xmax = upper_ci
        ),
        width = 0.2,
        orientation = "y"
      ) +
      geom_point(
        colour = BRAND_ORANGE,
        size = 3
      ) +
      scale_x_log10() +
      labs(
        x = "Odds ratio (log scale)",
        y = NULL
      ) +
      dashboard_plot_theme()
    
    make_plotly(p)
  })
  
  output$or_table <- renderDT({
    
    dat <- odds_ratio_results()
    
    if (nrow(dat) == 0) {
      return(
        datatable(
          data.frame(
            Message = "Case-control data or required variables are unavailable."
          )
        )
      )
    }
    
    dat <- dat |>
      mutate(
        exposure = recode(
          exposure,
          drank_untreated_water_7d = "Untreated water",
          ate_street_vended_food_7d = "Street-vended food",
          attended_large_gathering_7d = "Large gathering",
          household_contact_of_case = "Household contact",
          travel_outside_village_7d = "Travel outside village"
        ),
        odds_ratio = round(odds_ratio, 2),
        lower_ci = round(lower_ci, 2),
        upper_ci = round(upper_ci, 2),
        p_value = signif(p_value, 3)
      )
    
    datatable(
      dat,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  ## 12.28 ADJUSTED LOGISTIC REGRESSION
  ## This is an exploratory model and does not account for matching or clustering.
  
  output$adjusted_table <- renderDT({
    
    if (nrow(case_control) == 0) {
      return(
        datatable(
          data.frame(
            Message = "Case-control dataset unavailable."
          )
        )
      )
    }
    
    if (!"participant_type" %in% names(case_control)) {
      return(
        datatable(
          data.frame(
            Message = "participant_type variable is unavailable."
          )
        )
      )
    }
    
    available <- intersect(
      exposure_cc_variables,
      names(case_control)
    )
    
    if (length(available) == 0) {
      return(
        datatable(
          data.frame(
            Message = "No exposure variables available for modelling."
          )
        )
      )
    }
    
    model_data <- case_control |>
      mutate(
        case_status = ifelse(
          tolower(as.character(participant_type)) == "case",
          1,
          ifelse(
            tolower(as.character(participant_type)) == "control",
            0,
            NA_real_
          )
        )
      )
    
    for (v in available) {
      model_data[[v]] <- to_binary(model_data[[v]])
    }
    
    model_data <- model_data |>
      select(
        case_status,
        all_of(available)
      ) |>
      drop_na()
    
    if (
      nrow(model_data) < 10 ||
      length(unique(model_data$case_status)) < 2
    ) {
      return(
        datatable(
          data.frame(
            Message = "Insufficient complete observations for logistic regression."
          )
        )
      )
    }
    
    formula_model <- as.formula(
      paste(
        "case_status ~",
        paste(available, collapse = " + ")
      )
    )
    
    fitted_model <- tryCatch(
      glm(
        formula_model,
        data = model_data,
        family = binomial()
      ),
      error = function(e) NULL
    )
    
    if (is.null(fitted_model)) {
      return(
        datatable(
          data.frame(
            Message = "The logistic regression model could not be estimated."
          )
        )
      )
    }
    
    result <- broom::tidy(
      fitted_model,
      exponentiate = TRUE,
      conf.int = TRUE
    ) |>
      filter(term != "(Intercept)") |>
      mutate(
        odds_ratio = round(estimate, 3),
        lower_ci = round(conf.low, 3),
        upper_ci = round(conf.high, 3),
        p_value = signif(p.value, 3)
      ) |>
      select(
        term,
        odds_ratio,
        lower_ci,
        upper_ci,
        p_value
      )
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  ## 12.29 PERSON LINE LIST
  
  output$person_table <- renderDT({
    
    dat <- filtered_cases()
    
    datatable(
      dat,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    )
  })
  
  ## 12.30 COMPLETE CASE LINE LIST
  
  output$all_cases <- renderDT({
    
    dat <- filtered_cases()
    
    datatable(
      dat,
      rownames = FALSE,
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    )
  })
  
  ## 12.31 CASE-CONTROL DATA TABLE
  
  output$case_control_table <- renderDT({
    
    if (nrow(case_control) == 0) {
      return(
        datatable(
          data.frame(
            Message = "Case-control dataset was not supplied."
          )
        )
      )
    }
    
    datatable(
      case_control,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  
  ## 12.32 DATA DICTIONARY
  
  output$dictionary_table <- renderDT({
    
    if (is.null(dictionary_raw)) {
      return(
        datatable(
          data.frame(
            Message = "Data dictionary file was not supplied."
          )
        )
      )
    }
    
    datatable(
      dictionary_raw,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  
  ## 12.33 DOWNLOAD FILTERED CASES
  
  output$download_cases <- downloadHandler(
    
    filename = function() {
      paste0(
        "dataquest_filtered_cases_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write.csv(
        filtered_cases(),
        file,
        row.names = FALSE
      )
    }
  )
  
  ## 12.34 DOWNLOAD VILLAGE SUMMARY
  
  output$download_village <- downloadHandler(
    
    filename = function() {
      paste0(
        "dataquest_village_summary_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write.csv(
        village_summary(),
        file,
        row.names = FALSE
      )
    }
  )
  
  ## 12.35 DOWNLOAD CASE-CONTROL DATA
  
  output$download_controls <- downloadHandler(
    
    filename = function() {
      paste0(
        "dataquest_case_control_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      write.csv(
        case_control,
        file,
        row.names = FALSE
      )
    }
  )
}

## 13. RUN APPLICATION

shinyApp(
  ui = ui,
  server = server
)
