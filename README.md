# Outbreak Investigation Dashboard

### An Interactive Public Health Surveillance and Epidemiological Analysis Platform

<p align="center">
  <a href="https://enochosen.shinyapps.io/outbreak-investigation-dashboard/"><strong>🚀 Launch Live Dashboard</strong></a>
  &nbsp; | &nbsp;
  <a href="https://github.com/Biosticianenoch/outbreak-investigation-dashboard"><strong>View Source Code</strong></a>
  &nbsp; | &nbsp;
  <a href="https://www.dataquestsolutions.co.ke/"><strong>DataQuest Solutions</strong></a>
</p>

---

## Project Overview

The **Outbreak Investigation Dashboard** is an interactive public health surveillance and epidemiological analysis application developed using R and Shiny. It provides an integrated analytical environment for examining outbreak patterns, characterizing affected populations, assessing geographical distribution, exploring laboratory findings, evaluating clinical outcomes, and investigating potential exposure-related risk factors.

The platform transforms structured epidemiological datasets into interactive visualizations, statistical summaries, and analytical outputs that support outbreak investigation, surveillance monitoring, and evidence-informed public health decision-making.

Designed around the core principles of descriptive and analytical epidemiology, the dashboard organizes outbreak information according to **person, place, and time**, while incorporating laboratory, clinical, and case-control analytical dimensions.

The application is intended for epidemiologists, biostatisticians, public health professionals, disease surveillance officers, researchers, and health data scientists.

**Live Application:**  
https://enochosen.shinyapps.io/outbreak-investigation-dashboard/

## Project Objectives

The dashboard was developed to:

- Provide an integrated environment for exploring outbreak investigation data.
- Monitor temporal patterns in reported cases using epidemic curves.
- Characterize affected populations through demographic and clinical indicators.
- Examine geographical distribution and variation in disease burden.
- Explore laboratory findings and confirmation patterns.
- Assess hospitalization, mortality, and other clinical outcomes.
- Investigate potential exposure-related risk factors.
- Conduct case-control analysis using odds ratios and regression-based approaches.
- Improve accessibility of epidemiological information through interactive visualization.
- Support evidence-informed public health investigation and response.

## Key Features and Analytical Modules

### 1. Epidemiological Overview

Provides a high-level summary of outbreak indicators through interactive dashboard components.

Key indicators include:

- Total reported cases.
- Laboratory-confirmed cases.
- Reported deaths.
- Hospitalized cases.

The overview provides an entry point for understanding the distribution and characteristics of the outbreak.

### 2. Epidemic Curve Analysis

Examines the temporal distribution of reported cases and supports the identification of patterns in outbreak progression.

Analytical capabilities include:

- Epidemic curve visualization.
- Distribution of cases by onset date.
- Temporal trends in reported cases.
- Reporting delay analysis.
- Exploration of changes in case occurrence over time.

These outputs support the interpretation of outbreak dynamics and the identification of periods of increased case occurrence.

### 3. Person Analysis

Examines the demographic and individual characteristics of affected populations.

Variables explored include:

- Age and age groups.
- Sex distribution.
- Demographic characteristics.
- Reported symptoms.
- Clinical presentation.

This module supports the identification of population groups experiencing different levels of disease burden.

### 4. Place Analysis

Explores the geographical distribution of reported cases across administrative and community-level locations.

Analytical outputs include:

- Cases by sub-county.
- Cases by village.
- Population-based attack rates.
- Geographical variation in reported disease burden.

Population denominators are incorporated to support the calculation and comparison of attack rates.

### 5. Laboratory Analysis

Provides an analytical view of laboratory investigation findings.

Features include:

- Laboratory result distributions.
- Confirmed and non-confirmed case comparisons.
- Serogroup distribution where available.
- Exploration of laboratory-related indicators.

### 6. Clinical Outcomes Analysis

Examines clinical characteristics and outcomes among reported cases.

Indicators include:

- Hospitalization.
- Mortality.
- Dehydration.
- Treatment-related information.
- Clinical outcomes.

This module supports the assessment of disease severity and the distribution of clinical outcomes.

### 7. Risk Factor Analysis

Explores potential exposure-related factors associated with outbreak occurrence.

The dashboard supports examination of variables such as:

- Water sources.
- Environmental and exposure characteristics.
- Other recorded risk-related variables.

These analyses help identify exposure patterns that may warrant further epidemiological investigation.

### 8. Case-Control Analysis

Provides an analytical environment for examining associations between recorded exposures and case status.

Statistical approaches include:

- Exposure frequency comparisons.
- Crude odds ratios.
- Adjusted exploratory logistic regression.
- Interpretation of exposure-outcome associations.

The outputs support hypothesis generation and further epidemiological investigation. Statistical associations should not automatically be interpreted as causal relationships.

### 9. Interactive Data Explorer

Allows users to explore the underlying investigation datasets through interactive data tables.

Users can examine records and investigate the information supporting the dashboard's analytical outputs.

### 10. About DataQuest Solutions

Includes information about the organization behind the project and links to its professional and digital platforms.

## Interactive Dashboard Controls

The application includes interactive filtering functionality to support focused analysis.

Available filtering dimensions include:

- Date range.
- Sub-county.
- Village.
- Sex.
- Age group.
- Laboratory result.

The filters allow users to examine specific population groups, geographical areas, and time periods without manually restructuring the underlying datasets.

## Technology Stack

| Technology | Application |
|---|---|
| R | Statistical computing and analytical programming |
| Shiny | Interactive web application development |
| shinydashboard | Dashboard layout and navigation |
| ggplot2 | Statistical visualization |
| Plotly | Interactive charts and graphics |
| dplyr | Data manipulation and transformation |
| tidyr | Data restructuring |
| DT | Interactive data tables |
| lubridate | Date and time processing |
| scales | Visualization scales and formatting |
| broom | Statistical model output organization |
| stringr | String processing |
| forcats | Categorical variable management |

## Project Structure

The repository is organized around the Shiny application and its supporting epidemiological datasets.

```text
outbreak-investigation-dashboard/
│
├── app.R
│
├── case_line_list.csv
├── case_control_exposures.csv
├── daily_epidemic_curve.csv
├── population_denominators.csv
├── data_dictionary.csv
│
├── README.md
├── README.txt
│
└── rsconnect/
    └── shinyapps.io/
        └── enochosen/
            └── outbreak-investigation-dashboard.dcf
```

### Dataset Descriptions

| Dataset | Description |
|---|---|
| `case_line_list.csv` | Individual-level outbreak investigation records and case characteristics. |
| `case_control_exposures.csv` | Exposure information used for case-control analysis. |
| `daily_epidemic_curve.csv` | Daily case information supporting temporal analysis. |
| `population_denominators.csv` | Population denominators supporting population-based epidemiological indicators. |
| `data_dictionary.csv` | Documentation of dataset variables and their descriptions. |

The datasets are maintained separately from the application code to support organized data management and reproducible analytical workflows.

## Statistical and Epidemiological Methods

The dashboard incorporates analytical approaches commonly used in descriptive and analytical epidemiology.

| Analytical area | Method or indicator |
|---|---|
| Descriptive epidemiology | Person, place and time analysis |
| Temporal analysis | Epidemic curve analysis |
| Population-level assessment | Attack rate calculations |
| Laboratory epidemiology | Laboratory result distributions |
| Clinical epidemiology | Hospitalization and mortality summaries |
| Exposure assessment | Exposure frequency comparisons |
| Analytical epidemiology | Crude odds ratios |
| Regression analysis | Exploratory logistic regression |

### Epidemiological Interpretation

The analytical outputs should be interpreted within the context of the investigation design, data completeness, case definitions, sampling procedures, and population denominators.

In particular:

- Attack rates depend on the accuracy and appropriateness of population denominators.
- Epidemic curves depend on the quality and completeness of onset-date information.
- Odds ratios describe associations between exposures and case status.
- Adjusted regression estimates depend on model specification, variable selection, sample size, and underlying data quality.
- Observational associations do not independently establish causality.

The dashboard is an analytical support tool and does not replace epidemiological field investigation, laboratory confirmation, clinical judgment, or established public health response protocols.

## Installation and Local Setup

You can run the application locally using R and RStudio.

### Prerequisites

- R (version 4.6 or a compatible version).
- RStudio Desktop (recommended).
- Git (for cloning the repository).
- Internet connectivity for initial package installation.

### 1. Clone the repository

Open your terminal or RStudio Terminal and run:

```bash
git clone https://github.com/Biosticianenoch/outbreak-investigation-dashboard.git
```

### 2. Navigate to the project directory

```bash
cd outbreak-investigation-dashboard
```

Alternatively, open the downloaded project folder directly in RStudio.

### 3. Install required packages

Run the following commands in the R console:

```r
packages <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "dplyr",
  "tidyr",
  "ggplot2",
  "plotly",
  "lubridate",
  "scales",
  "broom",
  "stringr",
  "forcats"
)

installed <- rownames(installed.packages())

for (pkg in packages) {
  if (!pkg %in% installed) {
    install.packages(pkg)
  }
}
```

### 4. Launch the application

From the project directory, execute:

```r
shiny::runApp()
```

The application will start locally and provide a browser link, typically:

```text
http://127.0.0.1:xxxx
```

## Live Deployment

The dashboard is deployed using **shinyapps.io**, making it accessible through a web browser without requiring users to install R or RStudio.

<details>
<summary>View live application</summary>

https://enochosen.shinyapps.io/outbreak-investigation-dashboard/

</details>

**Access the dashboard:**  
[Launch Outbreak Investigation Dashboard](https://enochosen.shinyapps.io/outbreak-investigation-dashboard/)

## Intended Users

The application is relevant to professionals and organizations working in:

- Epidemiology and disease surveillance.
- Public health research.
- Biostatistics and statistical analysis.
- Outbreak investigation and response.
- Health information management.
- Laboratory surveillance.
- Monitoring and evaluation.
- Health data science.
- Academic and research institutions.
- Public health programme management.

## Potential Applications

The dashboard provides a foundation for several public health analytical activities:

1. Routine exploration of outbreak investigation datasets.
2. Monitoring temporal patterns in reported cases.
3. Comparing disease burden across geographical locations.
4. Examining demographic patterns in affected populations.
5. Reviewing laboratory and clinical investigation findings.
6. Investigating potential exposure-related factors.
7. Supporting analytical reporting and research communication.
8. Facilitating evidence-informed public health discussions.

## Data Governance and Responsible Use

Public health data may contain sensitive personal and health information.

Before using or adapting this application with operational investigation datasets, users should:

- Ensure appropriate authorization for data access and processing.
- Remove or protect personally identifiable information.
- Apply appropriate data security and access controls.
- Follow applicable data protection and research ethics requirements.
- Validate case definitions, variables, and population denominators.
- Review analytical outputs before using them to inform public health decisions.

Users are responsible for ensuring that datasets uploaded to or deployed through any hosting platform comply with applicable data protection requirements and the platform's terms of service.

## Future Development Opportunities

Potential extensions to the platform include:

- Geospatial outbreak mapping using GIS integration.
- Automated surveillance reporting.
- Real-time data integration from approved surveillance systems.
- Advanced time-series forecasting.
- Spatial epidemiological modelling.
- Bayesian outbreak analysis.
- Automated statistical reporting.
- Integration with KoboToolbox and other digital data collection platforms.
- Role-based access and enhanced data security.
- Expanded epidemiological modelling and risk prediction.

These are potential development directions and are not necessarily implemented in the current release.

## About the Developer

**Enock Bereka**  
*Health Data Scientist | Epidemiologist | Biostatistician | Founder, DataQuest Solutions*

I work at the intersection of epidemiology, biostatistics, data science, and technology, applying statistical computing and analytical methods to transform health data into meaningful insights.

My technical interests and professional work include:

- Health data science and biostatistics.
- Epidemiological research and disease surveillance.
- Statistical modelling and machine learning.
- Bayesian statistical analysis.
- Time-series and spatial analysis.
- Interactive dashboards and business intelligence.
- Digital data collection and health information systems.
- Data-driven public health decision-making.

This project reflects my commitment to developing practical analytical tools that strengthen the use of data in public health research, surveillance, and decision-making.

## Connect With Me

<p align="center">
  <a href="https://www.linkedin.com/in/enock-bereka/"><strong>LinkedIn</strong></a>
  &nbsp; | &nbsp;
  <a href="https://github.com/Biosticianenoch"><strong>GitHub</strong></a>
  &nbsp; | &nbsp;
  <a href="https://www.dataquestsolutions.co.ke/"><strong>Website</strong></a>
  &nbsp; | &nbsp;
  <a href="https://enochosen.shinyapps.io/outbreak-investigation-dashboard/"><strong>Live Dashboard</strong></a>
</p>

### DataQuest Solutions

**I Teach. I Analyze. I Empower Decisions.**

DataQuest Solutions is a data science, analytics, and technology initiative focused on empowering individuals, researchers, and organizations to unlock the value of data through analytical solutions, training, and technology.

Website: https://www.dataquestsolutions.co.ke/

---

## Acknowledgement

Developed by **Enock Bereka – Data Scientist**, under the DataQuest Solutions brand.

If you find this project useful for public health analytics, epidemiological research, or data science development, consider starring the repository and sharing it with fellow researchers and public health professionals.

**Repository:** https://github.com/Biosticianenoch/outbreak-investigation-dashboard

**Live Application:** https://enochosen.shinyapps.io/outbreak-investigation-dashboard/

---

<p align="center">
  <strong>DataQuest Solutions</strong><br>
  <em>Turning Data into Insights. Empowering Decisions.</em>
</p>
