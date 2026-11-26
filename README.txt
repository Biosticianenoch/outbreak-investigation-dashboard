# Outbreak Investigation Dashboard — Synthetic Dataset

## Scenario
A fictional cholera outbreak in "Mwangaza County" (synthetic), Kenya, from 2026-08-01 to 2026-09-25.
All records are simulated for dashboard development, teaching and portfolio demonstration. They do not describe a real outbreak or real people.

## Files
1. `case_line_list.csv` — 600 case records with demographic, clinical, laboratory, outcome, time, place and exposure variables.
2. `population_denominators.csv` — 18 village-level population denominators for calculating crude village attack rates.
3. `case_control_exposures.csv` — 1,800 analytic participants: 600 cases and 1,200 controls (2 controls per case), with exposure variables for exploratory case-control analysis.
4. `daily_epidemic_curve.csv` — 56 daily records including new and cumulative cases.
5. `data_dictionary.csv` — variable descriptions and file locations.

## Suggested dashboard modules
- Overview: total cases, confirmed cases, deaths, case-fatality proportion, hospitalization proportion, and reporting delay.
- Time: epidemic curve by symptom onset, cumulative cases, onset-to-report delay, weekly trend.
- Person: age group, sex, pregnancy status, symptom profile and clinical severity.
- Place: cases by subcounty/village; join case counts to population denominators by `village` and calculate crude attack rate per 1,000.
- Laboratory: confirmed/negative/pending proportions and serogroup distribution.
- Clinical outcomes: hospitalization, dehydration, treatment and outcome.
- Exposures: water source and reported exposures among cases.
- Analytic study: compare cases and controls in `case_control_exposures.csv`; calculate crude odds ratios and confidence intervals for exposure variables. Interpret cautiously because this is simulated, frequency/matched-set style data and the sample design is simplified.

## Important methodological notes
- The synthetic case line list contains cases only; it is not a population cohort. Do not calculate an attack rate by dividing cases by the number of rows in the line list.
- Use `population_denominators.csv` as the denominator for crude village attack rates. These denominators are illustrative simulated population estimates, not official census figures.
- Overall attack rate can be calculated as total cases divided by the sum of the population denominators, multiplied by 1,000, only if the selected population and case ascertainment period are treated as aligned.
- Case fatality proportion = deaths / cases with known outcome × 100.
- The case-control dataset is for exploratory association analysis. For formal inference, account for the sampling/matching design and verify the case definition, control selection, exposure windows and missingness.
- `1` means yes and `0` means no for binary indicator variables.
- Blank date fields indicate not applicable/not recorded, especially admission date for non-hospitalized cases.
- The operational classification is illustrative, not a substitute for current national or WHO case definitions and local public health protocols.
