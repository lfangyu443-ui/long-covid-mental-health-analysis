# Long COVID and Mental Health: Depression and Anxiety Analysis

Analysis of the association between Long COVID and mental health outcomes (depression and anxiety) using 2023 National Health Interview Survey (NHIS) data (n = 15,158).

---

## Overview

This project investigates whether Long COVID status is significantly associated with depression and anxiety disorder among U.S. adults, using data from the 2023 NHIS. Both descriptive statistics and multivariable logistic regression models were used to examine these associations while adjusting for key sociodemographic covariates.

---

## Research Questions

- Is Long COVID significantly associated with depression?
- Is Long COVID significantly associated with anxiety disorder?
- Do these associations persist after adjusting for age, sex, education, income, and insurance status?

---

## Data

- **Source:** [National Health Interview Survey (NHIS), 2023](https://www.cdc.gov/nchs/nhis/index.htm)
- **Provider:** National Center for Health Statistics (NCHS), CDC
- **Sample size:** 15,158 adult participants
- **Long COVID prevalence:** 2,375 participants (15.7%)

> **Note:** The raw data file is not included in this repository. It can be downloaded directly from the [CDC NHIS website](https://www.cdc.gov/nchs/nhis/2023nhis.htm).

---

## Methods

- **Study design:** Cross-sectional analysis
- **Exposure:** Long COVID status (yes/no)
- **Outcomes:** Ever diagnosed with depression; ever diagnosed with anxiety disorder
- **Covariates:** Age group, sex, education level, income-to-poverty ratio, health insurance status
- **Statistical tests:**
  - Pearson's chi-squared test for bivariate associations
  - Multivariable logistic regression for adjusted odds ratios
- **Software:** R / Python *(see `/code` folder)*

---

## Key Findings

### Depression
- **32.4%** of Long COVID participants reported depression vs. **19.2%** in the No Long COVID group (p < 0.001)
- After adjustment, Long COVID was associated with **83% higher odds** of depression (OR = 1.83, 95% CI: 1.66–2.02, p < 0.001)

### Anxiety
- **30.9%** of Long COVID participants reported anxiety vs. **18.7%** in the No Long COVID group (p < 0.001)
- After adjustment, Long COVID was associated with **79% higher odds** of anxiety disorder (OR = 1.79, 95% CI: 1.61–1.98, p < 0.001)

---

## Repository Structure

```
long-covid-mental-health-analysis/
├── README.md
├── report/
│   └── report.pdf               # Full written report with tables
├── code/
│   └── analysis.R               # Data cleaning, analysis, regression models
└── data/
    └── README.md                # Instructions for downloading NHIS data
```

---

## Tables

| Table | Description |
|-------|-------------|
| Table 1 | Participant characteristics by Long COVID status |
| Table 2 | Association between Long COVID and depression |
| Table 3 | Association between Long COVID and anxiety disorder |
| Table 4 | Multivariable logistic regression: Long COVID and depression |
| Table 5 | Multivariable logistic regression: Long COVID and anxiety disorder |

---

## How to Run the Code

1. Download the 2023 NHIS dataset from the [CDC website](https://www.cdc.gov/nchs/nhis/2023nhis.htm) and place it in the `/data` folder
2. Open `code/analysis.R` (or `.py`) in RStudio / your Python environment
3. Install required packages (see dependencies below)
4. Run the script from top to bottom

### Dependencies

**R:**
```r
install.packages(c("tidyverse", "survey", "gtsummary", "broom"))
```

**Python:**
```bash
pip install pandas scipy statsmodels
```

---

## Author

*Add your name, course, and institution here.*

---

## License

This project is for academic purposes. Data is publicly available from the CDC NHIS.
