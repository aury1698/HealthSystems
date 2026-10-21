# Data Science for Health Systems: Exploratory Analysis of the 2021 BRFSS Survey

An exploratory data science and statistical analysis project investigating how **25 selected lifestyle, demographic, and clinical features** relate to self-reported **diabetes diagnoses** across U.S. adults using the CDC's 2021 **Behavioral Risk Factor Surveillance System (BRFSS)** dataset.

Developed for the **Data Science for Health Systems** course at the **University of Perugia (UniPG)** – Department of Engineering.

---

## Project Overview

Diabetes is a major global public health condition. The **BRFSS** is the world's largest ongoing health telephone survey, interviewing over 400,000 U.S. residents annually across 15 core health sections.

This project performs a rigorous statistical pipeline on the **2021 BRFSS dataset** (originally containing 303 features and 438,693 records) to:
1. Extract and clean a curated subset of **25 key health indicators** (e.g., BMI, High Blood Pressure, Cholesterol, Physical Activity, Smoking, General Health, Mental/Physical Health days).
2. Compare three cohort populations: **Diabetes**, **PreDiabetes**, and **No Diabetes**.
3. Evaluate statistical relationships using **Chi-Squared**, **Fisher's Exact**, **Kruskal-Wallis**, **Dunn's Post-Hoc**, **Anderson-Darling**, and correlation matrices (Spearman & Pearson).
4. Identify structural dataset limitations (such as questionnaire skip-patterns and non-normality driven by massive sample size) that impact predictive health modeling.

---

## Repository Structure

```text
├── R src/                                # Main R source codebase
│   ├── DatasetProcessing.R               # Data extraction, feature selection & cleaning pipeline
│   ├── Diabetes.R                        # Target variable EDA and cohort distribution analysis
│   ├── Categorical.R                     # Statistical testing for categorical health variables
│   ├── Ordinal.R                         # Non-parametric tests & correlation for ordinal variables
│   ├── Numerical.R                       # Normality checks, Pearson correlation & non-parametric tests
│   ├── R src.Rproj                       # RStudio Project configuration
│   └── Readme.md                         # Detailed R script documentation
├── data/                                 # dataset directory 
├── results/                              # Output tables & statistical test results
│   └── chiSquare_fisher_results.csv      # Exported Chi-Square and Fisher exact test p-values
├── examples/                             # In-class statistical analysis scripts & reference materials
├── Data_Science_for_Health_System.pdf    # Full academic project report (IEEE format)
├── .gitignore                            # Git ignore rules
└── README.md                             # Project documentation
```

---

## R Source Scripts Breakdown (`R src/`)

The core analytical pipeline is modularized into dedicated R scripts in the `R src/` directory:

### 1. `DatasetProcessing.R`
- Downloads and unpacks the raw 2021 BRFSS SAS transport dataset (`LLCP2021XPT.zip`).
- Reduces the 303 original survey features down to **25 significant health indicators** across demographics, habits, and chronic conditions.
- Handles survey code conventions (re-encoding missing/refusal values `9`, `99`, `999` to baseline levels `0`, `31`, or `42069` for optional modules).

### 2. `Diabetes.R`
- Focuses on the target diagnosis variable (`DIABETE4`), categorizing subjects into **Diabetes (14.0%)**, **No Diabetes (83.5%)**, and **PreDiabetes (2.3%)**.
- Generates baseline exploratory plots, bar charts, and frequency distribution tables.

### 3. `Categorical.R`
- **Analyzed Features**: `HighBloodPressure`, `ToldHighCholesterol`, `HeartDisease`, `Stroke`, `PhysicalActivity`, `Fruit`, `Vegetables`, `HeavyDrinker`, `HealthPlan`, `MedicalCost`, `WalkingDifficulty`, `Sex`.
- **Statistical Methods**:
  - **Spearman Rank Correlation**: Measures pairwise associations among categorical risk factors.
  - **$\chi^2$ Test of Independence & Fisher's Exact Test**: Evaluates independence across all 78 feature pairs.

### 4. `Ordinal.R`
- **Analyzed Features**: `CholesterolCheck`, `BMI`, `Smoker`, `GeneralHealth`, `Age`, `Education`, `Income`, `Checkup`.
- **Statistical Methods**:
  - **Spearman Rank Correlation**: Assesses monotonic relationships across ordered risk brackets.
  - **Kruskal-Wallis Test**: Evaluates median differences across the three diabetes cohorts.
  - **Dunn's Post-Hoc Test with Bonferroni Correction**: Identifies specific pairwise cohort differences.

### 5. `Numerical.R`
- **Analyzed Features**: `MentalHealth`, `PhysicalHealth`, `BloodSugar` (annual checks), `FeetCheck` (annual checks).
- **Statistical Methods**:
  - **Pearson Correlation Matrix**: Assesses linear dependencies with diabetes status.
  - **Q-Q Plots & Anderson-Darling (AD) Test**: Rigorously tests for normality (confirming non-normal distributions due to massive sample sizes).
  - **Kruskal-Wallis & Dunn Tests**: Non-parametric evaluation of health-day metrics across cohorts.

---

## Key Findings & Dataset Limitations

1. **Health Correlations**: Diabetes status strongly correlates with elevated BMI, diagnosed hypertension, high cholesterol, physical inactivity, and walking difficulty.
2. **Sample Size Effect**: Extremely low p-values ($p < 2.2 \times 10^{-16}$, R's numerical precision floor) demonstrate that massive survey sizes detect micro-variations but violate standard normality assumptions.
3. **Structural Missingness**: Questionnaire skip-patterns (e.g., Prediabetes optional module questions for `BloodSugar` and `FeetCheck`) create conditional missingness (`NA` values in non-diabetic cohorts), preventing uniform multi-cohort statistical testing on those specific variables.
4. **Takeaway**: Single-year survey snapshots are valuable for population-level monitoring, but multi-year data pooling is necessary for robust predictive modeling in diabetes prevention.

---


## Authors & Academic Context

**Course**: Data Science for Health Systems  
**Institution**: Department of Engineering, University of Perugia (UniPG)  

**Team Members**:
- **Nicola Cucina** (`nicola.cucina@studenti.unipg.it`)
- **Aurora Traversini** (`aurora.traversini@studenti.unipg.it`)


## Sources: 

- CDC Diabetes Health Indicators from UC Irvine Machine Learning Repository https://archive.ics.uci.edu/dataset/891/cdc+diabetes+health+indicators
- BRFSS 2015 Cleaned Dataset from Kaggle https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset
- BRFSS Datasets from 2011 to 2015 https://www.kaggle.com/datasets/cdc/behavioral-risk-factor-surveillance-system/data
- EDA Example https://www.kaggle.com/code/bharat04/diabetes-detailed-eda-with-conclusion 
- CDC BRFSS 2021 Data and Documentation https://www.cdc.gov/brfss/annual_data/annual_2021.html
- R per l’analisi statistica multivariata https://tommasorigon.github.io/introR/lezioni/un_G.html
- Utile per Chi quadro e funzioni in R in generale https://www.geeksforgeeks.org/chi-square-test-in-r/
