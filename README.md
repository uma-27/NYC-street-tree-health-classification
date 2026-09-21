# NYC Street Tree Health Classification

## Overview

This project explores whether observable tree characteristics, site conditions and geographic information can be used to classify the **perceived health of living New York City street trees** as **Good, Fair or Poor**.

The analysis is based on the **2015 Street Tree Census – Tree Data** from NYC Open Data. The project was developed for **STAT5003** and focuses on exploratory data analysis, data quality assessment, classification planning and responsible interpretation of observational data.

## Research Question

> To what extent can observable tree characteristics, site conditions and geographic information classify the perceived health of living New York City street trees as Good, Fair or Poor?

The intended use is as a **screening and prioritisation aid**, not as an arboricultural diagnosis. The health label reflects a census worker's perceived assessment, and associations in the data should not be interpreted as causal effects.

## Dataset

Source: **NYC Open Data – 2015 Street Tree Census – Tree Data**

The raw dataset contains approximately **683,000 records and 45 variables**. For the classification cohort, the analysis is restricted to living trees with recorded Good, Fair or Poor health labels, leaving **652,172 observations**.

Important data characteristics identified in the analysis include:

- strong class imbalance, with Good-health trees forming the majority;
- structural missingness for condition-related fields among dead trees and stumps;
- potentially implausible or extreme diameter observations requiring sensitivity analysis;
- concentration of observations among a smaller set of common species;
- geographic, species and problem-burden relationships that may be informative for classification.

## Analysis Workflow

The project includes:

1. **Raw-data audit**
   - row and column counts
   - unique tree IDs
   - variable types
   - tree-status distribution

2. **Missing-data analysis**
   - overall missingness
   - missingness by tree status
   - distinction between structural and potentially random missingness

3. **Classification cohort definition**
   - retain living trees only
   - retain Good, Fair and Poor health classes
   - engineer problem-count features

4. **Exploratory Data Analysis**
   - health-class distribution
   - diameter distributions and outliers
   - most common species
   - borough-level health patterns
   - Poor-health share by species
   - health by recorded problem burden
   - user-type relationships
   - spatial distribution of health observations

5. **Classification planning**
   - identification of candidate predictors
   - recognition of class imbalance
   - emphasis on evaluation beyond overall accuracy
   - consideration of data leakage, responsible use and model limitations

## Key Findings from EDA

- The living-tree health outcome is strongly imbalanced: approximately **81.1% Good, 14.8% Fair and 4.1% Poor**.
- Because an always-Good classifier would already achieve around 81% accuracy, **accuracy alone would be misleading** for evaluating a future model.
- Missingness is closely tied to tree status because many health and condition fields are not applicable to dead trees and stumps.
- Tree diameter contains extreme values that should be investigated rather than automatically removed.
- Species composition is concentrated; for example, London planetree is one of the most common species in the living-tree cohort.
- Recorded site and tree problems, species and location show potentially useful relationships with perceived health.

## Technologies

- **R**
- **Quarto**
- **readr**
- **dplyr**
- **tidyr**
- **ggplot2**
- **forcats**
- **scales**
- **patchwork**
- **knitr**

## Repository Structure

```text
.
├── README.md
├── main.r
├── STAT5003_group_project_Quarto_template.qmd
├── Classifying the Perceived Health of New York City Street Trees.html
└── eda_outputs/
    ├── figures/
    ├── tables/
    └── session_info.txt
```

## Reproducibility

The analysis expects the NYC street-tree census CSV in the project root using the filename referenced in the scripts:

```text
2015_Street_Tree_Census_-_Tree_Data_20260912.csv
```

The dataset itself is not included in this repository because of its size. It can be obtained from the official NYC Open Data portal.

To run the EDA script from the project root:

```bash
Rscript main.r
```

The script generates report-ready tables and figures under `eda_outputs/`.

To render the Quarto report, install the required R packages and Quarto, then render the `.qmd` file.

## Responsible Use

This project is predictive and observational. The `health` field is a perceived condition recorded during the census, not a clinical or laboratory measurement. Any future classification model should therefore be treated as a tool for identifying trees that may warrant further inspection, rather than as a substitute for professional arboricultural assessment.

## Skills Demonstrated

- Exploratory Data Analysis
- Data Cleaning and Quality Assessment
- Missing-Data Analysis
- Feature Engineering
- Multi-class Classification Planning
- Class-Imbalance Awareness
- Statistical Reasoning
- Data Visualisation
- Reproducible Analysis with R and Quarto
- Responsible Interpretation of Predictive Models

## Author

**Uma Sree Asritha Adari**  
Master of Computer Science — University of Sydney
