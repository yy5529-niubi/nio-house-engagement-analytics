# NIO House Engagement Analytics

## Overview
This project develops a data-driven analytics framework to quantify customer engagement and evaluate the business impact of community activities in NIO House.

Traditional engagement strategies in community-based service environments are often evaluated qualitatively. This project addresses that gap by transforming user behavior data into measurable insights that support operational decision-making.

---

## Objectives
- Segment users based on behavioral KPIs  
- Measure the impact of community activities on engagement  
- Quantify changes in visit frequency, retention, and spending  
- Provide actionable insights for engagement optimization  

---

## Methodology
The analysis follows a structured pipeline:

1. Data cleaning and preprocessing of user, visit, transaction, and event datasets  
2. KPI construction (visit frequency, retention, spend, participation)  
3. Rule-based user segmentation  
4. Activity impact evaluation using quasi-experimental methods  
5. Difference-in-differences and robustness checks (placebo, pre-trend, sensitivity analysis)  

---

## Key Results
- Positive engagement impact from activity participation  
- Measurable uplift in visit frequency, spending, and retention  
- Significant variation across activity types  
- Identification of high-performing activities (e.g., workshops)  
- Clear segmentation of high-value and at-risk users  

---

## Tools and Technologies
- R  
- tidyverse  
- lubridate  

---

## Reproducibility
The analysis pipeline is designed to be reproducible.  
To rerun the analysis:

1. Update input datasets  
2. Run `run_nio_refresh_pipeline.R`  
3. Outputs will be generated in CSV format  

---

## Notes
Due to data confidentiality constraints, raw datasets are not publicly included.  
All analysis results are based on anonymized user data provided for academic purposes.

---

## Author
Yizhou Yang  
NYU School of Professional Studies
