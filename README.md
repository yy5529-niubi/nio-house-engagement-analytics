This project enables automated updating of user engagement analytics for NIO House using standardized input data.


# NIO House Analytics Project

## Overview
This project provides two main components:

1. **Full Analysis Script (`NIO.R`)**
   - Performs complete analytical workflow
   - Includes KPI construction, segmentation, DID, and robustness checks
   - Used for final report and academic evaluation

2. **Refresh Pipeline (`run_nio_refresh_pipeline.R`)**
   - Used for updating results when new data becomes available
   - Generates updated KPIs, segment profiles, and activity performance metrics
   - Designed for sponsor or future users

---

## Project Structure

- `data/` → Input CSV files  
- `outputs/` → Generated results  
- `NIO.R` → Full analysis (advanced)  
- `run_nio_refresh_pipeline.R` → Refresh pipeline (for future use)  

---

## How to Update Data (For Sponsor / Users)

### Step 1: Prepare updated data files
Ensure the following files are available and correctly formatted:

- nio_house_users.csv
- nio_house_visits.csv
- nio_house_event_participation.csv
- nio_house_transactions.csv
- nio_house_events_calendar.csv

---

### Step 2: Replace files
Put the updated CSV files into the `data/` folder.

---

### Step 3: Run the refresh pipeline

1. Open the project in RStudio (`NIO_project.Rproj`)
2. Open `run_nio_refresh_pipeline.R`
3. Click **Source**

---

### Step 4: Check results

Updated results will be generated in the `outputs/` folder:

- summary_table_refresh.csv
- segment_profile_active_refresh.csv
- activity_scorecard_refresh.csv
- exec_summary.csv
- activity_roi.csv
- segment_uplift.csv

---

## Notes

- The refresh pipeline provides **fast operational updates**
- The full analysis script provides **rigorous analytical results**
- Sponsor users only need to run the refresh pipeline

---

## Contact

For questions about methodology or data structure, please contact the project developer.