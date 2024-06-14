### DatasetProcessing.R 

Handles the creation of the dataset we intend to use. The input file is https://www.cdc.gov/brfss/annual_data/2021/files/LLCP2021XPT.zip , consisting of 438,693 items with 303 columns, while the output file is a csv that uses only 16 columns:
- "DIABETE4" 
- "X_RFHYPE6"
- "TOLDHI3"
- "X_CHOLCH3"
- "X_BMI5"
- "SMOKE100"
- "X_MICHD"
- "CVDSTRK3"
- "X_TOTINDA"
- "X_FRTLT1A"
- "X_VEGLT1A"
- "X_RFDRHV7"
- "X_HLTHPLN"
- "MEDCOST1"
- "GENHLTH"
- "MENTHLTH"
- "PHYSHLTH"
- "DIFFWALK"
- "SEXVAR"
- "X_AGEG5YR"
- "EDUCA"
- "INCOME3"

These were chosen while following the notebook https://www.kaggle.com/code/alexteboul/diabetes-health-indicators-dataset-notebook and confermed using the Codebook found at https://www.cdc.gov/brfss/annual_data/2021/pdf/codebook21_llcp-v2-508.pdf