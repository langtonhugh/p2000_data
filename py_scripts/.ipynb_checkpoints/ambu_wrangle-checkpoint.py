# Import libraries.
import pandas as pd

# Load in data. For now this is the sample generated
# in p2000_brandweer_create_data.r.
scrapes = pd.read_csv("../data/all_sample_scrapes.csv")

# Check load correctly.
print(scrapes.head, 6)
print(scrapes.columns)

# Rename columns.
#scrapes.rename(columns = {" } )
