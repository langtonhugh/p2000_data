# Import libraries.
import pandas as pd
import matplotlib.pyplot as plt

# Settings.
pd.set_option('display.max_colwidth', None)

# Load in data. For now this is the sample generated
# in p2000_brandweer_create_data.r.
scrapes = pd.read_csv("data/all_sample_scrapes.csv")

# Check load correctly.
#scrapes.head()

# Rename columns.
scrapes.rename(columns={'X1': 'code' ,
                        'X2': 'times',
                        'X3': 'dates',
                        'X4': 'sign',
                        'X5': 'info'},
               inplace=True)

# Check it worked.
scrapes.columns

# Count missings.
scrapes.isna().sum()

# Missings are an artefact of the scrape, separating incidents.
scrapes_nm = scrapes.dropna(inplace = False)

# Due to scrape frequency, there are loads of duplicates. Remove them.
scrapes_nm_dd = scrapes_nm.drop_duplicates(subset = ['times', 'dates', 'info'],
                                           keep = False)

# Check it worked as expected.
raw_dim = scrapes_nm.shape
dd_dim  = scrapes_nm_dd.shape
print(raw_dim, dd_dim)

# Load in CAP data.
cap = pd.read_csv("data/capcodelijst_source_in_header.csv", skiprows=1, delimiter=';')

# Check contents.
cap.head()

# Join by code. 
scrapes_wcodes = scrapes_nm_dd.merge(cap, on = "code", how = "left")

# Check structure now.
scrapes_wcodes.head()

# Flag occurences to ambulance or AMBU (we know these are commonly used).
scrapes_wcodes['info'].str.contains('Ambulance|AMBU').value_counts()

# Add flag to the data frame.
scrapes_wcodes = scrapes_wcodes.assign(ambu_flag = scrapes_wcodes['info'].str.contains('Ambulance|AMBU'))

# Check it worked.
scrapes_wcodes['ambu_flag'].value_counts()

# Compare the existing CAP flag with the character string filter.
pd.crosstab(index = scrapes_wcodes.cap_service, columns = scrapes_wcodes.ambu_flag)

disgaree = scrapes_wcodes[(scrapes_wcodes['cap_service'] == "Ambulance") &
                          (scrapes_wcodes['ambu_flag'] == False) ]

# Inspect manually.
disgaree.head()

scrapes_wcodes = scrapes_wcodes.assign(hyb_ambu_flag = (scrapes_wcodes['cap_service'] == "Ambulance")  | (scrapes_wcodes['ambu_flag'] == True))

# Check that it worked as expected.
pd.crosstab(index = scrapes_wcodes.hyb_ambu_flag, columns = scrapes_wcodes.cap_service)

# Now we make the filter for ambulance-only.
ambu_only = scrapes_wcodes[scrapes_wcodes.hyb_ambu_flag == True]

ambu_only.head()

# Pull out the priority codes using first three characters, then remove any whitespace.
ambu_only = ambu_only.assign(prio = ambu_only['info'].str[:3].str.replace(" ", ""))

# Check resulting categories. 
ambu_only['prio'].value_counts()

ambu_disp = ambu_only[(ambu_only.prio == "A1")  | (ambu_only.prio == "A2")]

# Frequency counts of prios.
prio_counts = ambu_disp['prio'].value_counts()
print(prio_counts)

# Bar plot of prios nationwide.
prio_counts.plot.bar()

# Frequency counts of incidents, nationwide.
region_counts = ambu_disp['region'].value_counts()
print(region_counts)

# Bar plot of regional counts.
region_counts.plot.bar()
plt.show()

# Check column names.
ambu_disp.columns

# Create time variable.
ambu_disp.loc[:, 'times'] = pd.to_datetime(ambu_disp['times'], format='%H:%M:%S').dt.time

print(type(ambu_disp['times'][0]))

ambu_disp.columns

print(ambu_disp['times'])

# Extract hours.
hours_result = []

for h in ambu_disp['times']:
    hours_result.append(h.hour)

# Check type. 
type(hours_result)

# Assign back to data frame. 
ambu_disp.loc[:, 'day_hours'] = pd.to_numeric(hours_result, downcast = 'signed')

print(ambu_disp['day_hours'])

# Freq count.
hourly_counts = ambu_disp['day_hours'].value_counts().reset_index()
print(hourly_counts)

# Plot.
hourly_counts.plot.scatter(x='day_hours', y='count')
plt.show()

ambu_disp.head()