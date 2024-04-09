
#load libaries
import pandas as pd
import os


#set working directory
os.chdir("C:\\SANDEEP\\Shmodel_US\\Data\\Housing\\aggregate_housing_value\\raw")

#Initialize a empty dataframe
dff = pd.DataFrame()
#Run a for loop to read files for each year
years = range(2010,2022) #year ranges from 2010 to 2021
for year in years:
    filename = f"ACSDT5Y{year}.B25082-Data.csv" #read file
    df = pd.read_csv(filename, skiprows = 1, usecols=["Geography", "Estimate!!Aggregate value (dollars)"])
    df.columns = ["FIPS", "value"] #change column name
    df["FIPS"] = df["FIPS"].str.replace('1400000US', '') #convert geoid to fips
    df['FIPS'] = pd.to_numeric(df['FIPS'])
    df["year"] = year #add additional column
    #append dataframes
    dff = pd.concat([dff, df], ignore_index= True)
    
#read coastal tracts fips file
fips_file = pd.read_csv("C:\\SANDEEP\\Shmodel_US\\Data\\fips_svi.csv",usecols = ['FIPS'])
#Keep entries of only coastal tracts
dff_new = dff[dff['FIPS'].isin(fips_file['FIPS'])]

# Create a directory to store the output files
output_dir = "C:\\SANDEEP\\Shmodel_US\\Data\\Housing\\aggregate_housing_value"

# Use list comprehension to create filtered DataFrames and write files
for fips in dff_new['FIPS'].unique():
    temp = dff_new[dff_new['FIPS'] == fips]
    filename = os.path.join(output_dir, f"vhousing_{fips}.csv")
    temp.to_csv(filename, index=False)