import pandas as pd

a = pd.read_csv("filea.csv")
b = pd.read_csv("fileb.csv")
b = b.dropna(axis=1)
merged = a.merge(b, on='title')
merged.to_csv("pbp_2019csv", index=False)


import os
import glob
import pandas as pd
os.chdir("/mydir")


## List filenames to be merged. 
filenames <- list.files(path="/Users/ColinWelsh/Documents/dev/football/data/games_data/2019",pattern="*.csv")

## Print filenames to be merged
print(filenames)
## [1] "abc.csv" "pqr.csv"

from os import chdir
from glob import glob
import pandas as pd

def produceOneCSV(list_of_files, file_out):
   # Consolidate all CSV files into one object
   result_obj = pd.concat([pd.read_csv(file) for file in list_of_files])
   # Convert the above object into a csv file and export
   result_obj.to_csv(file_out, index=False, encoding="utf-8")

# Move to the path that holds our CSV files
csv_file_path = '/Users/ColinWelsh/Documents/dev/football/data/games_data/2019'
chdir(csv_file_path)


file_pattern = ".csv"
list_of_files = [file for file in glob("*".format(file_pattern))]
print(list_of_files)

file_out = "ConsolidateOutput.csv"
produceOneCSV(list_of_files, file_out)