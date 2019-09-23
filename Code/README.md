# README for "A unified approach to analyzing linked data" 

September 23, 2019

by Rachel Anderson


## Workflow Overview 
### 1.  Simulate fake data 
In "Simulate_Data" folder, the files

- make\_gold\_data.R 
- make\_bad\_data.R

save datasets in ./Data/FakeData/ folder

### 2.  Link data

In "Link_Files" folder, 

- Run all\_linking\_procedures.R

This file calls the link\_files.R helper function, which implements, for two input datasets,

- abe\_matching.R
- prl\_match.R

and outputs linked dataset as CSV files in ./Data/MatchedData

### 3. Estimate using linked data 

In "Estimation" folder, 

- Run all\_estimation.R

using linked datasets.  This file calls the functions

- ahl19.R
- lahiri_larsen.R 

## Simulating fake data
