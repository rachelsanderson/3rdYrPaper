## README for linked data project 

###  September 23, 2019
### by Rachel Anderson


================
## What I've done

1. Simulated fake datasets. Randomly introduced errors in names (either typographical errors or missing variables) and birthdays (adding years, months, days, according to draws from a normal distribution).

2. Applied ABE and probabilistic record linkage (PRL) matching procedures, with either unique/multi matching, to obtain 4 matched versions of the fake datasets.

	- PRL is implemented using fastLink package in R.

3. Began writing code for AHL (2019) and Lahiri & Larsen (2005) estimation procedures (this is where I am stuck).

4. Applied for 1940 Census data – there are 2 transcriptions, which I will link to each other.

================

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


