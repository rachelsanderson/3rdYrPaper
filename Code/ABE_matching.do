/* This code gives an example of how the various versions of the ABE/Ferrie algorithm are run in Stata. 
In this example we give the code to link the 1865 Norwegian census to the 1900 Norwegian census. However the
majority of this code is generic, and will apply to any data set. In order use this code for your specific data, 
standardize the variable names given to match the names used in Section I. 

A note on linking large data sets: When linking large files (e.g. full-count census data), in order 
to make the matching more computationally feasible begin by saving your observations by place of birth 
(e.g. before matching all American-born men in the 1900 census to the 1910 census you should have 100 
individual data files, 50 place of birth files from each census year). The following linking steps will 
then be completed for each place of birth separately (e.g. link Alabama in 1900 to Alabama in 1910, then 
Arizona in 1900 to Arizona in 1910, ect…)  */
*-------------------------------------------------------------------------------------------------------*/
set more off
clear all

/* Specify directory locations: */
global MatchingDoFiles   "/Users/rachelanderson/Desktop/3rdYrPaper/Code/ABE_algorithm_code/codes"  	    	/*location where all ABE/Ferrie matching algorithm files (and .ado files) are stored */
global datadirA			 "/Users/rachelanderson/Desktop/3rdYrPaper/Code/Data/FakeData"		  		/*location where the raw data for dataset A is held*/
global datadirB			 "/Users/rachelanderson/Desktop/3rdYrPaper/Code/Data/FakeData"  			/*location where the raw data for dataset B is held*/
global outdir 			 "/Users/rachelanderson/Desktop/3rdYrPaper/Code/Data/MatchedData" 						/*location to store cleaned data*/
global matchdir 		 "/Users/rachelanderson/Desktop/3rdYrPaper/Code/Data/MatchedData"			 				/*location to store matched data*/

*******************************************************************************
/* SECTION 1. Standardize and Clean Data
*******************************************************************************/

/* 1.1 - Standardize data. This section of the code standardizes variable names */

use $datadirA//x_data.dta, clear

// keep if age >= 10 & age <= 20 & sex == 1 	// apply any restrictions 

rename first f_name 						// call first name "f_name"
rename last l_name 						// call last name "l_name"

// rename bplno Place_of_Birth 				// optional, if matching on place of birth

// tostring serial, replace
// tostring pernum, replace
// gen id = serial + "_" + pernum				// generate unique identifier, or rename existing identifier "id"

save $datadirA//x_data_standardized.dta, replace

use $datadirB//y_data.dta, clear

// keep if age >= 45 & age <= 55 & sex == 1  	// apply any restrictions

rename first f_name 						// call first name "f_name"
rename last l_name 						// call last name "l_name"
// rename bplno Place_of_Birth 				// optional, if matching on place of birth

// tostring serial, replace
// tostring pern/um, replace
// gen id = serial + "_" + pernum				// generate unique identifier, or rename existing identifier "id"

save $datadirB//y_data_standardized.dta, replace

/* 1.2 - Clean names using abeclean.ado
		 Options: nicknames standardize common nicknames, the variable "sex" is required to specify male or female (sex = 1 if male, = 2 if female).
				  intial(middleinitial) creates a middle initial from the "f_name" string. */

cd $MatchingDoFiles // set current directory to location of abeclean.ado and abematch.ado
				  
use $datadirA//x_data_standardized.dta, clear
abeclean f_name l_name
save $datadirA//x_data_ready2link.dta, replace

use $datadirB//y_data_standardized.dta, clear
abeclean f_name l_name
save $datadirB//y_data_ready2link.dta, replace
clear

*******************************************************************************
/* SECTION 2. ABE/Ferrie matching algorithm
*******************************************************************************/

/* 2.1 Set up abe matching algorithm.  */
	global A $datadirA//x_data_ready2link.dta 				// cleaned & standardized data file A
	global B $datadirB//y_data_ready2link.dta 				// cleaned & standardized data file B
	global outputfile "${outdir}/ABE_matches_single"  // file name to save matched data 

	/* "match_vars" specifies which variables to link on. There are many options. Here we use NYSIIS standardized 
	names (f_name_nysiis l_name_nysiis), but you could instead use exact names (f_name_cleaned l_name_cleaned). In this 
	example we match on place of birth, other options include middle initial, race, or any other time-invariant characteristic. 
	*/
	global match_vars f_name_nysiis l_name_nysiis 

	global timediff = 0 // number of years between the age reported in data set A and age reported in data set B. 

/* 2.2 Find ABE matches, standard version */
	clear
	abematch $match_vars,  file_A($A) file_B($B) timevar(year) timediff($timediff ) save($outputfile )  replace id_A(id) id_B(id) keep_A(l_name f_name) keep_B(l_name f_name) 
	
		* see abematch.sthlp for additional options 


/* 2.3 More conservative matching, restrict sample to those unique within +-2 years of age. */
// 	keep if unique_file2 == 1  // keep only names unique within +- 2 years in own data sets
// 	keep if unique_match2 == 1 // drop people that have another potential match within +-2 years of birth

// 	save ${outputfile}_5yr_band, replace

	
** last updated: Jan 14, 2018 
** please contact ranabr@stanford.edu (Ran Abramitzky), lboustan@princeton.edu (Leah Boustan), and/or kaeriksson@ucdavis.edu (Katherine Eriksson) 
** with any questions or feedback about this code. 


