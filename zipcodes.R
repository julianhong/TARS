#Copyright (C) Duke University/Julian Hong 2017
#GNU General Public License v2.0
#Please see LICENSE and README.md 

#zipcodes.r
#imports the patient zip code info

#import the info
pataddressinfo <- pataddressinfo.csv
#quick reformat of the zipcodes into just the first five
pataddressinfo$Patient.Primary.Postal.Code <- substr(pataddressinfo$Patient.Primary.Postal.Code, 1,5)

pataddressinfo <- rename(pataddressinfo, zip = Patient.Primary.Postal.Code)
