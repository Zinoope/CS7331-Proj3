### PART I: cleaning and Visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step I-01: Loading the Dataset ----------------------------------------------------
#load the csv file/ check how many missing values do we have
library("tidyverse")

cases <- read_csv("../data/Covid 04-02 + Census 2020-5yrs + Geo Boundaries.csv")
cases <- cases %>% mutate_if(is.character, factor)
dim(cases)
# check for NA values
is.na(cases) %>% sum()

## Step I-02: Aggregation, Normalization and Selection -----------------------------
cases_filtered <- cases %>% mutate(
  female_under_40_ratio= (female_under_5 +
                            female_5_to_9 +
                            female_10_to_14 +
                            female_15_to_17 +
                            female_18_to_19 +
                            female_20 +
                            female_21 +
                            female_22_to_24 +
                            female_25_to_29 +
                            female_30_to_34 +
                            female_35_to_39) / total_pop,
  
  female_pop_P100=female_pop/total_pop*100,
  
  male_under_40_ratio= (male_under_5 +
                          male_5_to_9 +
                          male_10_to_14 +
                          male_15_to_17 +
                          male_18_to_19 +
                          male_20 +
                          male_21 +
                          male_22_to_24 +
                          male_25_to_29 +
                          male_30_to_34 +
                          male_35_to_39) / total_pop,
  
  male_pop_P100=male_pop/total_pop*100,
  
  asian_pop_P1000=asian_pop/total_pop*1000,
  black_pop_P1000=black_pop/total_pop*1000,
  hispanic_pop_P1000=hispanic_pop/total_pop*1000,
  
  deaths_P1000 = deaths/total_pop*1000,
  confirmed_cases_P1000= confirmed_cases/total_pop*1000,
  
  walked_to_work_P1000 = walked_to_work/total_pop*1000,
  commuters_by_public_transportation_P1000 = commuters_by_public_transportation/total_pop*1000,
  commuters_by_carpool_P1000 = commuters_by_carpool/total_pop*1000,
  commuters_drove_alone_P1000 = commuters_drove_alone/total_pop*1000,
  
  pop_density_Pkm= total_pop * 10^6 /area_land_meters
  
)

cases_cleaned <- cases_filtered %>% select(county_name,
                                           # Ground truth
                                           confirmed_cases_P1000,
                                           deaths_P1000,
                                           # first Subset
                                           # gender/Ethnicity and age
                                           female_pop_P100,
                                           male_pop_P100,
                                           female_under_40_ratio,
                                           male_under_40_ratio,
                                           # habits and interactions
                                           walked_to_work_P1000,
                                           commuters_by_public_transportation_P1000,
                                           commuters_by_carpool_P1000,
                                           commuters_drove_alone_P1000,
                                           # financial related
                                           income_per_capita,
                                           
                                           # Second Subset
                                           # gender/Ethnicity and age
                                           asian_pop_P1000,
                                           black_pop_P1000,
                                           hispanic_pop_P1000,
                                           median_age,
                                           # habits and interactions
                                           pop_density_Pkm,
                                           # financial related
                                           median_income,
                                           
                                           
) 

# check for NA values
is.na(cases_cleaned) %>% sum()
cases_cleaned
rm(cases, cases_filtered)
