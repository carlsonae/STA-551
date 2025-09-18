################################################################################
# Programmer.....: Ainsley Carlson
# Date Created...: September 17, 2025
# Data Modified..: September 18, 2025
# Program Purpose: Retain specific information from multiple data sets and then merge them
#                  into one data set where each U.S. county only has one observation
# Data Used......: Presidential Election Data: [countypresidential_election_2000-2020.csv],
#                  Unemployment Data: [Unemployment.csv], Poverty Data: [PovertyEstimates.csv],
#                  Education Data: [Education.csv]
# Packages Used..: Readr
# Output.........: County Data: [County_info2020.csv]
################################################################################


################################################################################
############################### Load R Packages ################################
install.packages("tidyverse")
install.packages("readr")
install.packages("RCur")
install.packages("dplyr")


library(tidyverse)
library(readr)
library(RCurl)
library(dplyr)


################################################################################
#fips codes 1000-56000 by 1000 are state level codes and 0 is U.S
#some, like 3000 are skipped which is why it goes up to 56000
states <- seq(0,56000,1000)


################################################################################
########################## Presidential Election Data ##########################
#import prsidntial election data by county
countypresidential_election <- read_csv("https://raw.githubusercontent.com/carlsonae/STA-551/refs/heads/main/assignment-1/data/countypresidential_election_2000-2020.csv")

#create data frame of only 2020 presidential election results by county
presidential_election2020 <- countypresidential_election %>%
  rename(Fips_Code = 5) %>% #give fips codes all the same name
  filter(year==2020 & party %in% c('DEMOCRAT', 'REPUBLICAN')  & !(Fips_Code %in% states) ) %>% #filter parties
  group_by(Fips_Code, party) %>% #group by county then political party
  mutate(partyvotes=sum(candidatevotes)) %>% #sum party votes because some counties do not have a total vote
  select(state, state_po, county_name, Fips_Code, party, partyvotes) %>% #select which variables to keep
  distinct(Fips_Code, .keep_all = TRUE) #only keep distinct county_fips


# in a new section of code group by county fips and then select the winning party
presidential_election2020 <- presidential_election2020 %>%
  group_by(Fips_Code) %>%
  slice_max(partyvotes)

################################################################################



################################################################################
############################## Unemployment Data ###############################
#import unemployment data
unemployment <- read_csv("https://raw.githubusercontent.com/carlsonae/STA-551/refs/heads/main/assignment-1/data/Unemployment.csv")

#create data frame of only 2020 unemployment rates by county
unemployment_2020 <- unemployment %>%
  rename(Fips_Code = 1, unemployment_rate=5) %>% #fip codes have same name
  filter(Attribute=="Unemployment_rate_2020" & !(Fips_Code %in% states) ) %>% #only want 2020 unemployment rate and county level fips
  select(Fips_Code, unemployment_rate)

################################################################################




################################################################################
################################# Poverty Data #################################
#import poverty data
poverty <- read_csv("https://raw.githubusercontent.com/carlsonae/STA-551/refs/heads/main/assignment-1/data/PovertyEstimates.csv")

#create data frame of only 2019 poverty rates by county
poverty_2019 <- poverty %>% 
  rename(Fips_Code = 1, poverty_rate=5) %>% #give fips codes the same column name
  filter(Attribute=="PCTPOVALL_2019" & !(Fips_Code %in% states)) %>% # want 2019 poverty level
  select(Fips_Code, poverty_rate)
################################################################################




################################################################################
################################ Education Data ################################
#import education data
education <- read_csv("https://raw.githubusercontent.com/carlsonae/STA-551/refs/heads/main/assignment-1/data/Education.csv")


#72000 is Puerto Rico

#create data frame of only 2015-2019 rates of education level by county
education_2019 <- education %>% 
  rename(Fips_Code = 1, less_highschool = 44, highschool = 45,
        some_college=46, bachelor_more=47) %>%  #rename columns without spaces
  mutate(Fips_Code=as.numeric(Fips_Code)) %>% #convert fips column to numric
  filter(!(Fips_Code %in% states) & !(Fips_Code == 72000)) %>% #72000 is Puerto Rico
  select(Fips_Code,less_highschool, highschool, some_college, bachelor_more) #columns of interest
        


################################################################################


#crerate list of data frames
df_list <- list(presidential_election2020, unemployment_2020, poverty_2019, education_2019)

#merge data frames by fips code
county_info2020 <- df_list %>% 
  reduce(full_join, by=c('Fips_Code'))

#export data to csv to upload to github
write_csv(county_info2020, "C:\\Users\\OwNeR\\OneDrive - West Chester University of PA\\Desktop\\acarlson\\county_info2020.csv")


