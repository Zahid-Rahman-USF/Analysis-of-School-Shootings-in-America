#School Shooting Preprocessing
getwd()
setwd("C:/Users/ny123/Downloads") 
library(readxl)
library(dplyr)

rm(list=ls())

#Read each sheet of the excel file individually
incident = read_excel("School Shootings.xlsx", sheet = "Incident")
#incident2 = read_excel("School Shootings.xlsx", sheet = "Incident")
#unique(incident2$Shots_Fired)
shooter = read_excel("School Shootings.xlsx", sheet = "Shooter")
victim = read_excel("School Shootings.xlsx", sheet = "Victim")
weapon = read_excel("School Shootings.xlsx", sheet = "Weapon")
##Drop Incident ID at the end of preprocessing, not immediately
##Remember to add the school name back to the dropped columns

str(incident)
str(shooter)
str(victim)
str(weapon)


### Preprocessing Incident ###
#Only keeping rows where Number_Victims > 0 & Situation != "Accidental"
incident = incident %>% filter(Number_Victims > 0, Situation != "Accidental")
View(incident)
#str(incident)
#Check each column for missing values and print
sapply(incident, function(x) sum(is.na(x)))
colSums(is.na(incident))

#Checking how many incident happened as a suicide


#Drop columns that are not needed
    #Month	Day	Year	Date	School	Victims_Killed	Victims_Wounded	Source	Number_News	Media_Attention	Reliability	Quarter	City	First_Shot	Summary	Narrative	Accomplice	Accomplice_Narrative	Barricade	Active_Shooter_FBI	LAT	LNG Involved_Students_Staff
incident = incident %>% select(-Month, -Day, -Year, -Date, -Victims_Killed, -Victims_Wounded, -Source, -Number_News, -Media_Attention, -Reliability, -Quarter, -City, -First_Shot, -Summary, -Narrative, -Barricade, -Accomplice, -Accomplice_Narrative, -Active_Shooter_FBI, -LAT, -LNG)

#Checking for wherever Shooter_Killed = 2 and turning it into 1
incident$Shooter_Killed = ifelse(incident$Shooter_Killed == 2, 1, incident$Shooter_Killed)
#State Column (Includes all 50 states, plus DC & Virgin Islands but FL is repeated)
unique(incident$State)
table(incident$State)
incident = inciStateincident = incident %>% filter(!is.na(State))#Drop rows where State is NA (Only 1 row)
#1 row with state ="Florida" instead of "FL", so change it to "FL"
incident$State = ifelse(incident$State == "Florida", "FL", incident$State)
#Grouping states into three groups: High Gun Control, Medium Gun Control, Low Gun Control
    # High Gun Control: CA, CT, NJ, CO, HI, IL, MD, MA, NY, OR, WA, DC
    # Moderate Gun Control: DE, RI, VA, MN, PA, MI, NV, VT
    # Low Gun Control: NM, WI, NC, NE, FL, IN, ME, NH, OH, SC, AL, AK, AZ, AR, GA, ID, IA, KS, KY, LA, MS, MO, MT, ND, OK, SD, TN, TX, UT, WV, WY, VI
incident$State_Gun_Control = ifelse(incident$State %in% c("CA", "CT", "NJ", "CO", "HI", "IL", "MD", "MA", "NY", "OR", "WA", "DC"), "High", ifelse(incident$State %in% c("DE", "RI", "VA", "MN", "PA", "MI", "NV", "VT"), "Medium", "Low"))
incident = incident %>% select(-State)
#Drop Location column
incident = incident %>% select(-Location)

# Creating new feature that bins duration_min into 2 categories: "1 Minute or Less" and "Over 1 Minute"
#Check to see during modelling if this feature is giving trouble
#You have the option of keeping one or the other or keeping or dropping both
#The overwhleming majority of the data is under 1 minute, hence the decision to create this feature
incident$Duration_Min_Category <- ifelse(incident$Duration_min <= 1, "Under 1 min", "Over 1 min")

#'Shan's comments
#'missing duration_min rows are only 108, we can drop the rows. However making them 1 wouldn't casue a lot of damage, as the bias is towards 1 already. 

#For the missing values in Duration_min, impute them as 1
incident$Duration_min[is.na(incident$Duration_min)] <- 1
#For the missing values in Duration_Min_Category, impute them as "Under 1 min"
incident$Duration_Min_Category[is.na(incident$Duration_Min_Category)] <- "Under 1 min"
#barplot(table(incident$Duration_Min_Category), main="Duration of Incident", xlab="Duration", ylab="Frequency", col="blue", legend = rownames(table(incident$Duration_Min_Category)), beside=TRUE)

#Shots_Fired
#Visualizing the distribution of Shots_Fired
#hist(incident$Shots_Fired, main="Histogram of Shots Fired", xlab="Shots Fired", col="blue", breaks=2)
#str(incident$Shots_Fired)
incident$Shots_Fired <- as.character(incident$Shots_Fired)
unique(incident$Shots_Fired)
table(incident$Shots_Fired)
discrepancy.shotsfired = incident[which(incident$Shots_Fired == '0'), ]
View(discrepancy.shotsfired)
#'in discrepancy shotsfired data, we can observe that there were no shots fired, but the victims were killed.
#'The cause of death of these individuals could be something other than shooting. Or the data could have been falsely captured. 
#'Dropping the 7 rows from the dataset
incident = incident %>% filter(Shots_Fired > 0)
incident[which(incident$Shots_Fired == '0'), ]

#Wherever "Multiple" is present, we can impute it with 10
#Shan's comments: why are there no wos with value as 'multiple' in the data frame, while the excel has those rows?
incident$Shots_Fired <- ifelse(incident$Shots_Fired == "Multiple", 10, incident$Shots_Fired)
#For all missing values in Shots_Fired, we can impute it with 26
#summary(as.numeric(incident$Shots_Fired)) #Median = 3, mean = 83
sum(is.na(incident$Shots_Fired))
incident$Shots_Fired[is.na(incident$Shots_Fired)] <- 26
#Handle rows under Shots_fired that have inequality symbols using Boundary Imputation
#Example: If a value is ">100", we can impute it with 101 and if "<10", we can impute it with 9
incident$Shots_Fired <- ifelse(grepl("^>", incident$Shots_Fired), as.numeric(sub(">", "", incident$Shots_Fired)) + 1, incident$Shots_Fired)
incident$Shots_Fired <- ifelse(grepl("^<", incident$Shots_Fired), as.numeric(sub("<", "", incident$Shots_Fired)) - 1, incident$Shots_Fired)
incident$Shots_Fired <- as.numeric(incident$Shots_Fired)
summary(incident$Shots_Fired)
#Check for missing values
sum(is.na(incident$Shots_Fired))
# Check for any remaining instances of "Multiple"
any(grepl("Multiple", incident$Shots_Fired))
# Check for any remaining instances of inequality symbols
any(grepl("^[><]", incident$Shots_Fired))
all(sapply(incident$Shots_Fired, is.numeric))
str(incident)

#Drop Involves_Student_Staff
#I realized that this column may not be useful for our analysis as it more a descriptor than a predictor and also, it has a lot of missing values
incident = incident %>% select(-Involves_Students_Staff)
str(incident)

#Check each column for missing values and print
sapply(incident, function(x) sum(is.na(x)))

table(incident$School_Level)
sum(is.na(incident$School_Level))
#Print the rows where School_Level is NA
incident %>% filter(is.na(School_Level))
#Drop rows where School_Level is NA because the school name is not provided to determine school level
incident = incident %>% filter(!is.na(School_Level))
table(incident$School_Level)
#Print the rows where School_Level is 44724, the school name, and the ID
incident %>% filter(School_Level == 44724)  %>% select(School, Incident_ID)
incident %>% filter(School_Level == "Unknown")  %>% select(School, Incident_ID)
#Sulphur Rock Magnet School, it's school level is Elementary
#East English Village Preparatory Academy, it's school level is High
#Episcopal School of Jacksonville, it's school level is K-12
#Dekalb Alternative School, it's school level is High
#Impute the missing values
incident$School_Level = ifelse(incident$School == "Sulphur Rock Magnet School", "Elementary", incident$School_Level)
incident$School_Level = ifelse(incident$School == "East English Village Preparatory Academy", "High", incident$School_Level)
incident$School_Level = ifelse(incident$School == "Episcopal School of Jacksonville", "K-12", incident$School_Level)
incident$School_Level = ifelse(incident$School == "Dekalb Alternative School", "High", incident$School_Level)
incident$School_Level = ifelse(incident$School == "School of Choice", "High", incident$School_Level)
table(incident$School_Level)
#Whichever rows have School_Level as "Junior High", impute it to "Middle"
incident$School_Level = ifelse(incident$School_Level == "Junior High", "Middle", incident$School_Level)
#Which ever rows have School_Level as "6-12", "K-12", "K-8", or "Other", combine them all into a new category called "Mutli-Level" 
incident$School_Level = ifelse(incident$School_Level %in% c("6-12", "K-12", "K-8", "Other"), "Multi-Level", incident$School_Level)
#Only one school has school_level as 44724, impute it to "Multi-Level"
incident$School_Level = ifelse(incident$School_Level == 44724, "Multi-Level", incident$School_Level)

#Barplot of location_type
barplot(table(incident$Location_Type), main="Location Type", xlab="Location Type", ylab="Frequency", col="blue", legend = rownames(table(incident$Location_Type)), beside=TRUE)
#print the IDs of the rows where Location_Type has missing values
incident %>% filter(is.na(Location_Type)) %>% select(Incident_ID)
#The following Incident_IDs have missing values for Location_Type
#20191008TXWEH, 19830130TXWEC, 19810913MDABA
#Impute as follows:
    #20191008TXWEH, it's location_type is "Outside on School Property"
    #19830130TXWEC, it's location_type is "Outside on School Property"
    #19810913MDABA, it's location_type is "Outside on School Property"
incident$Location_Type = ifelse(incident$Incident_ID == "20191008TXWEH", "Outside on School Property", incident$Location_Type)
incident$Location_Type = ifelse(incident$Incident_ID == "19830130TXWEC", "Outside on School Property", incident$Location_Type)
incident$Location_Type = ifelse(incident$Incident_ID == "19810913MDABA", "Outside on School Property", incident$Location_Type)
table(incident$Location_Type)
#Drop whichever rows have Location_Type as "ND" due to lack of information to impute
incident = incident %>% filter(Location_Type != "ND")

#Check each column for missing values and print
sapply(incident, function(x) sum(is.na(x)))

#Barplot of During_Classes
barplot(table(incident$During_Classes), main="During Classes", xlab="During Classes", ylab="Frequency", col="blue", legend = rownames(table(incident$During_Classes)), beside=TRUE)
#print the rows  During_Classes has missing values
incident %>% filter(is.na(During_Classes))
#For the Incident_IDs(20050429OHDAC), impute During_Classes as "Yes"
incident$During_Classes = ifelse(incident$Incident_ID == "20050429OHDAC", "Yes", incident$During_Classes)
#For the Incident_IDs(19960514UTBIT), impute During_Classes as "No"
incident$During_Classes = ifelse(incident$Incident_ID == "19960514UTBIT", "No", incident$During_Classes)
#For the Incident_IDs(20211119COHIA), impute During_Classes as "Yes"
incident$During_Classes = ifelse(incident$Incident_ID == "20211119COHIA", "Yes", incident$During_Classes)
#For the Incident_IDs(19970428CAJOL), impute During_Classes as "Yes"
incident$During_Classes = ifelse(incident$Incident_ID == "19970428CAJOL", "Yes", incident$During_Classes)
#For the incident_IDs(19970403CAMAM), impute During_Classes as "No"
incident$During_Classes = ifelse(incident$Incident_ID == "19970403CAMAM", "No", incident$During_Classes)
#For the incident_IDs(19960411ALTAT), impute During_Classes as "No"
incident$During_Classes = ifelse(incident$Incident_ID == "19960411ALTAT", "No", incident$During_Classes)
#For the incident_IDs(19920128LAFRG), impute During_Classes as "Yes"
incident$During_Classes = ifelse(incident$Incident_ID == "19920128LAFRG", "Yes", incident$During_Classes)
#For the incident_IDs(19811209NYGEB), impute During_Classes as "Yes"
incident$During_Classes = ifelse(incident$Incident_ID == "19811209NYGEB", "Yes", incident$During_Classes)
#For the incident_IDs(19731109CALOL), impute During_Classes as "No"
incident$During_Classes = ifelse(incident$Incident_ID == "19731109CALOL", "No", incident$During_Classes)

#print the rows for Officer_Involved that has missing values
incident %>% filter(is.na(Officer_Involved))
#For the Incident_IDs(20220609ALWAG), impute Officer_Involved as "Yes"
incident$Officer_Involved = ifelse(incident$Incident_ID == "20220609ALWAG", "Yes", incident$Officer_Involved)
#For the Incident_IDs(20220421NDMOM), impute Officer_Involved as "Yes"
incident$Officer_Involved = ifelse(incident$Incident_ID == "20220421NDMOM", "Yes", incident$Officer_Involved)

#Check each column for missing values and print
sapply(incident, function(x) sum(is.na(x)))

#print the rows for Hostages that has missing values
incident %>% filter(is.na(Hostages))
#Barplot of Hostages
barplot(table(incident$Hostages), main="Hostages", xlab="Hostages", ylab="Frequency", col="blue", legend = rownames(table(incident$Hostages)), beside=TRUE)
#For the Incident_IDs(20220609ALWAG), impute Hostages as "No"
incident$Hostages = ifelse(incident$Incident_ID == "20220609ALWAG", "No", incident$Hostages)
#For the Incident_IDs(20220513FLALW), impute Hostages as "No"
incident$Hostages = ifelse(incident$Incident_ID == "20220513FLALW", "No", incident$Hostages)
#For the Incident_IDs(20220427TXMOS), impute Hostages as "No"
incident$Hostages = ifelse(incident$Incident_ID == "20220427TXMOS", "No", incident$Hostages)
#For the Incident_IDs(20220421NDMOM), impute Hostages as "No"
incident$Hostages = ifelse(incident$Incident_ID == "20220421NDMOM", "No", incident$Hostages)
#For the Incident_IDs(20220329NVWEL), impute Hostages as "No"
incident$Hostages = ifelse(incident$Incident_ID == "20220329NVWEL", "No", incident$Hostages)
#For the Incident_IDs(20211207ILHAC), impute Hostages as "No"
incident$Hostages = ifelse(incident$Incident_ID == "20211207ILHAC", "No", incident$Hostages)
#For the Incident_IDs(20210812GALIL), impute Hostages as "No"
incident$Hostages = ifelse(incident$Incident_ID == "20210812GALIL", "No", incident$Hostages)
#For the Incident_IDs(20210511NYPSB), impute Hostages as "No"
incident$Hostages = ifelse(incident$Incident_ID == "20210511NYPSB", "No", incident$Hostages)
#For the Incident_IDs(20210502ILCHC), impute Hostages as "No" 
incident$Hostages = ifelse(incident$Incident_ID == "20210502ILCHC", "No", incident$Hostages)

table(incident$Time_Period)
summary(incident$Time_Period)
#Decide before dropping Time_Period column. Do you want to keep it and drop DuringClasses or vice versa?
#incident = incident %>% select(-Time_Period)
#Print the rows where Time_Period is missing
incident %>% filter(is.na(Time_Period)) %>% select(Incident_ID)
#For the missing values in Time_Period, impute them as "Unknown"
incident$Time_Period = ifelse(is.na(incident$Time_Period), "Unknown", incident$Time_Period)
#Consolidate "Before School" & "School Start" into ine new category called "Before/Start of School"
incident$Time_Period = ifelse(incident$Time_Period %in% c("Before School", "School Start"), "Before/Start of School", incident$Time_Period)
#Merge "Not A School Day" with "Not a School Day"
incident$Time_Period = ifelse(incident$Time_Period == "Not A School Day", "Not a School Day", incident$Time_Period)
#Merge "School Event" and "Sport Event" into one category called "School/Sport Event"
incident$Time_Period = ifelse(incident$Time_Period %in% c("School Event", "Sport Event"), "School/Sport Event", incident$Time_Period)
#Combine "Night" and "Evening" into one category called "Night/Evening"
incident$Time_Period = ifelse(incident$Time_Period %in% c("Night", "Evening"), "Night/Evening", incident$Time_Period)

#Check each column for missing values and print
sapply(incident, function(x) sum(is.na(x)))

#Barplot of Gang_Related
barplot(table(incident$Gang_Related), main="Gang Related", xlab="Gang Related", ylab="Frequency", col="blue", legend = rownames(table(incident$Gang_Related)), beside=TRUE)
#Print the rows where Gang_Related is missing
incident %>% filter(is.na(Gang_Related))
str(incident)

# For the missing values in "Gang_Related", apply the following rules:
# Assign "Gang_Related" = "Yes" if:
#   - Situation = "Drive-by Shooting".
#   - Situation = "Escalation of Dispute" AND Location_Type = "Outside on School Property".
#   - Situation = "Illegal Activity" AND Location_Type = "Outside on School Property".

# Assign "Gang_Related" = "No" if:
#   - Situation = "Escalation of Dispute" AND Location_Type = "Inside School Building" or "School Bus".
#   - Situation = "Illegal Activity" AND Location_Type = "Inside School Building".
#   - Situation = "Indiscriminate Shooting", "Racial", or "Unknown".

# For any remaining missing values:
#   - Impute based on the existing distribution:
#     - Randomly assign "Yes" or "No" to match the proportion of the non-missing values
# Assign "Gang_Related" based on specific conditions
incident$Gang_Related <- ifelse(incident$Situation == "Drive-by Shooting", "Yes", incident$Gang_Related)
incident$Gang_Related <- ifelse(incident$Situation == "Escalation of Dispute" & incident$Location_Type == "Outside on School Property", "Yes", incident$Gang_Related)
incident$Gang_Related <- ifelse(incident$Situation == "Illegal Activity" & incident$Location_Type == "Outside on School Property", "Yes", incident$Gang_Related)
incident$Gang_Related <- ifelse(incident$Situation == "Escalation of Dispute" & incident$Location_Type %in% c("Inside School Building", "School Bus"), "No", incident$Gang_Related)
incident$Gang_Related <- ifelse(incident$Situation == "Illegal Activity" & incident$Location_Type == "Inside School Building", "No", incident$Gang_Related)
incident$Gang_Related <- ifelse(incident$Situation %in% c("Indiscriminate Shooting", "Racial", "Unknown"), "No", incident$Gang_Related)
# Calculate proportions of Yes and No in Gang_Related for non-missing values
proportions <- table(incident$Gang_Related[!is.na(incident$Gang_Related)]) / sum(!is.na(incident$Gang_Related))
# Impute remaining missing values in "Gang_Related" based on existing distribution
incident$Gang_Related[is.na(incident$Gang_Related)] <- sample(c("Yes", "No"), 
                                                              size = sum(is.na(incident$Gang_Related)), 
                                                              replace = TRUE, 
                                                              prob = c(proportions["Yes"], proportions["No"]))
# Check for any remaining missing values
sum(is.na(incident$Gang_Related))
# Check for any remaining instances of "Unknown"
any(grepl("Unknown", incident$Gang_Related))

#Check each column for missing values and print
sapply(incident, function(x) sum(is.na(x)))
table(incident$Bullied)
sum(is.na(incident$Bullied))
#Barplot of Bullied
barplot(table(incident$Bullied), main="Distribution of Bullied", xlab="Bullied", ylab="Frequency", col="blue", legend = rownames(table(incident$Gang_Related)), beside=TRUE)
#Bullied is difficult to assess from the other columns, dropping the rows where bullied is blank
incident =incident %>% filter(!is.na(Bullied))

#Handling Domestic Violence
sum(is.na(incident$Domestic_Violence))
incident %>% filter(is.na(Domestic_Violence)) # 8 rows had null values for domestic violence
incident %>% filter(Domestic_Violence == 'N/A')
unique(incident$Domestic_Violence)
table(incident$Domestic_Violence)
#we can drop N/A and null values from the data. We cannot classify these characteristics based on assumptions
incident =incident %>% filter(!is.na(Domestic_Violence))
sapply(incident, function(x) sum(is.na(x)))
#the na rows cannot be explained into an yes or a no based on other characteristics. Dropping them form the data
incident =incident %>% filter(Domestic_Violence != 'N/A')

#Check each column for missing values and print
sapply(incident, function(x) sum(is.na(x)))
colSums(is.na(incident))

#Handling Targets
incident %>% filter(is.na(Targets))
table(incident$Situation,incident$Targets)
#assigning the value of unknown to null targets
incident$Targets[is.na(incident$Targets)] = 'unknown'
table(incident$Targets)
table(incident$Situation,incident$Targets)
unknowntargets = incident %>% filter(incident$Targets == 'unknown')
View(unknowntargets)
#assign target as 'Victims Targeted' when the situation is escalation of dispute
table(incident$Situation,incident$Targets)
for (i in nrow(incident)) {
  incident$Targets = ifelse(incident$Targets == 'unknown' & incident$Situation == 'Escalation of Dispute', 'Victims Targeted', incident$Targets)
  incident$Targets = ifelse(incident$Targets == 'unknown' & incident$Situation == 'Indiscriminate Shooting', 'Random Shooting', incident$Targets)
  incident$Targets = ifelse(incident$Targets == 'unknown' & incident$Situation == 'Officer-Involved Shooting', 'Victims Targeted', incident$Targets)
  incident$Targets = ifelse(incident$Targets == 'unknown' & incident$Situation == 'Drive-by Shooting', 'Both', incident$Targets)
  incident$Targets = ifelse(incident$Targets == 'unknown' & incident$Situation == 'Intentional Property Damage', 'Random Shooting', incident$Targets)
  
} #leaves 8 rows with unknowns. we will have to drop these, as we cannot make assumption on the targets for during an illegal activitu, and for situation of unknown nature
incident %>% filter(incident$Targets == 'unknown')
incident = incident %>% filter(incident$Targets != 'unknown')

#checkign for missing values in incident
colSums(is.na(incident))

#For Shooter
#checking for nulls in dataframe shooter
colSums(is.na(shooter))
#there are no nulls in incident_ID. checking the unique count of incidents in shooter table
length(unique(shooter$Incident_ID))
#2885
#How many unique values of incidents do we have in the incident table
length(unique(incident$Incident_ID))
#690
#what are the duplicates in the shooter table
duplicated(shooter$Incident_ID)
#Joining the shooter table to the incident table using a left join on incident by incident_id
intermediate = left_join(incident,shooter,by = 'Incident_ID')
str(intermediate)
#how many nulls do we have in the new intermediate table
colSums(is.na(intermediate))
#The increase in row numbers from incident df to intermediate suggests that there could be multiple shooters in incidents

#handling Age in Shooter table
table(intermediate$Age)
#shooters under the age 18 can be assigned to Minor
for (i in nrow(intermediate)) {
  intermediate$age.new[i] = ifelse(intermediate$Age[i] < 18 | intermediate$Age[i] == 'Minor', 'Minor', 'Adult')
}
table(intermediate$age.new,intermediate$Age)
#We created a new feature that categorizes shooters based on age into Minors or Adults
#intermediate <- intermediate %>% select(-Age)
#How many nulls are there in Age?
colSums(is.na(intermediate))
#103
  intermediate$age.new = ifelse(is.na(intermediate$age.new), "Unknown", intermediate$age.new)
table(intermediate$age.new)
table(intermediate$age.new,intermediate$School_Affiliation)
#Based on the school affiliationn we can assign the age of the 2 unknown rows i.e SRO
for (i in nrow(intermediate)) {
  intermediate$age.new[i] = ifelse(intermediate$age.new[i]=='Unknown' & intermediate$School_Affiliation[i] == 'Police Officer/SRO', 'Adult', intermediate$age.new[i])
  }
table(intermediate$age.new,intermediate$School_Affiliation)
colSums(is.na(intermediate))

#Dropping the race feature as there are too many nulls (over 500)
intermediate = intermediate %>% select(-Race)

#Gender
#ommiting the null values from the data
#before 775
intermediate = intermediate %>% filter(!is.na(Gender))
#after 681
colSums(is.na(intermediate))

#School affiliation
table(intermediate$School_Affiliation)
barplot(table(intermediate$School_Affiliation), main="Location Type", xlab="School Affiliation", ylab="Frequency", col= colors(),legend = rownames(table(intermediate$School_Affiliation)), beside=FALSE)
intermediate$School_Affiliation = ifelse(is.na(intermediate$School_Affiliation), "Unknown", intermediate$School_Affiliation)
table(intermediate$School_Affiliation)
#what to do with the unknowns?

colSums(is.na(intermediate))

#Joining intermediate with weapon

m0 = left_join(intermediate,weapon, by='Incident_ID')
colSums(is.na(m0))
#dropping weapon details form weapon, it has too many nulls
m0 = m0 %>% select(-Weapon_Details)
colSums(is.na(m0))
unique(m0$Weapon_Caliber)
#droppin weapon calliber as it is not significant for our analysis
m0 = m0 %>% select(-Weapon_Caliber)
#what are the different types of weapons
unique(m0$Weapon_Type)
table(m0$Weapon_Type)
#assigning unknown and NA weapon types as unknown
m0$Weapon_Type = ifelse(is.na(m0$Weapon_Type), "Unknown", m0$Weapon_Type)
table(m0$Weapon_Type)
unique(m0$Weapon_Type)
#assigning no data value as unknown weapon type
for (i in 1:nrow(m0)) {
  m0$Weapon_Type[i] = ifelse(m0$Weapon_Type[i] == 'No Data', 'Unknown', m0$Weapon_Type[i])
  
}
table(m0$Weapon_Type)
unique(m0$Weapon_Type)

str(m0)

# Function to get top 10 unique values
get_top_10_unique <- function(x) {
  unique_vals <- unique(x)
  if(length(unique_vals) > 10) {
    return(head(unique_vals, 10))
  } else {
    return(unique_vals)
  }
}

# Apply the function to each column
result <- lapply(m0, get_top_10_unique)

# Print the results
for(col_name in names(result)) {
  cat("\nTop 10 unique values for", col_name, ":\n")
  print(result[[col_name]])
}



# Columns to be converted to factors
factor_columns <- c(
  "School_Level", "Location_Type", "During_Classes", "Time_Period",
  "Situation", "Targets", "Hostages", "Officer_Involved", "Bullied",
  "Domestic_Violence", "Gang_Related", "State_Gun_Control",
  "Duration_Min_Category", "Gender", "School_Affiliation",
  "Shooter_Outcome", "Shooter_Died", "Injury", "age.new", "Weapon_Type"
)

# Convert the selected columns to factors
m0[factor_columns] <- lapply(m0[factor_columns], factor)

# Verify the changes
str(m0[factor_columns])

str(m0)


#Plotting the Correlation Matrix

library(tidyverse)
library(corrplot)

# First mutate to convert factors to numeric, then select numeric variables
m0_numeric <- m0 %>%
  mutate(
    School_Level = as.numeric(School_Level),
    During_Classes = as.numeric(factor(During_Classes, levels = c("No", "Yes"))),
    State_Gun_Control = as.numeric(factor(State_Gun_Control, levels = c("Low", "Medium", "High"))),
    Duration_Min_Category = as.numeric(factor(Duration_Min_Category, levels = c("Under 1 min", "Over 1 min"))),
    Gender = as.numeric(factor(Gender, levels = c("Female", "Male"))),
    Shooter_Died = as.numeric(factor(Shooter_Died, levels = c("No", "Yes")))
  ) %>%
  # Now select only the numeric variables for correlation analysis
  select(Number_Victims, Shooter_Killed, Duration_min, Shots_Fired, 
         School_Level, During_Classes, State_Gun_Control, 
         Duration_Min_Category, Gender, Shooter_Died)

# Calculate the correlation matrix
cor_matrix <- cor(m0_numeric, use = "pairwise.complete.obs")
print(cor_matrix)


library("PerformanceAnalytics")
chart.Correlation(m0_numeric, use = "pairwise.complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)



#Histogram of dependent variables

library(ggplot2)
library(gridExtra)

# Function to create histogram or bar plot depending on variable type
create_histogram_or_barplot <- function(data, column, title) {
  if (is.factor(data[[column]]) || is.character(data[[column]])) {
    # Bar plot for categorical variables
    p <- ggplot(data, aes_string(x = column)) +
      geom_bar(fill = "lightblue", color = "black") +
      ggtitle(title) +
      theme_minimal()
    
    # Special handling for Shooter_Outcome
    if (column == "Shooter_Outcome") {
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
        scale_x_discrete(labels = function(x) substr(x, 1, 10))  # Truncate labels
    }
  } else {
    # Histogram for continuous variables
    p <- ggplot(data, aes_string(x = column)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
      geom_density(color = "red") +
      ggtitle(title) +
      theme_minimal()
  }
  return(p)
}

# Create plots for each variable
p1 <- create_histogram_or_barplot(m0, "During_Classes", "Histogram of During Classes")
p2 <- create_histogram_or_barplot(m0, "Gang_Related", "Histogram of Gang Related")
p3 <- create_histogram_or_barplot(m0, "Shooter_Outcome", "Histogram of Shooter Outcome")
p4 <- create_histogram_or_barplot(m0, "Number_Victims", "Histogram of Number of Victims")
p5 <- create_histogram_or_barplot(m0, "Domestic_Violence", "Histogram of Domestic Violence")
p6 <- create_histogram_or_barplot(m0, "Duration_min", "Histogram of Duration (minutes)")

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)



# For monthly incident counts (time series data), we need to convert it
  
#Finding unique values of some columns
unique(m0$School)
unique(m0$School_Level)
unique(m0$Location_Type)
unique(m0$Time_Period)
unique(m0$Duration_min)
unique(m0$Situation)
unique(m0$Targets)


# counting the unique values of Target column for insights 
 table(m0$Targets)
 

 table(m0$Hostages)
 
 table(m0$Officer_Involved)
 
 table(m0$Bullied)
 table(m0$Domestic_Violence)
  table(m0$Gang_Related)
  
  sapply(m0, function(x) length(unique(x)))
 
  
  # Assuming your dataset is named 'm0'
  result <- lapply(m0, function(col) {
    unique_count <- length(unique(col))
    if (unique_count < 30) {
      return(table(col))
    } else {
      return(NULL)
    }
  })
  
  # Remove NULL elements (columns with 30 or more unique values)
  result <- result[!sapply(result, is.null)]
  
  # Display the results
  for (col_name in names(result)) {
    cat("\n", col_name, ":\n")
    print(result[[col_name]])
  }
  

############ MODELS

m1 = glm(Number_Victims ~ School_Level + Location_Type + Situation + Time_Period + Gang_Related + Weapon_Type + Officer_Involved + Shooter_Killed, family = poisson, data = m0)

m2 =glm(During_Classes ~ School_Level * Time_Period + Location_Type, 
              data = m0, family = binomial)


m3 = glm(Gang_Related ~ School_Level * State_Gun_Control + School_Affiliation + Location_Type + Weapon_Type, 
              data = m0, family = binomial)

m4 =  glm(Number_Victims > 1 ~ Situation * Location_Type + Weapon_Type + School_Level + Gang_Related + Shots_Fired + age.new, 
              data = m0, family = binomial)

library(stargazer)
stargazer(m1, m2, m3, m4, type="text")


library(forecast)
m0$Date <- as.Date(substr(m0$Incident_ID, 1, 8), format = "%Y%m%d")
monthly_counts <- table(cut(m0$Date, breaks = "month"))
ts_data <- ts(monthly_counts, frequency = 12, start = c(year(min(m0$Date)), month(min(m0$Date))))
arima_model <- auto.arima(ts_data)
forecast(arima_model, h = 12)


#CLINE TEST 

# Load necessary libraries
library(car)
library(lmtest)

# Display model summaries
stargazer(m1, m2, m3, m4, type="text")

# 1. Test for Multicollinearity using Variance Inflation Factor (VIF)
vif(m1)
vif(m2)
vif(m3)
vif(m4)

# 2. Test for Linearity (Residuals vs Fitted Plot)
par(mfrow = c(2, 2))
plot(m1, which = 1, main = "Residuals vs Fitted (m1)")
plot(m2, which = 1, main = "Residuals vs Fitted (m2)")
plot(m3, which = 1, main = "Residuals vs Fitted (m3)")
plot(m4, which = 1, main = "Residuals vs Fitted (m4)")

# 3. Test for Independence (Durbin-Watson Test)
dwtest(m1)
dwtest(m2)
dwtest(m3)
dwtest(m4)

# 4. Test for Normality of Residuals
par(mfrow = c(2, 2))
qqnorm(residuals(m1), main = "Q-Q Plot (m1)")
qqline(residuals(m1), col = "red")
qqnorm(residuals(m2), main = "Q-Q Plot (m2)")
qqline(residuals(m2), col = "red")
qqnorm(residuals(m3), main = "Q-Q Plot (m3)")
qqline(residuals(m3), col = "red")
qqnorm(residuals(m4), main = "Q-Q Plot (m4)")
qqline(residuals(m4), col = "red")

# Shapiro-Wilk normality test
shapiro.test(residuals(m1))
shapiro.test(residuals(m2))
shapiro.test(residuals(m3))
shapiro.test(residuals(m4))

# 5. Test for Homoscedasticity (Equal Variance)
# Breusch-Pagan Test
bptest(m1)
bptest(m2)
bptest(m3)
bptest(m4)

# Non-constant Variance Score Test (NCV test)
ncvTest(m1)
ncvTest(m2)
ncvTest(m3)
ncvTest(m4)

# Reset plot layout to default
par(mfrow = c(1, 1))





