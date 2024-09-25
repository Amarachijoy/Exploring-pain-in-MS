#Exploring pain associated with Multiple sclerosis (MS)


#Setting work directory and uploading raw datasets

getwd()
load_dir <- "S:/Datathon - UK MS Register Datathon/Interns/Joy"
setwd(load_dir)

#Loading and reading the data using the read.csv 
demographics <- read.csv("S:/Datathon - UK MS Register Datathon/Interns/Joy/Demographics July 2024.csv")
symptoms <- read.csv("S:/Datathon - UK MS Register Datathon/Interns/Joy/Symptoms July 2024.csv")

#Cleaning data and removing white spaces, Null, and na, and keeping characters as characters not factors.
demographics <- read.csv("S:/Datathon - UK MS Register Datathon/Interns/Joy/Demographics July 2024.csv", na.strings = c('', 'NULL', 'na'), strip.white = T,
         stringsAsFactors = F)
symptoms <- read.csv("S:/Datathon - UK MS Register Datathon/Interns/Joy/Symptoms July 2024.csv", na.strings = c('', 'NULL', 'na'), strip.white = T,
                     stringsAsFactors = F)
demographics <- unique(demographics)

#Removing the commas in the symptoms dataset
#Not all the columns has the comma so it is better to identify columns with only commas so as not to affect the other columns.
#Checking the symptoms table, only the column with 'severity' has commas.
Severity_with_commas <- grep("severity", names(symptoms), value = TRUE)
#The columns with the word 'severity' in it are now identified and brought together. 
#The idea of this whole argument is to ensure the format style for other columns are not affected or changed when running the argument that cleans the commas.
symptoms_clean <- symptoms %>% mutate(across(all_of(Severity_with_commas), ~ as.numeric(gsub(",", "", .))))
symptoms_clean <- unique(symptoms_clean)
#When you open the symptoms_clean df, you would notice the commas has been removed while still maintaining the date format of all other columns without the commas. 
#Without identifying only the columns with commas, the argument might affect the format of other columns 
#Since only the columns with 'severity' has got commas, gather all of them and then clean by/with the identified columns.


#Renaming 'UserId' in both datasets to ensure accuracy. 
demographics <- demographics %>% rename(UserId = ï..UserId)
symptoms_clean <- symptoms_clean %>% rename(UserId = ï..UserId)

#Removing NA from the pain column and retaining only patients that said they experienced pain.

symptoms_clean <- symptoms_clean %>% filter(!is.na(Pain__severity))

#Creating the df 'Pain' to analyse people that said they experience pain. 

Pain <- symptoms_clean %>% select(UserId, Pain__since, Pain__severity, CompletedDate) %>% 
  group_by(UserId) %>% arrange(desc(CompletedDate)) %>% filter(row_number() == 1) %>% ungroup()

#Creating Age, OnsetDate, and DiagonosisDate with the demographics df

#Age
demographics %>% count(YearOfBirth)
demographics <- demographics %>% mutate(Age = 2024 - YearOfBirth)

#OnsetAge
#Cleaning the SymptomYear column to check for errors in year format.
demographics %>% count(SymptomsYear)
#The argument above checks out for format errors. When I run it, I get to see 0, and numbers less than the year of Birth
#which is not right cos symptomsyear cannot be less than the year of birth (of course you have to be born to experience symptoms).
#To better check for this, I ran the summary for YearofBirth to know the maximum year of birth and then, any number used for cleaning must be less than the max YOB.
demographics$SymptomsYear[which(demographics$SymptomsYear == 0)] <- NA
demographics$SymptomsYear[which(demographics$SymptomsYear < 2005)] <- NA



#Add the column 'OnsetAge'
demographics <- demographics %>% mutate(OnsetAge = SymptomsYear- YearOfBirth)

#DiagnosisAge
demographics %>% count(DiagnosisYear)
#Checking for errors in date format
#0, 12, 20, 2033, 3008, 20110 these are wrong dates written by patients while filling the form.
demographics$DiagnosisYear[which(demographics$DiagnosisYear < 20)] <- NA
demographics$DiagnosisYear[which(demographics$DiagnosisYear == 2033 )] <- 2003
demographics$DiagnosisYear[which(demographics$DiagnosisYear < 2005)] <- NA
demographics$DiagnosisYear[which(demographics$DiagnosisYear == 3008)] <- 2008
demographics$DiagnosisYear[which(demographics$DiagnosisYear == 20110)] <- 2011
#Each wrong dates has now been written correctly and also '2005' was added with respect to the maximum year of birth. 

#Add column for DiagnosisAge
demographics<- demographics %>% mutate(DiagnosisAge = DiagnosisYear- YearOfBirth)

#Check rows where DiagnosisAge is less than the OnsetAge.
#Practically, DiagnosisAge can not be less than OnsetAge.
inconsistent_rows <- which(demographics$DiagnosisYear < demographics$SymptomsYear)
demographics$DiagnosisYear[inconsistent_rows] <- NA
demographics$SymptomsYear[inconsistent_rows] <- NA


#Joining the two datasets. Joining the demo and Pain table brings all the data 
#in one table.

Data <- demographics %>% inner_join(Pain, by = "UserId")
Data <- Data %>% filter(MSTypeNow!= 'Benign')

#Finding the mean for Age, OnsetAge, and DiagnosisAge.
mean_Age <- mean(Data$Age, na.rm = T)
print(mean_Age)
mean_OnsetAge <- mean(Data$OnsetAge, na.rm = T)
print(mean_OnsetAge)
mean_DiagnosisAge <- mean(Data$DiagnosisAge, na.rm = T)
print(mean_DiagnosisAge)

# Cleaning the data - get rid of 'Prefer not to say' from gender and get rid of 'Unknown' MSTypeNow
Data <- Data %>%
  mutate(Gender = recode(Gender, 'Prefer not to say' = NA_character_),
         MSTypeNow = recode(MSTypeNow, 'Unknown' = NA_character_))
Data <- Data %>% filter(!is.na(Gender))
Data <- Data %>% filter(!is.na(MSTypeNow))
#Creating a tableone that displays the level of pain severity associated with other variables.
variables <- c('Age', 'OnsetAge', 'DiagnosisAge', 'MSTypeNow', 'MSAtDiagnosis', 'Gender')
strata = 'Pain__severity'
pain_table1 <- CreateTableOne(data = Data, strata = strata, vars = variables)
print(pain_table1)
#According to the table, 664 reported mild pain, 1542 reported moderate pain, and 678 reported severe pain. 
#It can also be deduced that as the age progresses, people living with MS experiences severe pain. However, people tend to experience moderate pain at both their onset and Diagnosis Age as it progresses to severe painas they grow older.
#There's also a difference between age and pain severity levels.
#Furthermore, a higher % of people living with PPMS experience severe pain compared to people living with RRMS and other types of MS.It also shows a significant difference. 
#For Gender, there's no significant difference between pain severity levels and gender. 
#However, the table shows a higher % of males experiencing severe pain which can be drawn from the 'gender_table1' where more males has PPMS, which also reported to be severe. 
#QUESTION: Why is there no significant difference even though more males experience PPMS? 
#Moreover, looking at the tableone stratified by pain severity , there's no much difference in the percentage of pain severity. 
#Around 25% of males experience mild pain and 25% severe pain. 

#Creating a tableone that displays the association of gender and other variables like Age, OnsetAge, and so on. 
variables <- c('Age', 'OnsetAge', 'DiagnosisAge', 'MSTypeNow', 'MSAtDiagnosis', 'Gender', 'Pain__severity')
strata = 'Gender'
gender_table1 <- CreateTableOne(data = Data, strata = strata, vars = variables)
print(gender_table1)
#From the table displayed in the console, there are 2187 females and 690 males which indicates more females in this report.
#Also, the p-value shows a statistical difference between age and gender.As well as the onsetAge and Diagnosis age. 
#A high % of males reported PPMS than women while a higher % of females reported RRMS than males. Also, MSType for females is reported to benign compared to their male counterparts. Additionally, there's a statistical difference in MStype and gender.


#Creating a tableone for Age
variables <- c('Age', 'OnsetAge', 'DiagnosisAge', 'MSTypeNow', 'MSAtDiagnosis', 'Gender')
strata = 'Age'
Age_table1 <- CreateTableOne(data = Data, strata = strata, vars = variables)
print(Age_table1)

#Creating tableone for Onset Age
variables <- c('Age', 'OnsetAge', 'DiagnosisAge', 'MSTypeNow', 'MSAtDiagnosis', 'Gender')
strata = 'OnsetAge'
OnsetAge_table1 <- CreateTableOne(data = Data, strata = strata, vars = variables)
print(OnsetAge_table1)


#Analysing the level of pain severity and level of disability caused by MS. Does people with severe pain experience more disability(Physical Disability) 

#Loading the MSIS and HADS datasets
getwd()
load_dir <- "S:/Datathon - UK MS Register Datathon/Interns"
MSIS <- read.csv("S:/Datathon - UK MS Register Datathon/Interns/MSIS_20240806.csv")
HADS <- read.csv("S:/Datathon - UK MS Register Datathon/Interns/HADS_20240806.csv")

#Cleaning the datasets
MSIS <- read.csv("S:/Datathon - UK MS Register Datathon/Interns/MSIS_20240806.csv", na.strings = c('', 'NULL', 'na', 'Null'), strip.white = T, stringsAsFactors = F)
MSIS <- MSIS %>% rename(CombinedUserId = ï..CombinedUserId)
MSIS <- MSIS %>% select(-'CombinedUserId', -'OldUserId')

HADS <- read.csv("S:/Datathon - UK MS Register Datathon/Interns/HADS_20240806.csv", na.strings = c('', 'NULL', 'na', 'Null', 'null'), strip.white = T, stringsAsFactors = F)
HADS <- HADS %>% rename(CombinedUserId = ï..CombinedUserId)
HADS <- HADS %>% select(-'CombinedUserId', -'OldUserId')

#Getting the earliest entry person for each df.
MSIS <- MSIS %>% group_by(UserId) %>% arrange(CompletedDate) %>% filter(row_number() == 1)
#This argument selects the earliest entry of each person and not the recent entry. This is done to get the people with UserId not before 2018 as the Pain datasets contains reports in 2018.

HADS <- HADS %>% group_by(UserId) %>% arrange(CompletedDate) %>% filter(row_number() == 1)

#Joining the MSIS and HADS df to create one df. 
#First, I will remove the 'NA' from both df and select the columns I need for this study. 
MSIS <- MSIS %>% select(UserId, PhysicalScoreNorm) %>% filter(!is.na(UserId) & !is.na(PhysicalScoreNorm))
#Renaming PhysicalScoreNorm column to MSISPhys_baseline
MSIS <- MSIS %>% rename(MSISPhys_baseline = PhysicalScoreNorm)

HADS <- HADS %>% rename(HADS_anxiety_baseline = Anxiety_total, HADS_depression_baseline = Depression_total)
HADS <- HADS %>% select(UserId, HADS_anxiety_baseline, HADS_depression_baseline) %>% filter(!is.na(UserId) & !is.na(HADS_anxiety_baseline) & !is.na(HADS_depression_baseline))                                                

#Joining the two df
MSIS_HADS <- MSIS %>% inner_join(HADS, by = 'UserId')
#Joining with the Data df that got the demographics and Pain severity. 
DaPain_MSISHADS <- Data %>% inner_join(MSIS_HADS, by = 'UserId')
#The argument below places CompletedDate at the far right.
DaPain_MSISHADS <- DaPain_MSISHADS %>% select(-CompletedDate, CompletedDate)

#Creating tableone for DaPain_MSISHADS and stratifying by pain.

variables <- c('Age', 'OnsetAge', 'DiagnosisAge', 'MSTypeNow', 'MSAtDiagnosis', 'Gender', 'HADS_anxiety_baseline', 'HADS_depression_baseline','MSISPhys_baseline')
strata <- 'Pain__severity'
DaPain_MSISHADS_table1 <- CreateTableOne(data = DaPain_MSISHADS, strata = strata, vars = variables)
print(DaPain_MSISHADS_table1)
#The result of the tableone stratified by pain severity displays the intersection of pain in MS with numerous variables 
#Studies has shown that severe pain is associated with worse depression and anxiety of which the tableone below supports.
#This suggests the possibility that the presence of one condition in MS amplifies the severity of another in MS.
#For physical disability, patients with severe pain experience an increased level of disability. In essence, patients experiencing PPMS associated with an increased pain are likely to be disabled.
#Two key questions that can be culled out from this analysis are; Does pain precedes mood disorders(anxiety and depression) and physically disability? 
#Does pain have a higher impact on mood disorders and physical disability or does mood disorders and physical disability have a higher impact on pain?
#Severe Pain has been reported to affect the quality of life of people living with MS which includes family time,reaching life goals, life satisfaction, employment, ability to attend social groups, or just do their day-to-day activities.
#When MS patients experience more pain, all of these activities that boosts their moods reduces and as a result, depression and anxiety kicks in. 
#Adopting similar position, the presence of severe pain predicts a higher level of MS-related disability. When a person living with MS experiences severe pain, they are unable to walk independently, make it to work, or even engage in physical activities. This could, in essence, classify them as physically disabled as a result of pain. 
#However, it can not be completely concluded that pain has a higher impact on mood disorders and physical disability or vice versa, but it could be summarised from this analysis that increased pain intertwines with mood disorders and level of physical disability in multiple sclerosis. 

#Creating charts for different variables.
hist(DaPain_MSISHADS$Pain__severity)
hist(DaPain_MSISHADS$MSISPhys_baseline)
hist(DaPain_MSISHADS$HADS_depression_baseline)
hist(DaPain_MSISHADS$HADS_anxiety_baseline)

#Statistical analysis to confirm the results of the tableone.

#To test for significant association between gender and pain severity
chisq.test(DaPain_MSISHADS$Gender, DaPain_MSISHADS$Pain__severity)
#The result (p-value = 0.4965)confirms that there's no significant association between gender and pain severity. Women experience severe pain as much as men and vice versa.#
#To test for signicant association between age and pain severity
chisq.test(DaPain_MSISHADS$Age, DaPain_MSISHADS$Pain__severity)
#There is a significant association between age and pain severity. From the tableone, it can seen that older people experience more pain.
chisq.test(DaPain_MSISHADS$MSTypeNow, DaPain_MSISHADS$Pain__severity)
#The above shows a significant association between MSType and pain severity. The tableone shows that people with PPMS experience severe pain.
chisq.test(DaPain_MSISHADS$Pain__severity, DaPain_MSISHADS$MSISPhys_baseline)
#There's a significant association between level of physical disability and pain severity. The average score of people with physical disability that experiences severe pain is higher. 
chisq.test(DaPain_MSISHADS$Pain__severity, DaPain_MSISHADS$HADS_anxiety_baseline)
chisq.test(DaPain_MSISHADS$Pain__severity, DaPain_MSISHADS$HADS_depression_baseline)
#The above shows a significant association between pain severity and mood disorders. The average score of people that experiences severe pain is higher. That is, people with severe pain experiences more mood disorders. 


tableone_csv <- print(DaPain_MSISHADS_table1, cramVars = c("Gender"))
write.csv(tableone_csv, file = 'tableone_name.csv')

tablegender_csv <- print(gender_table1, cramVars = c("Gender"))
write.csv(tablegender_csv, file = 'tablegender_name.csv')


#Creating histograms and bar plots
#Since Anxiety is a continuous variable and Pain severity is a categorical variable, creainga a bar plot would require calculating the mean of anxiety across
#each level of pain. 
data_plot <- aggregate(HADS_anxiety_baseline ~ Pain__severity, data = DaPain_MSISHADS, mean)

ggplot(data_plot, aes(x = as.factor(Pain__severity), y = HADS_anxiety_baseline,  fill = as.factor(Pain__severity))) + 
  geom_bar(stat = "identity") + labs(title = "Distribution of pain and anxiety", x = "Pain severity", y = "Anxiety baseline", col = "#159290") + 
  scale_fill_manual(values = c("0" = "Dark Orange", "1" = "Dark blue", "2"= "#159290")) + theme_minimal()
 
#Find mean for depression across pain severity levels
data_plot_2 <- aggregate(HADS_depression_baseline ~ Pain__severity, data = DaPain_MSISHADS, mean)

ggplot(data_plot_2, aes(x = as.factor(Pain__severity), y = HADS_depression_baseline,  fill = as.factor(Pain__severity))) + 
  geom_bar(stat = "identity") + labs(title = "Distribution of pain and depression", x = "Pain severity", y = "Depression baseline", col = "#159290") + 
  scale_fill_manual(values = c("0" = "Dark Orange", "1" = "Dark blue", "2"= "#159290")) + theme_minimal()

#Find mean for physical disability across pain severity levels
data_plot_3 <- aggregate(MSISPhys_baseline ~ Pain__severity, data = DaPain_MSISHADS, mean)

ggplot(data_plot_3, aes(x = as.factor(Pain__severity), y = MSISPhys_baseline,  fill = as.factor(Pain__severity))) + 
  geom_bar(stat = "identity") + labs(title = "Distribution of pain and physical disability", x = "Pain severity", y = "Physical disability", col = "#159290") + 
  scale_fill_manual(values = c("0" = "Dark Orange", "1" = "Dark blue", "2"= "#159290")) + theme_minimal()

#Calculating percentage of MSType for creating a bar plot
data_percentage <- DaPain_MSISHADS %>% group_by(Gender, MSTypeNow) %>% summarise(count = n()) %>% mutate(percentage = count / sum(count) * 100)

ggplot(data_percentage, aes(x= MSTypeNow, y = percentage, fill = Gender)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Distribution of gender and MSType", x = "MSType", y = "count", fill = "Gender") + 
  scale_fill_manual(values = c("FEMALE" = "Dark Orange", "MALE"= "#159290")) + theme_minimal()
