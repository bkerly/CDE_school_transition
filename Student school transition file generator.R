# School changes!

# Setup environment -------------------------------------------------------


#Add in packages and functions I like-----
library(tidyverse)
library(readr)
library(readxl)
library(DBI)
library(lubridate)

#Define Brian's Favorite Utility Function
#It's like %in%, but not.
"%!in%" <- function(x,y)!('%in%'(x,y))

#Calculate age
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}


#Load in CDE Data-------------
##The October 21-22 data was provided separately, so I'll pull it in separately.
October.21.22.CDE.Data <- read_excel("CDE Enrollment Data/2021-2022_StudentAttendingNotAttendingBrickandMortar.xlsx") %>%
  mutate(last_name = iconv(`Last Name`, "ASCII", "UTF-8", sub=""),
         first_name = iconv(`First Name`, "ASCII", "UTF-8", sub="")) %>%
  mutate(first_name = toupper(first_name),
         last_name = toupper(last_name))  %>%
  mutate(dob = as.Date(parse_date_time(DOB,orders="ymd"))) %>%
  rename(school_code = `Sch Code`,
         district_code = `Distr Code`,
         school_name = `School Name`,
         district_name = `Distr Name`,
         grade = `Grade Level`) #%>%
  #select(first_name,last_name,DOB,school_code,school_name,district_code,district_name,grade)


##The school and district code data is extracted from the October 21-22 data
###There's a file "organization codes with county" that CDE gave us, but it doesn't have district names
SchoolDistrictCodes <- October.21.22.CDE.Data %>%
  select(district_code,school_code,district_name,school_name) %>%
  unique()

SchoolEnrollment <-  October.21.22.CDE.Data %>%
  group_by(school_code,school_name) %>%
  summarize(enrollment = n())

##The October 20-21 data is in a different format, so I'll pull it in separately.
October.20.21.CDE.Data <- read_excel("CDE Enrollment Data/2020-2021StudentOctober_Students_WithoutParentsResidence.xlsx") %>%
  mutate(last_name = iconv(`Last Name`, "ASCII", "UTF-8", sub=""),
         first_name = iconv(`First Name`, "ASCII", "UTF-8", sub="")) %>%
  mutate(first_name = toupper(first_name),
         last_name = toupper(last_name))  %>%
  mutate(dob = as.Date(parse_date_time(DOB,orders="ymd"))) %>%
  rename(school_code = `School Code`,
         district_code = `District Code`,
         school_name = `School Name`,
         district_name = `District Name`)# %>%
  #select(first_name,last_name,DOB,school_code,school_name,district_code,district_name)


##The other data is consistently formatted
##Create a function that will handle the rest of the data sets
CDECleanR <- function(x){
  x %>%
    mutate(first_name = iconv(FIRST_NAME_STUDENT, "ASCII", "UTF-8", sub="") %>%
                toupper(),
              last_name= iconv(LAST_NAME_STUDENT, "ASCII", "UTF-8", sub="") %>%
                toupper(),
              DOB = parse_date_time(BIRTH_DATE_STUDENT,orders = "mdy"),
              school_code = SCHOOL_CODE,
              district_code = DISTRICT_CODE) %>%
    left_join(SchoolDistrictCodes, by = c("school_code","district_code")) %>%
    return()
}

##Extract data from each of the data sets
March.1.20.21.CDE.Data <- read_excel("CDE Enrollment Data/March 1 2020 Enrollment.xlsx") %>%
  CDECleanR()


# Find change ages --------------------------------------------------------

# For each school, find the maximum grade (trimming off the top 1% in case it is weird)

# First, we need to convert grades to numeric. But some are weird! If 120 is 12th grade, what is K? What is pre-K?
October.21.22.CDE.Data %>%
  group_by(school_code) %>%
  filter(grade %in% c(
    "004","006","007","010"
  )) %>%
  group_by(grade) %>%
  slice_sample(n= 10) %>%
  mutate(age = age(DOB)) %>%
  summarize(mean_age = mean(age)) %>% view()

# Based on this, we will call grade 04 as -1 (preK), and grades 006 and 007 as 0 (kindergarden), then all future grades will be their numerical value divided by 10.

October.21.22.CDE.Data <- October.21.22.CDE.Data %>%
  mutate(grade_num = 
           case_when(
             grade %in% "004" ~ -1,
             grade %in% c("006","007") ~ 0,
             TRUE ~ as.numeric(grade)/10
           ) #/ case when
         ) #/ mutate

# Let's plot that to make sure it makes sense

October.21.22.CDE.Data %>%
  transmute(grade_num = as.factor(grade_num),
            age = age(DOB)) %>%
  ggplot(aes(x = grade_num,y=age)) +
  geom_boxplot()+
  labs(title = "Current Age by Grade Level",
       subtitle = "The point is just to make sure it's stepwise.")+
  xlab("Grade Level")+
  ylab("")

# Output a dataframe of max grades by school

school_transition_grades <- October.21.22.CDE.Data %>%
  left_join(SchoolEnrollment) %>%
  group_by(school_code) %>%
  arrange(desc(grade_num)) %>%
  slice_tail(prop = 0.97) %>%
  summarize(max_grade = max(grade_num))
  

# Find change destinations ------------------------------------------------

# For students in the max age of each school in 19-20 and 20-21, see where they went in 21-22

trans_dest_df <- October.21.22.CDE.Data %>%
  select(first_name,last_name,DOB,school_code,grade_num) %>% # choose only useful columns
  rename(school_code_21 = school_code,
         grade_num_21 = grade_num) %>% # code changeable data by year (_21)
  
  # Load in 20-21 data (_20)
  inner_join(October.20.21.CDE.Data %>%
              select(first_name,last_name,DOB,school_code) %>%
              rename(school_code_20 = school_code),
            
            by=c("first_name",
                 "last_name","DOB")
            )  %>% #/ inner_join
  mutate(grade_num_20 = grade_num_21 -1) %>%
  
  # Load in 19_20 data
  inner_join(March.1.20.21.CDE.Data %>%
              select(first_name,last_name,DOB,school_code) %>%
              rename(school_code_19 = school_code),
            
            by=c("first_name",
                 "last_name","DOB")
  )  %>% #/ inner_join
  mutate(grade_num_19 = grade_num_20 -1)


# For 2019, figure  out where kids go

dest_19 <- trans_dest_df %>%
  group_by(school_code_19,grade_num_19,school_code_20) %>%
  summarize(frequency = n()) %>%
  ungroup()

max_dest_19 <- dest_19 %>%
  group_by(school_code_19,grade_num_19) %>%
  arrange(desc(frequency)) %>%
  slice_max(frequency,n=1) %>%
  rename(year_1_code = school_code_19,
         year_1_grade_num = grade_num_19,
         year_2_code = school_code_20)

# For 2020, figure out where kids go
dest_20 <- trans_dest_df %>%
  group_by(school_code_20,grade_num_20,school_code_21) %>%
  summarize(frequency = n()) %>%
  ungroup()

max_dest_20 <- dest_20 %>%
  group_by(school_code_20,grade_num_20) %>%
  arrange(desc(frequency)) %>%
  slice_max(frequency,n=1) %>%
  rename(year_1_code = school_code_20,
         year_1_grade_num = grade_num_20,
         year_2_code = school_code_21)

# Let's do a join to compare!
max_dest_19 %>%
  inner_join(max_dest_20,
            by=c("year_1_code",
                "year_1_grade_num"),
            suffix= c("_19","_20")) %>%
  mutate(same = (year_2_code_19 == year_2_code_20)) %>%
  group_by(year_1_code,year_1_grade_num) %>%
  summarize(concordant = sum(same/n(),na.rm=TRUE)) %>%
  ggplot(aes(x=concordant))+
  geom_histogram(bins = 10) +
  labs(title = "Percent Similarity between 19-20 and 20-21 Transition Assignments")+
  xlab("Percent Concordance")+
  ylab("Number of School/Grade Pairs")

# It also appears from this data that if someone's grade number is 12, they are likely to graduate

# Alright, so they're similar, but not identical. Let's just the higher frequency as the tiebreaker
adv_assignments <- max_dest_19 %>%
  full_join(max_dest_20,
             by=c("year_1_code",
                  "year_1_grade_num"),
             suffix= c("_19","_20")) %>%
  mutate(same = (year_2_code_19 == year_2_code_20)) %>%
  mutate(same = replace_na(FALSE)) %>%
  transmute(year_1_code = year_1_code,
            year_1_grade_num = year_1_grade_num,
            year_2_code = case_when(
              year_1_grade_num >= 12 ~ NA_character_,
              same == TRUE ~ year_2_code_20,
              !is.na(year_2_code_20) ~ year_2_code_20,
              TRUE ~ year_2_code_19
            )) %>%
  ungroup() %>%
  tidyr::complete(.,year_1_code,year_1_grade_num) %>%
  group_by(year_1_code) %>%
  arrange(desc(year_1_grade_num)) %>%
  tidyr::fill(.,year_2_code,.direction = "downup"
  ) %>%
  ungroup() 
# 

# Validation --------------------------------------------------------------

# Total accuracy
prediction_acc_all <-trans_dest_df %>%
  left_join(adv_assignments,
            by=c(
              "school_code_20" = "year_1_code",
              "grade_num_20" = "year_1_grade_num"
            )) %>%
  mutate(same = (school_code_21 == year_2_code)) %>%
  summarize(concordant = sum(same/n(),na.rm=TRUE)) 

prediction_acc <- trans_dest_df %>%
  left_join(adv_assignments,
            by=c(
              "school_code_20" = "year_1_code",
              "grade_num_20" = "year_1_grade_num"
            )) %>%
  mutate(same = (school_code_21 == year_2_code)) %>%
  group_by(school_code_20,grade_num_20) %>%
  summarize(concordant_prediction = sum(same/n(),na.rm=TRUE))

# Compare to not doing anything

no_prediction_acc_all <- trans_dest_df %>%
  mutate(same = (school_code_21 == school_code_20)) %>%
  summarize(concordant = sum(same/n(),na.rm=TRUE)) 

no_prediction_acc <-trans_dest_df %>%
  mutate(same = (school_code_21 == school_code_20)) %>%
  group_by(school_code_20,grade_num_20) %>%
  summarize(concordant_no_prediction = sum(same/n(),na.rm=TRUE)) 

left_join(no_prediction_acc,prediction_acc,by=c("school_code_20","grade_num_20")) %>%
  ggplot()+
  geom_histogram(aes(x=concordant_prediction),fill="green",alpha = 0.5,
                 bins=30)+
  geom_histogram(aes(x=concordant_no_prediction),fill="red",alpha = 0.5,
                 bins=30)+
  labs(title = "Accuracy of predictions (green) vs carry forward method (red)",
       subtitle = "By school/grade pairs")+
  xlab("Percent concordance")+
  ylab("")

# Generate 22-23 data -----------------------------------------------------

# Go through the 21-22 data and flag transitioning students

# Move tranitioning students to their new school

# Increase everyone's grade by 1

# Ouput the data


# Estiamte impact ---------------------------------------------------------

# See how many students 
# are in the same school
# Are in a new school
# Are no longer in school

