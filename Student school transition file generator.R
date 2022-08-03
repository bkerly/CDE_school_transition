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
  mutate(grade_num_19 = grade_num_20 -1) %>%
  
  # 
  
  

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

