###########################
# BPS Pre-Processing 
########################### 

# Overview: This R code performs the pre-processing analyses that underpin the R Shiny dashboards for BPS  

# IMPORTANT: This has been updated to include all students, both Native American and Non-native American 

###############
# Dependencies 
###############

library(data.table)
library(tidyverse)
library(ggiraph)
library(ggplot2)
library(dplyr)
library(patchwork)
library(plotly)
library(gtools)
library(viridis)
library(qs2)

####################
# Load in datasets 
####################

# School Information: SCHOOLS Table
# Student Information: STUDENTS Table
# Enrollment: STUDENTS Table
# Absences: ATTENDANCE Table
# Grades: Standard Grades, Traditional Grades, and Stored Grades Tables
# Discipline Records: INCIDENT Table
# Student Contacts: Table of student contact information per student 

# Bring in data - be mindful of the delimiter in order to make sure data is brought in correctly!

# New data is a bit different, pay attention to where code is changed for temp solutions! We will need to account for this later. 
#setwd("/Users/jlawson77/Desktop/BETA DEPLOYED V5_9.12.25/BPS District_Docker/raw_data")
#setwd("../raw_data") # original April data 

RAW_DIR <- Sys.getenv("BPS_RAW_DIR", unset = "../data_raw")
RAW_DIR <- normalizePath(RAW_DIR, mustWork = FALSE)

# --------------- data old to new -------------------
#setwd("/Users/jlawson77/Desktop/BETA DEPLOYED V5_9.12.25/BPS District_Docker/new_data") # <--- test locally 
#setwd("../data_raw") # later export, test 
files <- list.files(RAW_DIR, full.names = TRUE)
attendance <- read_delim(files[1], delim = ",", escape_double = FALSE, trim_ws = TRUE)
courses <- read_delim(files[2], delim = ",", escape_double = FALSE, trim_ws = TRUE)
discipline <- read_delim(files[3], delim = ",", escape_double = FALSE, trim_ws = TRUE)
schools <- read_delim(files[4], delim = ",", escape_double = FALSE, trim_ws = TRUE)
#schools <- read_delim(files[4], delim = "\t", escape_double = FALSE, trim_ws = TRUE) # <--- for old data 
std_grades <- read_csv(files[5])
stored_grades <- read_delim(files[6], delim = ",", escape_double = FALSE, trim_ws = TRUE)
studentid_conversion <- read_delim(files[7], delim = ",", escape_double = FALSE, trim_ws = TRUE)
students <- read_delim(files[8], delim = ",", escape_double = FALSE, trim_ws = TRUE)
trad_grades <- read_csv(files[9])
contacts <- read_csv(files[10]) # make sure it is named HHS_Contacts.txt

#OUTPUT_CACHE <- "../cache.qs"  
OUTPUT_CACHE <- Sys.getenv("BPS_PUBLISH_QS_PATH", unset = "../cache.qs")

# Compatibility shim: some older runner code may refer to `built_cache`
# Keep it identical to OUTPUT_CACHE so either name works.
built_cache <- OUTPUT_CACHE

#####################
# Helper Functions 
#####################

get_mode <- function(x) {
  ux <- na.omit(x)                # remove NAs
  names(sort(table(ux), decreasing = TRUE))[1]
}

######################################
# Step 1: Student-level File Creation 
######################################

# --- Requires: --- 
# BPS_Students_<month-date-year>.txt (students - enrollment)
# BPS_Schools_<month-date-year>.txt (schools)
# BPS_STUDENTID_CONVERSION_<month-date-year>.txt 

# IMPORTANT Inclusion criteria: ALL currently enrolled students either tribal or non-tribal 

# Load students and remove those without a tribe code
names(students)

# Remove graduated students who are coded 99 for now 
table(students$STUDENTS.Grade_Level)
students<-students[students$STUDENTS.Grade_Level!=99,] # <--------- Removes graduated students  
table(students$STUDENTS.Grade_Level)
student_list<-students

# Keep currently enrolled (i.e., keep .Enroll_status==0)
table(students$STUDENTS.Enroll_Status) # all in this dataset currently enrolled 
student_list$current<-ifelse(student_list$STUDENTS.Enroll_Status==0, 1,0); table(student_list$current)
student_list<-student_list[student_list$current==1,]; table(student_list$current)
students<-students[students$STUDENTS.Enroll_Status==0,]; table(students$STUDENTS.Enroll_Status)

# Signed Consent (clean this up)
# U_STUDENTSUSERFIELDS.info_release == 1 OR “Yes I do agree” OR “p_Agree” 
table(students$U_STUDENTSUSERFIELDS.info_release)
student_list$authorized<-ifelse(student_list$U_STUDENTSUSERFIELDS.info_release=="p_Agree" | 
                                  student_list$U_STUDENTSUSERFIELDS.info_release=="Yes I do authorize", 1, 0); table(student_list$authorized)

# Tribe code - creates indicator for tribal students 
table(students$S_NM_STU_X.tribe)
student_list$tribal_student<-ifelse(student_list$S_NM_STU_X.tribe!="00", 1, 0); table(student_list$tribal_student)
students_dat<-students # <------ backup data set created that includes all students 
total_students<-length(unique(na.omit(students$STUDENTS.ID))); message("Total students is: ",total_students)
tribal_students<-length(unique(na.omit(students_dat[students_dat$S_NM_STU_X.tribe!="00",]$STUDENTS.ID))); message("Tribal students is: ",tribal_students)
prop_no_release<-round(prop.table(table(student_list$authorized))[1],2); message("Proportion of students who did not sign a release is: ",prop_no_release)

# Pull out the vars you want to keep for a student list 
names(student_list)
student_vars_needed<-c("STUDENTS.ID", "STUDENTS.dcid", "STUDENTS.DistrictEntryDate", 
                       "STUDENTS.First_Name", "STUDENTS.Last_Name", "current", "authorized", "tribal_student")
student_list<-student_list[,student_vars_needed]

# Tribal student list 
tribal_student_list<-student_list[,c("STUDENTS.ID", "STUDENTS.dcid", "tribal_student")]

# Merge school names from schools data into student data 
names(schools)
sch<-schools[,c("SCHOOLS.School_Number","SCHOOLS.Name" )]
students_dat<-merge(students_dat, sch, by.x="STUDENTS.SchoolID", by.y="SCHOOLS.School_Number", all.x=TRUE)

# Create a cleaned up, date DOB variable 
students_dat$DOB2<-as.Date(students_dat$STUDENTS.DOB)

# Examine duplicates by Students.DCID (primary key) and Students.Grade_Level
names(students_dat)
students_dat$dups<-paste0(students_dat$STUDENTS.dcid, "_", students_dat$STUDENTS.Grade_Level)
message("Number of duplicates based on DCID and grade is: ", anyDuplicated(students_dat$dups)) # None, good  

# Order by entry date and remove duplicates based on DCID and grade, if they exist 
students_dat<-students_dat[order(students_dat$dups, students_dat$STUDENTS.SchoolEntryDate), ]
students_dat<-students_dat[!duplicated(students_dat[c("dups")], fromLast = TRUE), ]
students_dat$dups<-NULL # remove dups (duplicates) variable 

# Merge into the student data additional identification information (from studentid_conversion file)
names(studentid_conversion)
dat<-studentid_conversion[,c("REQUIRED.05-dcid","REQUIRED.09-school_id","REQUIRED.11-dob",
                             "REQUIRED.04-student_state_id","REQUIRED.03-student_id")]
students_dat<-merge(students_dat, dat, by.x="STUDENTS.dcid", by.y="REQUIRED.05-dcid", all.x=TRUE)

# Create tribe name variable from tribe data 
table(students_dat$S_NM_STU_X.tribe)
students_dat$tribe_name<-recode(students_dat$S_NM_STU_X.tribe, "00" = "Not Applicable", 
                                "01" = "Acoma",
                                "02" = "Cochiti",
                                "03" = "Isleta",
                                "04" = "Jemez", 
                                "05" = "Jicarilla Apache", 
                                "06" = "Laguna", 
                                "07" = "Mescalero Apache",
                                "08" = "Nambe", 
                                "09" = "Navajo", 
                                "10" = "Picuris", 
                                "11" = "Pojoaque", 
                                "12" = "San Felipe", 
                                "13" = "San Ildefonso", 
                                "14" = "Ohkay Owingeh",
                                "15" = "Sandia",
                                "16" = "Santa Ana", 
                                "17" = "Santa Clara",
                                "18" = "Kewa", 
                                "19" = "Taos", 
                                "20" = "Tesuque", 
                                "21" = "Zia", 
                                "22" = "Zuni", 
                                "23" = "Other") 
#students_dat$tribe_name<-ifelse(students_dat$tribe_name=="NA", NA, students_dat$tribe_name); table(students_dat$tribe_name)

# New Race variable 
table(students_dat$STUDENTS.Ethnicity)
students_dat$STUDENTS.Ethnicity2<-recode(students_dat$STUDENTS.Ethnicity, "A" = "Asian", 
                                         "B" = "Black", 
                                         "C" = "Caucasian",
                                         "H" = "Hispanic", 
                                         "I" = "American Indian", 
                                         "P" = "Pacific-Islander"
)
table(students_dat$STUDENTS.Ethnicity2)

# New Gender variable 
table(students_dat$STUDENTS.Gender)
students_dat$STUDENTS.Gender2<-recode(students_dat$STUDENTS.Gender, "F"="Female", 
                                      "M"="Male")
table(students_dat$STUDENTS.Gender2)

############################################################
# Step 2. Create Grade, School, and Tribe Count Data Sets  
############################################################

# Create Grade dataset with counts 
grade_counts <- table(students_dat$STUDENTS.Grade_Level)
grade_df <- data.frame(
  Grade = factor(names(grade_counts), levels=mixedsort(levels(factor(names(grade_counts))))),
  Count = as.numeric(grade_counts)
)
all_grades<- c("-2", "-1", "0", "1", 
               "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
grade_df$Grade<- c("Pre-K 1", "Pre-K 2", "Kindergarten", "1", 
                   "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
order_levels<-c("Pre-K 1", "Pre-K 2", "Kindergarten", "1", 
                "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
grade_df$Grade<-factor(grade_df$Grade, levels=order_levels)

# Custom colors for grades (used later for graphing - alternate between these two colors for grades so graphs are visually better)
grade_colors <- c("lightgrey", "#d62728")
colors <- rep(grade_colors, ceiling(length(grade_df$Grade)/2))[1:length(grade_df$Grade)]

# Create School dataset with counts 
table(students_dat$SCHOOLS.Name)
school_counts <- table(students_dat$SCHOOLS.Name)
school_df <- data.frame(
  School = factor(names(school_counts), levels=mixedsort(levels(factor(names(school_counts))))),
  Count = as.numeric(school_counts)
)

# Create Tribe dataset with counts 
table(students_dat$tribe_name)
tribe<-students_dat %>%
  group_by(tribe_name) %>%
  mutate(unique_students = n_distinct(STUDENTS.dcid))
names(tribe)
tribe<-tribe[,c("tribe_name","unique_students")]
table(tribe$tribe_name)
tribe<-tribe[!duplicated(tribe$tribe_name),]
tribe<-tribe[!is.na(tribe$tribe_name),] # N's of students by tribes 

# ---- This info then goes into Overview Tab in dashboards to produce: ---- 

# Grade distributions by Tribe 
# School distribution by Tribe 
# And additional breakdowns 

#####################
# Step 3. Attendance 
#####################

# --- Requires: ---
# BPS_Attendance_041125.txt (attendance) 
# Also uses student id_conversion data 

# NOTES: 
# This calculates period-level attendance 
# A daily measure could be calculated but this would have to rely on date and use a count of 
# at least one present (up to all present) within id and date block

# To calculate (approximate) attendance at the period-level, we use the following 
# knowledge that we have about the school calendar and period-day structure: 

# Grades 9-12 have 188 days, 7 periods a day 
# Grades 7-8 have 188 days, 7 periods a day 
# Middle schools choose how many periods they have 
# Santo Domingo and Cochiti have 9 periods per day 
# Spartan Learning Academy has 7 periods per day 
# K-6 has 183 days (elementary), with 1 period a day  
# Prek has 157 days, with 1 period a day (Homeroom)

upper_9_12_total_periods<-(188*7)-(6*3.5+37*7); message("Total periods for Grades 9-12 is: ", upper_9_12_total_periods)
upper_7_8_total_periods<-(188*7)-(6*3.5+37*7); message("Total periods for Grades 7-8 is: ", upper_7_8_total_periods)
upper_5_6_total_periods<-(183*5)-(6*3.5+36*7); message("Total periods for Grades 5-6 is: ", upper_5_6_total_periods) 
lower_K_4_total_periods<-(183*1)-(6*0.5+36*1); message("Total periods for Grades K-4 is: ", lower_K_4_total_periods)
lower_preK_total_periods<-(157*1)-(6*0.5+53*1); message("Total periods for Grade PreK is: ",lower_preK_total_periods) 

# Make total periods current - take most recent date in attendance records/ date from file and use this to inform 
# how much of the denominator you should use 

# ---- Scale full-year totals to "to date" using calendar days elapsed ----
# Count distinct school days seen in the current attendance feed
att_dates <- attendance %>%
  dplyr::mutate(att_date = as.Date(`REQUIRED.03-att_date`)) %>%
  dplyr::distinct(att_date)
days_elapsed <- nrow(att_dates)

# Planned school days by band (matches your assumptions)
PLANNED_7_12 <- 188L  # grades 7–12
PLANNED_5_6  <- 183L  # grades 5–6
PLANNED_K_4  <- 183L  # grades K–4
PLANNED_PREK <- 157L  # PreK

# Fractions of the year elapsed (cap to [0,1])
frac_7_12 <- pmin(1, days_elapsed / PLANNED_7_12)
frac_5_6  <- pmin(1, days_elapsed / PLANNED_5_6)
frac_k_4  <- pmin(1, days_elapsed / PLANNED_K_4)
frac_prek <- pmin(1, days_elapsed / PLANNED_PREK)

# Calendar-to-date totals (integers, never < 1)
upper_9_12_total_periods <- max(1L, round(upper_9_12_total_periods * frac_7_12)); message("New total periods for Grades 9-12 is: ", upper_9_12_total_periods)
upper_7_8_total_periods  <- max(1L, round(upper_7_8_total_periods  * frac_7_12)); message("New total periods for Grades 7-8 is: ", upper_7_8_total_periods)
upper_5_6_total_periods  <- max(1L, round(upper_5_6_total_periods  * frac_5_6)); message("New total periods for Grades 5-6 is: ", upper_5_6_total_periods)
lower_K_4_total_periods  <- max(1L, round(lower_K_4_total_periods  * frac_k_4)); message("New total periods for Grades K-4 is: ", lower_K_4_total_periods)
lower_preK_total_periods <- max(1L, round(lower_preK_total_periods * frac_prek)); message("New total periods for Grades Pre-K is: ", lower_preK_total_periods)

# Examine variable names in attendance and studentid_conversion datasets and merge the two 
names(attendance) # use REQUIRED.02-student.id to link 
names(studentid_conversion) # use REQUIRED.03-student_id to link 
# Merge 
attendance<-merge(attendance, studentid_conversion, by.x="REQUIRED.02-student_id", 
                  by.y="REQUIRED.03-student_id", all.x=TRUE)

# # Random spot checks of student id and attendance (uncomment to perform)
# check<-attendance[attendance$`REQUIRED.02-student_id`=="111151312",]
# names(check)
# #View(check[,c(1,4,5,8,15)])
# check<-check[order(check$`REQUIRED.02-student_id`, check$`REQUIRED.03-att_date`),]
# # Check prek
# check2<-attendance[attendance$`REQUIRED.02-student_id`=="133629550",]
# names(check2)
# #View(check[,c(1,4,5,8,15)])
# check2<-check2[order(check2$`REQUIRED.02-student_id`, check2$`REQUIRED.03-att_date`),]

# Get the Count of Total School Days that is represented in data - Not reliable, as students in data have different number of records due to only attendance-related events triggering a reponse 
# table(attendance$`REQUIRED.03-att_date`)
# total_days<-aggregate(attendance$`REQUIRED.03-att_date` ~ attendance$`REQUIRED.02-student_id`, data=attendance, function(x) length(unique(x)))
# names(total_days)<-c("REQUIRED.02-student_id", "total_days_in_data")

# NOTE: Here is where we first initiate, i.e., create, the tracker data set, which is our student-level attendance file 
# We use the variable ATTENDANCE.presence_status_cd to count presences, absences, etc. 
tracker<-aggregate(attendance$ATTENDANCE.presence_status_cd ~ attendance$`REQUIRED.02-student_id`, data=attendance, function(x) sum(x=="Absent"))
names(tracker)<-c("REQUIRED.02-student_id", "periods_absent")

# Get Student ID and grade (unique) from attendance data and merge this into tracker data to create total period count
names(attendance)
att_grades<-attendance[,c(1,5)]
att_grades <- att_grades %>% # this produces a unique student id x grade dataset
  distinct()
# Create total periods 
tracker<-merge(tracker, att_grades, by="REQUIRED.02-student_id", all.x=TRUE)
tracker$total_periods<-ifelse(tracker$ATTENDANCE.grade_level>=9, upper_9_12_total_periods,
                              ifelse(tracker$ATTENDANCE.grade_level<9 & tracker$ATTENDANCE.grade_level>6, upper_7_8_total_periods,
                                     ifelse(tracker$ATTENDANCE.grade_level==6 | tracker$ATTENDANCE.grade_level==5, upper_5_6_total_periods,
                                            ifelse(tracker$ATTENDANCE.grade_level<5 & tracker$ATTENDANCE.grade_level>=0, lower_K_4_total_periods, lower_preK_total_periods))))
# Periods present (need total periods to compute this,i.e., total - absences)
tracker$periods_present<-tracker$total_periods-tracker$periods_absent

# Proportion Absent 
tracker$proportion_absent<-round(tracker$periods_absent/tracker$total_periods, 2)

# Proportion Present 
tracker$proportion_present<-round(tracker$periods_present/tracker$total_periods, 2)

# Excused Absences 
excused_absence<-aggregate(attendance$`REQUIRED.04-att_code` ~ attendance$`REQUIRED.02-student_id`, data=attendance, function(x) sum(x=="EX"))
#periods_present<-aggregate(attendance$ATTENDANCE.presence_status_cd ~ attendance$`REQUIRED.02-student_id` + attendance$`REQUIRED.03-att_date`, data=attendance, function(x) sum(x=="Present"))
names(excused_absence)<-c("REQUIRED.02-student_id", "excused_absence")

# Proportion Excused Absence out of Total Absences
tracker<-merge(tracker, excused_absence, by="REQUIRED.02-student_id")
tracker$proportion_excused_absence<-round(tracker$excused_absence/tracker$periods_absent, 2)

# Count of Excused Full Day Cultural Observance 
excused_culture<-aggregate(attendance$`REQUIRED.04-att_code` ~ attendance$`REQUIRED.02-student_id`, data=attendance, function(x) sum(x=="EFDCO"))
#periods_present<-aggregate(attendance$ATTENDANCE.presence_status_cd ~ attendance$`REQUIRED.02-student_id` + attendance$`REQUIRED.03-att_date`, data=attendance, function(x) sum(x=="Present"))
names(excused_culture)<-c("REQUIRED.02-student_id", "excused_culture")

# Proportion of Total Excused Absences due to Excused Cultural Days 
tracker<-merge(tracker, excused_culture, by="REQUIRED.02-student_id")
tracker$proportion_culture<-round(tracker$excused_culture/tracker$periods_absent, 2)

# Unexcused Absences 
unexcused_absence<-aggregate(attendance$`REQUIRED.04-att_code` ~ attendance$`REQUIRED.02-student_id`, data=attendance, function(x) sum(x=="UX"))
#periods_present<-aggregate(attendance$ATTENDANCE.presence_status_cd ~ attendance$`REQUIRED.02-student_id` + attendance$`REQUIRED.03-att_date`, data=attendance, function(x) sum(x=="Present"))
names(unexcused_absence)<-c("REQUIRED.02-student_id", "unexcused_absence")

# Proportion Excused Absence out of Total Absences
tracker<-merge(tracker, unexcused_absence, by="REQUIRED.02-student_id")
tracker$proportion_unexcused_absence<-round(tracker$unexcused_absence/tracker$periods_absent, 2)

# Perfect Period Attendance Indicator 
tracker$perfect<-ifelse(tracker$proportion_present==1, 1, 0); table(tracker$perfect)
tracker$complete_absence<-ifelse(tracker$proportion_absent==1, 1, 0); table(tracker$complete_absence)

# Truancy Ratio = unexcused absences/(total school days - excused absences)
tracker$truancy_ratio <-round(tracker$unexcused_absence/(tracker$total_periods - tracker$excused_absence), 2)

# Truancy category = Categories: Perfect (0%), Good (1%), Signal (2%), At Risk (5%), Truant (10%)
tracker$truancy_category<-ifelse(tracker$truancy_ratio==0.00, "Perfect", 
                                 ifelse(tracker$truancy_ratio>0.00 & tracker$truancy_ratio < 0.02, "Good", 
                                        ifelse(tracker$truancy_ratio>=0.02 & tracker$truancy_ratio < 0.05, "Signal",
                                               ifelse(tracker$truancy_ratio>=0.05 & tracker$truancy_ratio < 0.10, "At Risk", "Truant"))))
tracker$truancy_category<-ifelse(is.na(tracker$truancy_category), "Not Yet Available", tracker$truancy_category)
table(tracker$truancy_category)
prop.table(table(tracker$truancy_category))

# Chronic Absenteeism Ratio and Category 
tracker$absenteeism_ratio <-round(tracker$periods_absent/(tracker$total_periods), 2)
tracker$absenteeism_category<-ifelse(tracker$absenteeism_ratio==0.00, "Perfect", 
                                     ifelse(tracker$absenteeism_ratio>0.00 & tracker$absenteeism_ratio < 0.02, "Good", 
                                            ifelse(tracker$absenteeism_ratio>=0.02 & tracker$absenteeism_ratio < 0.05, "Signal",
                                                   ifelse(tracker$absenteeism_ratio>=0.05 & tracker$absenteeism_ratio < 0.10, "At Risk", "Chronic"))))
tracker$absenteeism_category<-ifelse(is.na(tracker$absenteeism_category), "Not Yet Available", tracker$absenteeism_category)
table(tracker$absenteeism_category)
prop.table(table(tracker$absenteeism_category))

# Adjusted Chronic Absenteeism = Ratio = (total absences - excused absences)/(total days - cultural days)
tracker$adj_absenteeism_ratio<-round((tracker$periods_absent - tracker$excused_absence)/(tracker$total_periods - tracker$excused_culture), 2)
tracker$adj_absenteeism_category<-ifelse(tracker$adj_absenteeism_ratio==0.00, "Perfect", 
                                         ifelse(tracker$adj_absenteeism_ratio>0.00 & tracker$adj_absenteeism_ratio < 0.02, "Good", 
                                                ifelse(tracker$adj_absenteeism_ratio>=0.02 & tracker$adj_absenteeism_ratio < 0.05, "Signal",
                                                       ifelse(tracker$adj_absenteeism_ratio>=0.05 & tracker$adj_absenteeism_ratio < 0.10, "At Risk", "Chronic"))))
tracker$adj_absenteeism_category<-ifelse(is.na(tracker$adj_absenteeism_category), "Not Yet Available", tracker$adj_absenteeism_category)
table(tracker$adj_absenteeism_category)
prop.table(table(tracker$adj_absenteeism_category))

# Merge in Tribe and School information to tracker data 
names(students_dat)
context<-students_dat[,c(3,40,19,46)]; names(context) # this is ID, economic disadvantage, grade, and student sate ID 
tracker$ATTENDANCE.grade_level<-NULL
dcid<-attendance[,c(1,22)]
dcid<-dcid[!duplicated(dcid$`REQUIRED.02-student_id`),]
names(attendance) # grab dcid to merge on 
tracker<-merge(tracker, dcid, by="REQUIRED.02-student_id", all.x=TRUE)
tracker<-merge(tracker, context, by.x="REQUIRED.05-dcid", by.y="STUDENTS.ID", all.x=TRUE)

# District-wide truancy rate 
district_truancy<-round(mean(tracker$truancy_ratio, na.rm=TRUE),2)

# Order 
#tracker<-tracker[order(tracker$`REQUIRED.05-dcid`, tracker$)]

# ---- ATTENDANCE UPDATE

# Calculate days present and absent from period-level attendance data and merge to tracker data 
# 1. Student × date tallies
day_tallies <- attendance %>%
  transmute(
    `REQUIRED.02-student_id`,
    `REQUIRED.01-school_id`,
    att_date = as.Date(`REQUIRED.03-att_date`),
    grade_level = ATTENDANCE.grade_level,
    is_absent    = ATTENDANCE.presence_status_cd == "Absent" | `REQUIRED.04-att_code` %in% c("EX","UX","EFDCO"),
    is_excused   = `REQUIRED.04-att_code` %in% c("EX","EFDCO"),
    is_unexcused = `REQUIRED.04-att_code` %in% c("UX"),
    full_day_excused = `REQUIRED.04-att_code` %in% c("EFDCO")
  ) %>%
  group_by(`REQUIRED.02-student_id`, `REQUIRED.01-school_id`, att_date, grade_level) %>%
  summarise(
    n_absent    = sum(is_absent,    na.rm = TRUE),
    n_excused   = sum(is_excused,   na.rm = TRUE),
    n_unexcused = sum(is_unexcused, na.rm = TRUE),
    any_full_day_excused = any(full_day_excused, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Default schedule: 7–12 = 7 periods/day; K–6 = 1 mark/day (adjust below if needed)
    periods_per_day = if_else(grade_level >= 7, 7L, 1L),
    # Daily rule: secondary = half-day (>= 50%); elementary = full-day
    threshold       = if_else(grade_level >= 7, 0.5, 1.0),
    required_absent = ceiling(periods_per_day * threshold),
    
    day_absent  = any_full_day_excused | (n_absent    >= required_absent),
    day_excused = any_full_day_excused | (n_excused   >= required_absent),
    day_truant  =                           n_unexcused >= required_absent
  )

# 2. Collapse to per-student day counts
days_by_student <- day_tallies %>%
  group_by(`REQUIRED.02-student_id`) %>%
  summarise(
    days_absent  = sum(day_absent,  na.rm = TRUE),
    days_excused = sum(day_excused, na.rm = TRUE),
    days_truant  = sum(day_truant,  na.rm = TRUE),
    .groups = "drop"
  )

# 3. School-day denominator (calendar days elapsed) per student
days_by_school <- attendance %>%
  distinct(`REQUIRED.01-school_id`, `REQUIRED.03-att_date`) %>%
  count(`REQUIRED.01-school_id`, name = "days_elapsed")
# map each student to their primary school (most records seen)
sid_school <- attendance %>%
  group_by(`REQUIRED.02-student_id`, `REQUIRED.01-school_id`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(`REQUIRED.02-student_id`) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(`REQUIRED.02-student_id`, `REQUIRED.01-school_id`)

student_days_elapsed <- sid_school %>%
  left_join(days_by_school, by = "REQUIRED.01-school_id")

district_days <- max(days_by_school$days_elapsed, na.rm = TRUE)

# 4. Join into tracker and compute days present + rates
tracker <- tracker %>%
  left_join(days_by_student,      by = "REQUIRED.02-student_id") %>%
  left_join(student_days_elapsed, by = "REQUIRED.02-student_id") %>%
  mutate(
    days_elapsed       = coalesce(days_elapsed, district_days),
    days_absent        = coalesce(days_absent,  0L),
    days_excused       = coalesce(days_excused, 0L),
    days_truant        = coalesce(days_truant,  0L),
    days_present       = pmax(0L, days_elapsed - days_absent),
    day_absence_rate   = round(days_absent / pmax(days_elapsed, 1L), 3),
    day_attendance_rate= round(1 - day_absence_rate, 3)
  )


###########
# GRADES 
###########

# ---- Requires: ----- 
# BPS_TRAD_GRADES_<mm-dd-yyyy>.txt 
# BPS_STORED_GRADES_<mm-dd-yyyy>.txt - student course level (this is only needed for the weighted GPA calculation)
# BPS_Students_042225.txt - students data for id linking 
# Possibly incorporate standard grades in the future, leave out for now 

# NOTES: 
# There are a lot of zeros in the Potential Credit column - so a lot of things become 0 when multiplied by that 
# Also stored grades has a lot of missingness, e.g., a lot of term grades are missing
# We rely on traditional grades over stored grades 

# Y1 = End of year and is the Average of S1 and S2 
# Grades are as follows: 
  # Grades 0 - 6 use 1-4 scale and have Y1 pretty much only 
  # Grades 7 - 12 use A-F letter grade scale has Q's, S's, and Y1 time points in the data 

# Calculating Weighted GPA: 
# GPA Points Earned*Potential Credit = Earned GPA Points 

# APPROACH: 
  # Rely on traditional grades to calculate Proportion Letter Grades and GPA (unweighted, much more widely available)
  # Use stored grades ONLY for calculating weighted GPA, as this has potential credit hours data that traditional grades is missing 
  # note that weighted GPA gets called cumulative GPA in the data, but it's really just weighted GPA and we write this in the dahsboard output that user sees 


# USING TRADITIONAL GRADES TO START 
table(trad_grades$`REQUIRED.04-mp_code`)
names(trad_grades)
gr2<-trad_grades[,c("REQUIRED.01-student_id", "REQUIRED.08-school_year", "COURSES.school_id", "COURSES.grade_level",
                    "REQUIRED.03-course_name", "REQUIRED.04-mp_code", "REQUIRED.07-grade", "COURSES.percent")]
# Merge in dcid
names(studentid_conversion)
gr2<-merge(gr2, studentid_conversion[,c(1,3)], by.x="REQUIRED.01-student_id", by.y="REQUIRED.03-student_id", all.x=TRUE)
gr2 <- gr2 %>%
  select("REQUIRED.05-dcid", everything())
gr2<-gr2[order(gr2$`REQUIRED.01-student_id`, gr2$`REQUIRED.08-school_year`, gr2$COURSES.school_id,
               gr2$COURSES.grade_level, gr2$`REQUIRED.03-course_name`, gr2$`REQUIRED.04-mp_code`),]
#check_trad<-gr2[which(gr2$`REQUIRED.05-dcid`=="2054"),] # gives you a manageable view of course grade data for one person 

# Reduce gr (grades) data to just variables needed so you can more easily work with it 
names(gr2) # trad grades 
#stored_grades$year<-sub("-.*", "", stored_grades$STOREDGRADES.DateStored) # create year variable to match traditional grades
#dim(gr2)

# Merge school information into grade data 
gr2<-merge(gr2, sch, by.x="COURSES.school_id", by.y="SCHOOLS.School_Number", all.x=TRUE)
dim(gr2)
names(gr2) # trad grades 

# ------------ Grade Calculations Using Traditional Grades ---------------
# Subset to just S1, S2, Y1
# Y1 is end of year and is an average of S1 and S2; Y2 is rare and is for multi-year course sequence like an AP language course
# Without potential credits earned you can only calculate unweighted GPA 

# Create a full traditional grade set for report card reporting capturing all time points of grades
gr2_full<-gr2 
gr2_full<-gr2_full[gr2_full$`REQUIRED.04-mp_code`!="E1" & gr2_full$`REQUIRED.04-mp_code`!="E2" &
                     gr2_full$`REQUIRED.04-mp_code`!="R1",]
table(gr2_full$`REQUIRED.04-mp_code`)
table(gr2_full$`REQUIRED.04-mp_code`)
gr2_full<-gr2_full[order(gr2_full$`REQUIRED.05-dcid`, gr2_full$COURSES.school_id,
                         gr2_full$COURSES.grade_level, gr2_full$`REQUIRED.03-course_name`, gr2_full$`REQUIRED.04-mp_code`),]
# Subset to just S1, S2, and Y1 to create reduced traditional grades data for GPA and Grade calculations
gr2<-gr2[gr2$`REQUIRED.04-mp_code`=="S1" | gr2$`REQUIRED.04-mp_code`=="S2" | gr2$`REQUIRED.04-mp_code`=="Y1",]; table(gr2$`REQUIRED.04-mp_code`)
table(gr2$`REQUIRED.03-course_name`)
check<-gr2[gr2$`REQUIRED.05-dcid`=="2054",]
df_summary <- gr2 %>%
  group_by(`REQUIRED.05-dcid`) %>%
  summarise(Unique_Value_Count = n_distinct(COURSES.grade_level))
# IMPORTANT
any(df_summary$Unique_Value_Count==2 & is.na(df_summary$`REQUIRED.05-dcid`)==FALSE) # no multiple grades with ID

# Clean up and save gr2, which is cleaned up traditional grades data, for later use in data dashboard
course_explorer<-gr2
course_explorer<-course_explorer[,c("REQUIRED.05-dcid",  "SCHOOLS.Name", "COURSES.grade_level",
                                    "REQUIRED.04-mp_code", "REQUIRED.03-course_name",  "REQUIRED.07-grade", "COURSES.percent")]
names(course_explorer)<-c("DCID", "School", "Grade", "Marking Period", "Course Name", "Letter Grade", "Grade (0-100)")
course_explorer$Evaluation<-ifelse(course_explorer$`Grade (0-100)` >= 90, "Excellent Performance", 
                                   ifelse(course_explorer$`Grade (0-100)` < 90 & course_explorer$`Grade (0-100)` >= 80, "Good",
                                          ifelse(course_explorer$`Grade (0-100)` < 80 & course_explorer$`Grade (0-100)` >= 70, "Satisfactory",
                                                 ifelse(course_explorer$`Grade (0-100)` < 70 & course_explorer$`Grade (0-100)` >= 60, "Needs Monitoring", 
                                                        ifelse(course_explorer$`Grade (0-100)` < 60, "FAILING", NA)))))

# Split traditional grades into two segments: grades 0-6 and grades 7-12
str(gr2$COURSES.grade_level)
gr2_lower<-gr2[gr2$COURSES.grade_level >=0 & gr2$COURSES.grade_level <=6,]; table(gr2_lower$COURSES.grade_level)
gr2_higher<-gr2[gr2$COURSES.grade_level >=7 & gr2$COURSES.grade_level <=12,]; table(gr2_higher$COURSES.grade_level)
# Eliminate non-grade information
table(gr2_higher$`REQUIRED.07-grade`)
gr2_higher<-gr2_higher[gr2_higher$`REQUIRED.07-grade`!="P" & gr2_higher$`REQUIRED.07-grade`!="I" & gr2_higher$`REQUIRED.07-grade`!="INC",]
table(gr2_higher$`REQUIRED.07-grade`)
names(gr2_lower)
names(gr2_higher)
var_names<-c("school_id", "dcid", "student_id", "school_year", "grade_level",
             "course_name", "mp_code", "letter_grade", "numerical_grade", "school_name")
names(gr2_higher)<-var_names; names(gr2_lower)<-var_names
names(gr2_higher)
gr2_higher$Course<-paste0(gr2_higher$course_name, "_", gr2_higher$mp_code)
gr2_lower$Course<-paste0(gr2_lower$course_name, "_", gr2_lower$mp_code)

# ---- Within each student get proportion A's, B's, etc. within S1 and S2, and unweighted GPA ----
# NOTE: This is across S1 and S2

# Eliminate Y1 for higher grades
gr2_higher<-gr2_higher[gr2_higher$mp_code!="Y1",]
# Keep only Y1 for lower grades
gr2_lower<-gr2_lower[gr2_lower$mp_code=="Y1",]
table(gr2_lower$letter_grade) # note that in the lower grades, some 6th graders will have letter grades, below we drop these 
gr2_lower<-gr2_lower[gr2_lower$letter_grade=="1" | gr2_lower$letter_grade=="2" |
                       gr2_lower$letter_grade=="3" | gr2_lower$letter_grade=="4",]

# ---- IMPORTANT: Calculate Letter Grade Proportions and GPA ----

# Higher grades 7 - 12
table(gr2_higher$grade_level)
# Remove any non-letter grades from higher grade data 
table(gr2_higher$letter_grade)
gr2_higher<-gr2_higher[which(gr2_higher$letter_grade!="1" & gr2_higher$letter_grade!="2" 
                             & gr2_higher$letter_grade!="3" & gr2_higher$letter_grade!="4"),]; table(gr2_higher$letter_grade)
higher_grade_props <- gr2_higher %>%
  group_by(dcid, letter_grade) %>%
  mutate(letter_grade = gsub("[+-]", "", letter_grade)) %>% # get rid of + and - as we just want counts of all letters together
  summarise(Count = n(), .groups = "drop") %>%
  group_by(dcid) %>%
  mutate(Proportion = Count / sum(Count)) %>% mutate(Proportion = round(Proportion, 2))
higher_wide_prop <- higher_grade_props %>%
  select(dcid, letter_grade, Proportion) %>%
  pivot_wider(
    names_from = letter_grade,
    values_from = Proportion,
    names_prefix = "Prop_"
  ) %>%
  mutate(across(everything(), ~replace_na(., 0)))

# Lower grades K - 6
table(gr2_lower$grade_level)
lower_grade_props <- gr2_lower %>%
  group_by(dcid, letter_grade) %>%
  mutate(letter_grade = gsub("[+-]", "", letter_grade)) %>% # get rid of + and - as we just want counts of all letters together
  summarise(Count = n(), .groups = "drop") %>%
  group_by(dcid) %>%
  mutate(Proportion = Count / sum(Count)) %>% mutate(Proportion = round(Proportion, 2))
lower_wide_prop <- lower_grade_props %>%
  select(dcid, letter_grade, Proportion) %>%
  pivot_wider(
    names_from = letter_grade,
    values_from = Proportion,
    names_prefix = "Prop_"
  ) %>%
  mutate(across(everything(), ~replace_na(., 0)))

# Clean up variable names 
higher_wide_prop<-higher_wide_prop[,c("dcid", "Prop_A", "Prop_B", "Prop_C", "Prop_D", "Prop_F")]
lower_wide_prop<-lower_wide_prop[,c("dcid", "Prop_4", "Prop_3", "Prop_2", "Prop_1")]

# Grades Definitions for Grades 1-4 are as follows: 
# 4 - Exceeds; 3 - Meets Expectations; 2 - Progressing Towards Expectations; 1 - Below expectations 

# Calculate GPA
gpa_scale <- c("A" = 4.0, "B" = 3.0, "C" = 2.0, "D" = 1.0, "F" = 0.0)
higher_gpa <- gr2_higher %>%
  filter(mp_code %in% c("S1", "S2")) %>%
  mutate(
    letter_grade = gsub("[+-]", "", letter_grade),  # strip +/- if needed
    GPA_Points = gpa_scale[letter_grade]
  ) %>%
  group_by(dcid) %>%
  summarise(GPA = mean(GPA_Points, na.rm = TRUE))  %>% mutate(GPA = round(GPA, 2)) # this is unweighted
lower_gpa <- gr2_lower %>%
  filter(mp_code %in% c("Y1")) %>%
  mutate(
    GPA_Points = as.numeric(letter_grade),  # turn numeric
  ) %>%
  group_by(dcid) %>%
  summarise(GPA = mean(GPA_Points, na.rm = TRUE))  %>% mutate(GPA = round(GPA, 2)) # this is unweighted
# Merge grade breakdowns with GPA's 
higher_grades<-merge(higher_wide_prop, higher_gpa, by="dcid")
lower_grades<-merge(lower_wide_prop, lower_gpa, by="dcid")
grades<-bind_rows(higher_grades, lower_grades)
grades$Performance<-ifelse(grades$GPA >= 3.5, "Excellent Academic Performance",
                           ifelse(grades$GPA >=3.0 & grades$GPA < 3.5, "Above Average",
                                  ifelse(grades$GPA >=2.0 & grades$GPA < 3.0, "Average Performance", 
                                         ifelse(grades$GPA >=1.0 & grades$GPA < 2.0, "Low Performance", "Crticial/Failing"))))
table(grades$Performance)

# ---- Calculate Weighted GPA for grades 9-12 (High School)

# Need stored grades to calculate this 
# Calculated this only within grades 9-12 excluding Exam terms 

# Formula for this is: 
# Cumulative GPA Formula:round( (gpa_sum( gpa_gpapoints()*gpa_potentialcredit() ) / sum(gpa_potentialcredit() )),4)
# Explained in words: SUM GPA Points*Potential Credits and divide this by the SUM of Potential Credits (rounded to 4 digits of precision)


# Create cleaner student grade data from stored grades, selecting vars you need
#names(stored_grades[,c(1,38,35,22,37,21,29,6,12,20,10,30,9,43)])
grade_vars_to_extract<-c("STOREDGRADES.dcid", "STOREDGRADES.StudentID", "STOREDGRADES.SchoolName", "STOREDGRADES.Grade_Level",     
                         "STOREDGRADES.StoreCode",  "STOREDGRADES.Grade",  "STOREDGRADES.Percent", "STOREDGRADES.Course_Name",    
                         "STOREDGRADES.ExcludeFromGPA", "STOREDGRADES.GPA_Points", "STOREDGRADES.EarnedCrHrs", "STOREDGRADES.PotentialCrHrs",  
                         "STOREDGRADES.DateStored", "STOREDGRADES.TRANSACTION_DATE")
gr<-stored_grades[,grade_vars_to_extract]; gr<-gr[order(gr$STOREDGRADES.StudentID, gr$STOREDGRADES.Grade_Level),]

# Count of unique students - early in the year it won't be many, as year progresses you will see more
length(unique(gr$STOREDGRADES.StudentID)) 

# Run a check to see if there is more than one grade within student ID 
# Count unique values per ID
df_summary <- gr %>%
  group_by(STOREDGRADES.StudentID) %>%
  summarise(Unique_Value_Count = n_distinct(STOREDGRADES.Grade_Level))
#print(df_summary)
table(df_summary$Unique_Value_Count)

# Create proportion of grades by individual from stored grades 
gr<-gr[order(gr$STOREDGRADES.StudentID, gr$STOREDGRADES.Grade_Level, 
             gr$STOREDGRADES.Course_Name, gr$STOREDGRADES.StoreCode),]
# Restrict to just grades that count for GPA
gr<-gr[which(gr$STOREDGRADES.ExcludeFromGPA==0),]
#check_stored<-gr[which(gr$STOREDGRADES.StudentID=="2054"),]
table(stored_grades$STOREDGRADES.StoreCode)
table(gr$STOREDGRADES.StoreCode)

# Eliminate E1 codes
gr<-gr[which(gr$STOREDGRADES.StoreCode!="E1"),]
table(gr$STOREDGRADES.StoreCode)
# Calculate weighted GPA for each student 
gr<-gr[order(gr$STOREDGRADES.StudentID, gr$STOREDGRADES.SchoolName, # uses Student ID as identifier column 
             gr$STOREDGRADES.Grade_Level, gr$STOREDGRADES.StoreCode),]
gr_cum_gpa<-gr[which(gr$STOREDGRADES.Grade_Level >= 9),]; table(gr_cum_gpa$STOREDGRADES.Grade_Level)
table(gr_cum_gpa$STOREDGRADES.PotentialCrHrs)
gr_cum_gpa <- gr_cum_gpa %>%
  group_by(STOREDGRADES.StudentID) %>%
  summarise(Cumulative_GPA = sum(STOREDGRADES.GPA_Points*STOREDGRADES.PotentialCrHrs) / sum(STOREDGRADES.PotentialCrHrs) )%>% 
  mutate(Cumulative_GPA = round(Cumulative_GPA, 2)) # this is unweighted
names(gr_cum_gpa)
table(gr_cum_gpa$Cumulative_GPA)
# Merge weighted GPA into grades data 
grades<-merge(grades, gr_cum_gpa, by.x="dcid", by.y="STOREDGRADES.StudentID", all.x=TRUE)
names(gr)
grade_levels<-gr[,c(2,4)]
max_grades_per_id <- grade_levels %>%
  group_by(STOREDGRADES.StudentID) %>%
  summarise(max_grade = max(STOREDGRADES.Grade_Level, na.rm = TRUE)) 
grades<-merge(grades, max_grades_per_id, by.x="dcid", by.y="STOREDGRADES.StudentID", all.x=TRUE)
table(is.na(grades$Cumulative_GPA), grades$max_grade) # TRUE is missing, lower grades have it because dcid spans multiple grades 
names(grades)
names(grades)<-c(names(grades)[-length(names(grades))], "Grade_Level"); names(grades)
# Remove cumulative from grades for which they shouldn't be there 
#grades$Cumulative_GPA<-ifelse(grades$Grade_Level<9, NA, grades$Cumulative_GPA)
table(is.na(grades$Cumulative_GPA), grades$Grade_Level) 


#####################
# DISCIPLINARY DATA 
#####################

# Requires BPS_DISCIPLINE_041125.txt
names(discipline)

# Merge in dcid information 
names(studentid_conversion)
discipline<-merge(discipline, studentid_conversion[,c(1,3)], by.x="REQUIRED.01-student_id", 
                  by.y="REQUIRED.03-student_id", all.x=TRUE)
discipline <- discipline %>%
  select("REQUIRED.05-dcid", everything())
table(discipline$INCIDENT.disc_type)
table(discipline$INCIDENT.sub_behavior)
names(discipline)
discipline<-discipline[,c("REQUIRED.05-dcid", "INCIDENT.behavior", "INCIDENT.disc_type",
                          "INCIDENT.sub_behavior", "REQUIRED.02-disc_date")]
anyDuplicated(discipline$`REQUIRED.05-dcid`)
discipline<-discipline[order(discipline$`REQUIRED.05-dcid`),]
names(discipline)<-c("dcid", names(discipline[,c(2:dim(discipline)[2])]))

# Reshape before creating disciplinary data 
anyNA(discipline$dcid)
discipline<-discipline[is.na(discipline$dcid)==FALSE,]

# Merge out non-tribal students
#discipline<-merge(discipline, tribal_student_list[,c(2:3)], by.x="dcid", by.y="STUDENTS.dcid", all.x=TRUE)
#discipline<-discipline[discipline$tribal_student==1,]
#discipline$tribal_student<-NULL

# Incident Behaviors 
beh_wide <- discipline %>%
  group_by(dcid) %>%
  mutate(pos = row_number()) %>%
  ungroup()
names(beh_wide)
beh_wide<-beh_wide[,c("dcid", "INCIDENT.behavior", "pos")]
beh_wide <- beh_wide %>%
  pivot_wider(
    names_from = pos,
    values_from = INCIDENT.behavior,
    names_prefix = "Behavior_"
  )
beh_wide$Number_behaviors<-rowSums(!is.na(beh_wide[ ,-1]))

# Discipline Types
disc_wide <- discipline %>%
  group_by(dcid) %>%
  mutate(pos = row_number()) %>%
  ungroup()
names(disc_wide)
disc_wide<-disc_wide[,c("dcid", "INCIDENT.disc_type", "pos")]
disc_wide <- disc_wide %>%
  pivot_wider(
    names_from = pos,
    values_from = INCIDENT.disc_type,
    names_prefix = "Discipline_"
  )
disc_wide$Number_disciplines<-rowSums(!is.na(disc_wide[ ,-1]))

# Sub Behaviors
sub_wide <- discipline %>%
  group_by(dcid) %>%
  mutate(pos = row_number()) %>%
  ungroup()
names(sub_wide)
sub_wide<-sub_wide[,c("dcid", "INCIDENT.sub_behavior", "pos")]
sub_wide <- sub_wide %>%
  pivot_wider(
    names_from = pos,
    values_from = INCIDENT.sub_behavior,
    names_prefix = "Sub_Behavior_"
  )
sub_wide$Number_sub_behaviors<-rowSums(!is.na(sub_wide[ ,-1]))

# Create dates for each incident to help with tracking 
date_wide <- discipline %>%
  group_by(dcid) %>%
  mutate(pos = row_number()) %>%
  ungroup()
names(date_wide)
date_wide<-date_wide[,c("dcid", "REQUIRED.02-disc_date", "pos")]
date_wide <- date_wide %>%
  pivot_wider(
    names_from = pos,
    values_from = `REQUIRED.02-disc_date`,
    names_prefix = "Incident_Date_"
  )


#####################
# AGGREGATE PROFILES
#####################

# Merge student data with attendance, disciplinary, and gpa data 
names(students_dat) # this is tribal student data 
names(disc_wide) # disciplinary data 
names(grades) # academic performance data 
names(tracker) # attendance data 

# Merge in needed variables for reporting at district and tribal levels 
names(students_dat)
names(disc_wide)
students_dat2<-merge(students_dat, disc_wide, by.x="STUDENTS.dcid", by.y="dcid", all.x=TRUE)
names(grades)

########### FIX HERE 

students_dat2<-merge(students_dat2, grades, by.x="STUDENTS.dcid", by.y="dcid", all.x=TRUE)
names(tracker)
tracker<-tracker[,-c(22:24)] # remove SCHOOLS.Name, STUDENTS.Grade_Level, and tribe_name
students_dat2<-merge(students_dat2, tracker, by.x="STUDENTS.dcid", by.y="REQUIRED.05-dcid", all.x=TRUE)
students_dat2<-students_dat2[!is.na(students_dat2$STUDENTS.dcid),]
students_dat2$Number_disciplines<-ifelse(is.na(students_dat2$Number_disciplines), 0, students_dat2$Number_disciplines)
students_dat2$Grade_level<-ifelse(students_dat2$STUDENTS.Grade_Level==-2, "PreK 1",
                                  ifelse(students_dat2$STUDENTS.Grade_Level==-1, "PreK 2",
                                         ifelse(students_dat2$STUDENTS.Grade_Level==0, "Kindergarten", students_dat2$STUDENTS.Grade_Level)))
disciplinary<-df_selected <- students_dat2 %>%
  select(starts_with("Discipline_")); disciplinary<-names(disciplinary)

# School Map 
school_map <- data.frame(aggregate(students_dat2$STUDENTS.SchoolID~students_dat2$SCHOOLS.Name, FUN=unique))
names(school_map)<-c("Name", "Code")

# Student Lookup
lookup<-students_dat2[,c("STUDENTS.dcid", "STUDENTS.Last_Name", "STUDENTS.First_Name", 
                         "tribe_name", "SCHOOLS.Name",  "STUDENTS.Grade_Level", "GPA", "Cumulative_GPA","proportion_absent", "Number_disciplines")]
names(lookup)<-c("dcid", "last_name", "first_name", "tribe_name", "school_name", "grade_level", "GPA", "cumulative_GPA", "proportion_absent", "number_disciplines")
lookup$grade_level2<-ifelse(lookup$grade_level==-2, "Prek-1",
                            ifelse(lookup$grade_level==-1, "Prek-2",
                                   ifelse(lookup$grade_level==0, "Kindergarten", lookup$grade_level)))


# Based on stored 
#courses_taken<-gr[,c("STOREDGRADES.StudentID", "STOREDGRADES.Course_Name", "STOREDGRADES.StoreCode", "STOREDGRADES.Grade", "STOREDGRADES.GPA_Points")]
#names(courses_taken)<-c("ID", "Course_Name", "Marking Period", "Grade", "GPA Points") # uses STUDENTS.ID
# Based on traditional
courses_taken<-gr2_full[,c("REQUIRED.05-dcid", "REQUIRED.03-course_name", "REQUIRED.04-mp_code", "REQUIRED.07-grade", "COURSES.percent")]
recode_gpa <- function(score) {
  case_when(
    score >= 90 ~ 4.0,
    score >= 80 ~ 3.0,
    score >= 70 ~ 2.0,
    score >= 60 ~ 1.0,
    TRUE        ~ 0.0
  )
}
courses_taken <- courses_taken %>%
  mutate(COURSES.percent = recode_gpa(COURSES.percent))
names(courses_taken)<-c("ID", "Course Name", "Marking Period", "Grade", "GPA Points") # uses STUDENTS.ID
# Bump AP coursework by +1 - make sure you have list of all AP courses 
#table(grepl("\\bAP\\b", courses_taken$Course_Name, ignore.case = FALSE))
courses_taken$`GPA Points`<-ifelse(grepl("\\bAP\\b", courses_taken$`Course Name`, ignore.case = FALSE), 
                                   courses_taken$`GPA Points`+1, courses_taken$`GPA Points`)
courses_taken$`GPA Points`<-ifelse(courses_taken$Grade=="P", 0, courses_taken$`GPA Points`)
ids<-students_dat2[,c(1,3)]; names(ids)<-c("dcid", "ID")
courses_taken<-merge(courses_taken, ids, by="ID", all.x=TRUE)
courses_taken<-courses_taken[order(courses_taken$ID, courses_taken$`Marking Period`),]

# Create tribal student indicator and split datasets
table(students_dat2$S_NM_STU_X.tribe)
students_dat2$tribal_student<-ifelse(students_dat2$S_NM_STU_X.tribe=="00",0,1); table(students_dat2$tribal_student)
students_dat2_all<-students_dat2

# Finalize course_explorer data, merging in first, last name, email, phone, and tribe from student data 
course_explorer<-merge(course_explorer, students_dat2[,c(1,17,25,23,24,48)], by.x="DCID", by.y="STUDENTS.dcid", all.x=TRUE)
# Rearrange data variables 
course_explorer<-course_explorer[,c(1,9,10,13,2:8,11:12)]
names(course_explorer)<-c("DCID", "First Name", "Last Name", "Tribe", "School", "Grade", "Marking Period", 
                          "Course Name", "Letter Grade", "Grade (0-100)", "Evaluation", "Guardian Email", "Home Phone")

##################
# Circle Bar Code 
##################

# ---- PREPARE DATA ----
# Select relevant columns
circle_vars_needed<-c("STUDENTS.dcid", "tribe_name", "STUDENTS.SchoolID", "proportion_absent", "total_periods" )
circle <- students_dat2[,circle_vars_needed]
colnames(circle) <- c("id", "tribe_name", "school_name", "proportion_absent", "something_else")

# Remove missing values
circle <- circle[!is.na(circle$proportion_absent), ]

# Aggregate to get average absence per tribe and school
result <- aggregate(proportion_absent ~ tribe_name + school_name, data = circle, FUN = mean)
colnames(result) <- c("individual", "group", "value1")

# Create second value to make it a stack
result$value1 <- round(result$value1, 2)
result$value2 <- round(1 - result$value1, 2)

# Pivot to long format
data <- result %>% 
  pivot_longer(cols = c(value1, value2), names_to = "observation", values_to = "value")

# Ensure group is a factor
data$group <- factor(data$group)

# ---- SPACING ----
empty_bar <- 2
nObsType <- nlevels(factor(data$observation))
to_add <- data.frame(matrix(NA, empty_bar * nlevels(data$group) * nObsType, ncol(data)))
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each = empty_bar * nObsType)
data <- rbind(data, to_add)

# Arrange and assign ID
data <- data %>% arrange(group, individual)
data$id <- rep(seq(1, nrow(data) / nObsType), each = nObsType)

# ---- LABELS ----
label_data <- data %>%
  group_by(id, individual) %>%
  summarize(tot = sum(value, na.rm = TRUE), .groups = "drop")

number_of_bar <- nrow(label_data)
label_data$angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(label_data$angle < -90, 1, 0)
label_data$angle <- ifelse(label_data$angle < -90, label_data$angle + 180, label_data$angle)

# Clean school name from label
label_data$school_clean <- sapply(strsplit(as.character(label_data$individual), " - "), tail, 1)
label_data$school_clean <- ifelse(label_data$school_clean=="Not Applicable", "Non-Native", label_data$school_clean)

# ---- BASELINES ----
base_data <- data %>%
  group_by(group) %>%
  summarize(start = min(id), end = max(id) - empty_bar, .groups = "drop") %>%
  rowwise() %>%
  mutate(title = mean(c(start, end)))
base_data<-merge(base_data, school_map, by.x="group", by.y="Code", all.x=TRUE)

# ---- GRID ----
grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:(nrow(grid_data)-1))] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1, ]

# ---- PLOT ----
# p <- ggplot(data) +      
#   geom_bar(aes(x = as.factor(id), y = value * 100, fill = observation), stat = "identity", alpha = 0.8) +
#   scale_fill_manual(
#     values = c("value1" = "red", "value2" = "orange"),
#     labels = c("value1" = "% Absent", "value2" = "% Present"),
#     name = "Attendance",
#     na.translate = FALSE
#   ) +
#   
#   # Grid lines
#   geom_segment(data = grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
#   geom_segment(data = grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
#   geom_segment(data = grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
#   
#   # Y-axis labels
#   annotate("text", x = max(data$id) + 1, y = c(0, 50, 100), label = c("0%", "50%", "100%"), color = "black", size = 5, hjust = 0, fontface = "bold") +
#   
#   # Axis and layout
#   ylim(-30, 135) +
#   theme_void() +
#   theme(
#     legend.position = "inside",
#     legend.position.inside = c(0.05, 0.9),             # Top-left inside
#     legend.justification = c(0, 1),                # Anchor to top-left
#     legend.background = element_rect(fill = "white", color = NA),
#     legend.title = element_text(face = "bold", size = 14),  # Larger, bold title
#     legend.text = element_text(face = "bold", size=12), 
#     plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
#     plot.margin = unit(rep(-1, 4), "cm")
#   ) +
#   coord_polar(start = 0) +
#   
#   # ---- SCHOOL LABELS (OUTER) ----
# geom_text(data = label_data, 
#           aes(x = id, y = 125, label = school_clean, angle = angle, hjust = hjust),
#           inherit.aes = FALSE,
#           color = "black", fontface = "bold", size = 3.5, alpha = 0.9) + 
#   
#   # ---- TRIBE LABELS (INNER) ----
# geom_text(data = base_data,
#           aes(x = title, y = 112, label = paste0("Sch ",group)),
#           inherit.aes = FALSE,
#           angle = -0.5,
#           hjust = 0.5,
#           color = "red",
#           fontface = "bold",
#           size = 4.2)


# ---- PRINT ----
#print(p)

# Add students contacts table from contacts object imported in from HHS Contacts.txt file 
student_contacts <- contacts %>%
  dplyr::transmute(
    STUDENTS.dcid   = as.integer(STUDENT.dcid),
    Student_Name    = STUDENT.name,
    Contact_Name    = stringr::str_squish(
      paste(PERSON.first_name, PERSON.last_name)
    ),
    Relationship    = CURRENT_RELATIONSHIP_TYPE.display_value,
    Email           = dplyr::coalesce(
      EMAIL.access_account_email,
      EMAIL.email_address
    ),
    Phone           = PHONE.phone_number,
    Lives_With      = as.logical(STUDENT_CONTACT_DETAIL.lives_with),
    Receives_Mail   = as.logical(STUDENT_CONTACT_DETAIL.receives_mail),
    School_Pickup   = as.logical(STUDENT_CONTACT_DETAIL.school_pickup),
    Emergency_Contact = as.logical(STUDENT_CONTACT_DETAIL.is_emergency),
    Custodial       = as.logical(STUDENT_CONTACT_DETAIL.is_custodial),
    Has_Data_Access = as.logical(STUDENT_CONTACT_DETAIL.has_data_access)
  ) %>%
  dplyr::filter(
    !is.na(STUDENTS.dcid),
    !is.na(Contact_Name),
    Contact_Name != ""
  )

# Clean up 
# .keep <- c(
#   "attendance","courses","discipline","schools","students","get_mode","students_dat",
#   "total_students","tribal_students","prop_no_release","check","grade_counts","grade_df",
#   "order_levels","grade_colors","colors","school_counts","school_df","tribe","excused_absence",
#   "unexcused_absence","context","dcid","district_truancy","grades","grade_levels","beh_wide",
#   "disc_wide","date_wide","students_dat2","disciplinary","school_map","lookup","courses_taken",
#   "circle","result","data","label_data","base_data","grid_data", "course_explorer", "student_contacts"
# )

# ----------------------------
# Package safe cache artifact
# ----------------------------

.keep <- c(
  "attendance","courses","discipline","schools","students","get_mode","students_dat",
  "total_students","tribal_students","prop_no_release","check","grade_counts","grade_df",
  "order_levels","grade_colors","colors","school_counts","school_df","tribe","excused_absence",
  "unexcused_absence","context","dcid","district_truancy","grades","grade_levels","beh_wide",
  "disc_wide","date_wide","students_dat2","disciplinary","school_map","lookup","courses_taken",
  "circle","result","data","label_data","base_data","grid_data","course_explorer","student_contacts",
  "OUTPUT_CACHE"
)

# IMPORTANT:
# Do NOT rm() from .GlobalEnv — this script is used by other runners (LaunchAgent, etc.)
# Only package the objects we want into a single qs.
cache <- mget(.keep, inherits = TRUE)

# Write the cache artifact
qs2::qsave(cache, OUTPUT_CACHE)
