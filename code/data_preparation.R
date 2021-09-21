# --------------
# Author: Kaiwen Wang
# Date: 2021-07-25
# Purpose: 
# Requires:
# Output: 
# --------------
library(readxl)
library(tidyverse)
source(here::here('code','initiation.R'))
# disabled because regex overlapped with that supplied in stringr which supplies 
# more functionalities
# library(rex)
df <- data.table(read_excel(here('input','clinical_df.xlsx')))



#===============================================================#
# Rename columns ----
#===============================================================#

# Prognostic markers at baseline

setnames(df,old='Biopsy date (diagnosis)',new='date_of_diagnosis')

setnames(df,old='FIGO Staging (TNM Staging)',new='figo_staging')

setnames(df,old='Performance status at diagmosis( if known) 0,1, 2, 3, 4',new='performance_status')

setnames(df,old="Were they operable after 3 cycles? Y/N.\r\nIf yes, go to outcome of surgery section",
         new='outcome_after_3_cycles')

setnames(df,old="Was there a change to there chemo regimen? Y/N",
         new='change_in_chemo')

setnames(df,old='R number',new='patient_id')

setnames(df,old="If inoperable after 3 cycles, did they complete 6 cycles? Y/N",
         new='6_cycle_complete_status')

setnames(df,old="If yes. Which new chemo regimen ? Free text",
         new='new_chemo_regimen')

setnames(df,old='Were they operable after 6 cycles? Y/N',new='outcome_after_6_cycles')

setnames(df,old='Is patient still alive or deceased?(Y/N)',new='os_status')

setnames(df,old='Progressive disease (Y/N)',new='pfs_status')

setnames(df,old='PFS (date of chemo start date to progresstion/recurrence) - days',new='pfs_time')

setnames(df,old='OS (Start of chemo to death) - days',new='os_time')

setnames(df,old='BRCA status',new='brca_status')

setnames(df,old='Optimally or suboptimally debulked?',new='surgical_outcome')

setnames(df,old='Date of last follow up (bloods or imaging?)',new='date_last_follow_up')

setnames(df,old='Date neoadjuvant chemo commenced',new='date_neoadjuvant_chemo_start')

setnames(df,old='If deceased( Date of death)',new='date_of_death')
setnames(df,old='Maintenance therapy after surgery? (Y/N)',new='maintenance_therapy')

setnames(df, old = "What neodjuvant did they start with?", new = "type_of_chemo")

col_to_include <- c('DOB','date_of_diagnosis','date_last_follow_up','date_neoadjuvant_chemo_start','date_of_death',
                    'figo_staging','patient_id',
                    'outcome_after_3_cycles','change_in_chemo','outcome_after_6_cycles',
                    'surgical_outcome','brca_status','performance_status',
                    'os_time','os_status','pfs_time','pfs_status', "maintenance_therapy", "type_of_chemo")

clinical_df <- df[,..col_to_include]
# clinical_df %>% count(type_of_chemo)

# Remove Non-HGSOC cases and patients with no patient_id (they don't have any relevant clinical details)
clinical_df <- clinical_df[!(patient_id%in%c('R695231','R046570'))]

clinical_df[,`:=`(date_of_diagnosis=as.Date(date_of_diagnosis),
                  DOB=as.Date(DOB),
                  date_neoadjuvant_chemo_start=as.Date(as.numeric(date_neoadjuvant_chemo_start),origin='1900-01-01')
)]

clinical_df[,age_at_diagnosis:= as.double(date_of_diagnosis-DOB)/365.25]
clinical_df[,.(age_at_diagnosis)]

# Clean date of last follow-up
clinical_df[,date_last_follow_up]
clinical_df[,date_last_follow_up:=str_extract(date_last_follow_up,'[0-9]{2}/[0-9]{1,2}/[0-9]{2,4}|[0-9]{5}')]

clinical_df[,date_last_follow_up:=case_when(
  str_detect(date_last_follow_up,'[0-9]{2}/[0-9]{1,2}/[0-9]{2,4}') ~ as.Date(date_last_follow_up,'%d/%m/%y'),
  str_detect(date_last_follow_up,'[0-9]{5}') ~as.Date(as.numeric(date_last_follow_up),origin="1900-01-01"))]

clinical_df[,date_of_death]

# Clean survival status
clinical_df[,table(os_status)]

clinical_df[,os_status:=ifelse(str_detect(os_status,regex('deceased|dead',ignore_case=T)),1,0)]

clinical_df[,os_time:=case_when(
  os_status==0 ~ as.numeric(difftime(time1=date_last_follow_up,time2=date_neoadjuvant_chemo_start,units='days')),
  os_status==1 ~ as.numeric(os_time)
)]

clinical_df[,os_time:=os_time/365.25]

# progression free survival

clinical_df[,table(pfs_status)]

clinical_df[,pfs_status:=case_when(
  str_detect(pfs_status,regex('yes|y$',ignore_case = T))~ 1,
  str_detect(pfs_status,regex('no',ignore_case = T))~ 0,
)]


clinical_df[,pfs_time:=case_when(
  pfs_status==0 ~ as.numeric(difftime(time1=date_last_follow_up,
                                      time2=date_neoadjuvant_chemo_start,units='days')),
  pfs_status==1 ~ as.numeric(pfs_time)
)]

clinical_df[,pfs_time:=pfs_time/365.25]


### cleaning performance status
clinical_df[,table(performance_status)]
clinical_df[performance_status=='?',performance_status:= NA]

# clear figo staging
clinical_df[,table(figo_staging)]


#===============================================================#
# Clean up surgical outcome ----
#===============================================================#

clinical_df[,table(surgical_outcome)]

clinical_df[, surgical_outcome:=
              case_when(
                str_detect(surgical_outcome, regex("(?i)sub?optimal")) ~"suboptimal",
                str_detect(surgical_outcome, regex("(?i)optimal")) ~  "optimal",
                str_detect(surgical_outcome, regex("(?i)residual|milliary")) ~"suboptimal",
              )]

# 
# test_str <- c('Residual milliary disease','suboptimally debulked',
#               'residual disease <1cm (?optimally debulked)',
#               'optimal debulking with minimal defuse residual miliary disease (1-2 mm diameter)')
# 
# case_when(
#   str_detect(test_str, regex("(?i)sub?optimal")) ~"suboptimal",
#   str_detect(test_str, regex("(?i)optimal")) ~  "optimal",
#   str_detect(test_str, regex("(?i)milliary|residual")) ~"suboptimal",
# )

#===============================================================#
# Format outcome after 3 cycles ----
#===============================================================#
clinical_df[, table(outcome_after_3_cycles)]

# exclude patient who refused or not fit for or died before further treatment  
patient_to_exclude <- clinical_df[str_detect(outcome_after_3_cycles,regex('died|COVID|declined|refused|documentation|deteriorated|not fit|aborted|ileostomy',ignore_case = T)),patient_id]

clinical_df<- clinical_df[!(patient_id %in% patient_to_exclude)]

clinical_df[, outcome_after_3_cycles:=
              case_when(
                str_detect(outcome_after_3_cycles, regex("^(?i)no")) ~  "no",
                str_detect(outcome_after_3_cycles, regex("^(?i)yes")) ~"yes"
              )]
# start with case-insensitive no 

clinical_df<- clinical_df[!(is.na(outcome_after_3_cycles))]

# clean up brca status
clinical_df[,table(brca_status)]
clinical_df[str_detect(brca_status,'BRCA\\s?[0-9]',negate = T),brca_status:=NA]

# first standardising all that says normal
clinical_df[str_detect(brca_status, 'normal'),brca_status:='normal']

# second standardise BRCA 1 and BRCA 2
clinical_df[str_detect(brca_status, 'BRCA\\s?1'),brca_status:='BRCA 1 mutation']
clinical_df[str_detect(brca_status, 'BRCA\\s?2'),brca_status:='BRCA 2 mutation']

# clean up change in chemo
clinical_df[,table(change_in_chemo)]

# Patient who did not have 6 cycles of chemo, change in chemo should be NA

clinical_df[str_detect(change_in_chemo,regex('N/A|NA|no documentation',ignore_case=T)),change_in_chemo:=NA]
clinical_df[str_detect(change_in_chemo,regex('no|N$',ignore_case=T)),change_in_chemo:='no_change']
clinical_df[str_detect(change_in_chemo,regex('yes|Y$',ignore_case=T)),change_in_chemo:='changed']
clinical_df[change_in_chemo=='Added on bevacizumab in cycle 4 onwards',change_in_chemo:='changed']

# clean up outcome after 6 cycles
clinical_df[,table(outcome_after_6_cycles)]

patient_to_exclude <- clinical_df[str_detect(outcome_after_6_cycles,regex('did not go for surgery|declined|not to have surgery|8 cycles',ignore_case = T)),patient_id]

clinical_df<- clinical_df[!(patient_id %in% patient_to_exclude)]

clinical_df[, outcome_after_6_cycles:=
              case_when(
                str_detect(outcome_after_6_cycles, regex("^(?i)no")) ~  "no",
                str_detect(outcome_after_6_cycles, regex("^(?i)yes")) ~"yes"
              )]

# patient grouping
# patient completed 3 cycles and have surgery
clinical_df[outcome_after_3_cycles=='yes',patient_group := '3_cycle_surgery']

# patient went on to 6 cycles and continued on carbo/taxol till 6 cycles and have surgery
clinical_df[outcome_after_3_cycles=='no'&change_in_chemo=='no_change' & outcome_after_6_cycles=='yes',patient_group := '6_cycle_no_chemo_change_surgery']

# patient went on to 6 cycles and switched chemo and have surgery
clinical_df[outcome_after_3_cycles=='no'&change_in_chemo=='changed'&outcome_after_6_cycles=='yes',patient_group := '6_cycle_chemo_change_surgery']

# patient went on to 6 cycles, swithced chemo with no intention to operate 
clinical_df[outcome_after_3_cycles=='no'&change_in_chemo=='changed'&outcome_after_6_cycles=='no',patient_group := '6_cycle_no_surgery']



clinical_df <- clinical_df %>% 
  mutate(patient_group = 
           case_when( 
             patient_group %in% c("6_cycle_chemo_change_surgery", "6_cycle_no_chemo_change_surgery") ~ "6_cycle_surgery",
             patient_group == "3_cycle_surgery " ~ "3_cycle_surgery ",
             patient_group == "6_cycle_no_surgery" ~ NA_character_,
             TRUE ~ patient_group
           ))

clinical_df[,patient_group:=factor(patient_group)]

clinical_df %>% count(patient_group)

clinical_df[,table(patient_group)]

# figo staging
clinical_df[,table(figo_staging)]
clinical_df[!(str_detect(figo_staging, "3?\\/4")),figo_staging := str_extract(figo_staging, "\\d")]

clinical_df[str_detect(figo_staging, "3?\\/4"),figo_staging:= '4']

# Maintenance therapy
clinical_df[,table(maintenance_therapy)]

clinical_df <- clinical_df %>% 
  dplyr::mutate(
    maintenance_therapy =
      case_when(str_detect(maintenance_therapy, regex("(?i)yes") ) ~ "yes",
                str_detect(maintenance_therapy, regex("(?i)no|Nil|Two further cycles|offered radiotherapy")) ~ "no")
  )

clinical_df <- clinical_df %>% 
  mutate(
    type_of_chemo_cleaned = 
      case_when(
        type_of_chemo %in% c("NA", "N/A", "??") ~ NA_character_,
        str_detect(type_of_chemo, "(?i)carbo.*pac.*") ~ "1",
        str_detect(type_of_chemo, "(?i)single.*carbo.*") ~ "2",
        TRUE ~ "3" 
      )
    
    ### three groups you can find: 
    ### carboplatin + pac
    ### carboplatin alone 
    ### other combinations e.g. Carboplatin + paclitaxel, bevacizumab 
  )

#===============================================================#
# Create multi cox clinical df ----
#===============================================================#

multi_cox_cols <- c('patient_id','os_time','os_status','pfs_time','pfs_status',
                    'age_at_diagnosis','brca_status','figo_staging','performance_status',
                    'surgical_outcome','patient_group','maintenance_therapy', 'change_in_chemo', 'type_of_chemo_cleaned')

clinical_multi_df <- clinical_df[,..multi_cox_cols]


