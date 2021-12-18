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
                    'figo_staging','patient_id','6_cycle_complete_status',
                    'outcome_after_3_cycles','change_in_chemo','outcome_after_6_cycles',
                    'surgical_outcome','brca_status','performance_status',
                    'os_time','os_status','pfs_time','pfs_status', "maintenance_therapy", "type_of_chemo",'new_chemo_regimen')

clinical_df <- df[,..col_to_include]
# clinical_df %>% count(type_of_chemo)
clinical_df[,table(`6_cycle_complete_status`)]


# Remove Non-HGSOC cases and patients with no patient_id (they don't have any relevant clinical details)
# R695231,R046570, R627716, R679420: non-HGSOC
# R67740,R681384: died after 2 cycles of chemo with no surgery
# R663500: only patient with performance status of 4. Exclude him for sanity
# R662523: patient refused any further treatment after one cycle of chemo
# R662846: patient operable after 6 cycles but refused surgery
# R666674: patient operable after 6 cycles but refused surgery
# R652709: patient operable after 4 cycles but refused surgery  
# R670471: patient operable after 6 cycles but refused surgery

clinical_df <- clinical_df[!(patient_id%in%c('R695231','R046570','R627716','R679420',
                                             'R677420','R681384',
                                             'R663500',
                                             'R662523','R662846','R666674','R652709','R670471'))]


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

clinical_df[str_detect(change_in_chemo,'N/A|NA|no documentation'),change_in_chemo:=NA]


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

###=============================================###
# patient grouping
###=============================================###

# patient completed 3 cycles and have surgery
clinical_df[outcome_after_3_cycles=='yes',patient_group := '3_cycle_surgery']

# patient went on to 6 cycles and continued on carbo/taxol till 6 cycles and have surgery
clinical_df[outcome_after_3_cycles=='no' & outcome_after_6_cycles=='yes',patient_group := '6_cycle_surgery']

# patient went on to 6 cycles, switched chemo with no intention to operate 
clinical_df[outcome_after_3_cycles=='no'& outcome_after_6_cycles=='no',patient_group := '6_cycle_no_surgery']


clinical_df[is.na(patient_group)]


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


# Type of chemo used in first attempt

table(df$type_of_chemo)

clinical_df[,bevacizumab:=ifelse(str_detect(type_of_chemo, regex("bevaci|avastin",ignore_case=T)),1,0)]
clinical_df[,carbo_pac:= ifelse(str_detect(type_of_chemo, regex("(?i)carbo.*(pac.*|taxol.*)",ignore_case=T)),1,0)]
clinical_df[,carbo:=ifelse(str_detect(type_of_chemo, regex("(carbo.*(?! pac.*|taxol.*)(?! bevaci|avastin))",ignore_case = T)),1,0)]

clinical_df[bevacizumab==1,first_chemo_cleaned:='bevacizumab_based']
clinical_df[carbo_pac==1&bevacizumab==0,first_chemo_cleaned:='carbo_pac']
clinical_df[carbo==1&carbo_pac==0&bevacizumab==0,first_chemo_cleaned:='carbo']


# The second chemo tried

# clinical_df[,table(new_chemo_regimen)]
# # 
# test_df = clinical_df[,.(patient_id, type_of_chemo,new_chemo_regimen,change_in_chemo,outcome_after_3_cycles)]
# test_df = test_df[str_detect(new_chemo_regimen,'N/A|NA|no documentation',negate=T)]

clinical_df[patient_id=='R677420',new_chemo_cleaned:='carbo_pac']
# Same treatment with dose reduction
clinical_df[patient_id=='R675343',new_chemo_cleaned:=NA]
clinical_df[patient_id=='R677966',new_chemo_cleaned:='carbo_caelyx']
clinical_df[patient_id=='R678971',new_chemo_cleaned:='carbo_caelyx']
clinical_df[patient_id=='R691852',new_chemo_cleaned:='carbo_weekly_pac']
clinical_df[patient_id=='R692311',new_chemo_cleaned:='carbo_weekly_pac']
clinical_df[patient_id=='R693820',new_chemo_cleaned:=NA]

# Same treatment with dose reduction
clinical_df[patient_id=='R694700',new_chemo_cleaned:=NA]
# Stopped chemo
clinical_df[patient_id=='R695907',new_chemo_cleaned:=NA]
clinical_df[patient_id=='R696128',new_chemo_cleaned:='anti-hormone']
clinical_df[patient_id=='R697578',new_chemo_cleaned:='carbo']
clinical_df[patient_id=='R697764',new_chemo_cleaned:='anti-hormone']
clinical_df[patient_id=='R682544',new_chemo_cleaned:='bevacizumab_based']
clinical_df[patient_id=='R684651',new_chemo_cleaned:='weekly_pac']

# Same treatment with dose reduction
clinical_df[patient_id=='R688738',new_chemo_cleaned:=NA]
clinical_df[patient_id=='R689680',new_chemo_cleaned:='carbo']
clinical_df[patient_id=='R690237',new_chemo_cleaned:='bevacizumab_based']
clinical_df[patient_id=='R691053',new_chemo_cleaned:='carbo']
clinical_df[patient_id=='R661457',new_chemo_cleaned:='bevacizumab_based']

# Same treatment with dose reduction
clinical_df[patient_id=='R661725',new_chemo_cleaned:=NA]
clinical_df[patient_id=='R662425',new_chemo_cleaned:='caelyx']

# Same treatment with dose reduction
clinical_df[patient_id=='R662920',new_chemo_cleaned:=NA]
clinical_df[patient_id=='R663320',new_chemo_cleaned:='carbo']
clinical_df[patient_id=='R663663',new_chemo_cleaned:= 'carbo_weekly_pac']

# Count as no change as only switched on cycle 6
clinical_df[patient_id=='R663924',new_chemo_cleaned:= NA]
clinical_df[patient_id=='R663948',new_chemo_cleaned:= 'carbo_weekly_pac']
clinical_df[patient_id=='R667118',new_chemo_cleaned:= 'bevacizumab_based']

# Same treatment with dose reduction
clinical_df[patient_id=='R653099',new_chemo_cleaned:= NA]
clinical_df[patient_id=='R654622',new_chemo_cleaned:= 'weekly_pac']
clinical_df[patient_id=='R655205',new_chemo_cleaned:= 'carbo_caelyx']
# Same treatment with dose reduction
clinical_df[patient_id=='R656688',new_chemo_cleaned:= NA]
clinical_df[patient_id=='R657548',new_chemo_cleaned:= 'stopped']
clinical_df[patient_id=='R658406',new_chemo_cleaned:= 'carbo_weekly_pac']

# Same treatment with dose reduction
clinical_df[patient_id=='R617108',new_chemo_cleaned:= NA]
# Stopped chemo
clinical_df[patient_id=='R644555',new_chemo_cleaned:= NA]
clinical_df[patient_id=='R645941',new_chemo_cleaned:= 'bevacizumab_based']
clinical_df[patient_id=='R646605',new_chemo_cleaned:= 'carbo_weekly_pac']

# count as no change as bevacizumab only started from cycle 6
clinical_df[patient_id=='R647384',new_chemo_cleaned:= NA]
# Same treatment with dose reduction
clinical_df[patient_id=='R651189',new_chemo_cleaned:= NA]
clinical_df[patient_id=='R651919',new_chemo_cleaned:= 'carbo_pac']
clinical_df[patient_id=='R652422',new_chemo_cleaned:= 'bevacizumab_based']
clinical_df[patient_id=='R652428',new_chemo_cleaned:= 'carbo_weekly_pac']
clinical_df[patient_id=='R652619',new_chemo_cleaned:= 'carbo_weekly_pac']

clinical_df[str_detect(new_chemo_regimen,'N/A|NA|no documentation'),new_chemo_cleaned := NA]

clinical_df[!is.na(new_chemo_cleaned) & new_chemo_cleaned!=first_chemo_cleaned,change_in_chemo_new :='changed']
clinical_df[is.na(change_in_chemo_new)&!(is.na(first_chemo_cleaned)),change_in_chemo_new :='no change']
test_df <- clinical_df[,.(patient_id,new_chemo_cleaned,first_chemo_cleaned,change_in_chemo_new)]
# test_new_chemo <- clinical_df[,.(patient_id,new_chemo_regimen,new_chemo_cleaned)]

#===============================================================#
# Create multi cox clinical df ----
#===============================================================#

multi_cox_cols <- c('patient_id','os_time','os_status','pfs_time','pfs_status',
                    'age_at_diagnosis','brca_status','figo_staging','performance_status',
                    'surgical_outcome','patient_group','maintenance_therapy', 'change_in_chemo_new', 'first_chemo_cleaned','new_chemo_cleaned')

clinical_multi_df <- clinical_df[,..multi_cox_cols]


