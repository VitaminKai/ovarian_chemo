# --------------
# Author: Kaiwen Wang
# Date: 2021-07-25
# Purpose: 
# Requires:
# Output: 
# --------------
library(readxl)

source(here::here('code','initiation.R'))

df <- data.table(read_excel(here('input','clinical_df.xlsx')))



#===============================================================#
# Rename columns ----
#===============================================================#

# Prognostic markers at baseline

setnames(df,old='FIGO Staging (TNM Staging)',new='figo_staging')
df[,table(figo_staging)]

df[,'figo_staging']

setnames(df,old='Performance status at diagmosis( if known) 0,1, 2, 3, 4',new='performance_status')

df[,.(performance_status)]

setnames(df,old="Were they operable after 3 cycles? Y/N. If yes, go to outcome of surgery section",
         new='outcome_after_3_cycles')


setnames(df,old="Was there a change to there chemo regimen? Y/N",
         new='change_in_chemo')
df[,table(change_in_chemo)]
df[,str_replace(change_in_chemo,regex('yes',ignore.case=T),'yes')]

multi_cox_results <- coxph(surv(time=,event=)~change_in_chemo+figo_staging)

library(gtsummary)
gtsummary::

setnames(df,old="If inoperable after 3 cycles, did they complete 6 cycles? Y/N ",
         new='6_cycle_complete')

setnames(df,old="If yes. Which new chemo regimen ? Free text",
         new='new_chemo_regimen')
df[,table(new_chemo_regimen)]


setnames(df,old='Were they operable after 6 cycles? Y/N',new='outcome_after_6_cycles')

setnames(df,old='Is patient still alive or deceased?(Y/N)',new='os_status')

setnames(df,old='Progressive disease (Y/N)',new='pfs_status')

setnames(df,old='PFS (date of chemo start date to progresstion/recurrence) - days',new='pfs_time')

setnames(df,old='OS (Start of chemo to death) - days',new='os_time')

setnames(df,old='BRCA status',new='brca_status')

setnames(df,old='Optimally or suboptimally debulked?',new='surgical_outcome')
df[,]

col_to_include <- c('age')