library(tidyverse)
library(reshape2)
library(openxlsx)

needDat <- FALSE
if(!exists("dat")) needDat <- TRUE
if(exists("dat")) if(min(dat$agep!=18)){
  print(paste('min age',min(dat$agep)))
  needDat <- TRUE
}

if(!needDat) print('using dataset "dat" already in workspace')

if(needDat){
  if('attainmentEmploymentDataACS13-17.RData'%in%list.files()){
    print('loading attainmentEmploymentDataACS13-17.RData from folder')
    load('attainmentEmploymentDataACS13-17.RData')
  } else{
    print('running makeData.r')
    source('makeData.r')
  }
}

## print(xtabs(~raceEth+attainCum+Sex,data=dat))
## print(xtabs(~raceEth+Sex+deaf,data=dat))
print(xtabs(~raceEth+blackORwhite,data=dat))
print(xtabs(~st,data=dat))
print(range(dat$agep))
source('../generalCode/estimationFunctions.r')
source('../generalCode/median.r')

#################################################################################################
#### useful general functions
#################################################################################################

###############
### standard calculations+translation thereof
###############

lower1 <- function(x) paste0(tolower(substr(x,1,1)),substr(x,2,nchar(x)))
capsRight <- function(x,tab)
  if(x%in%tab) x else tab[tolower(tab)==tolower(x)]
capitalize <- function(x) paste0(toupper(substr(x,1,1)),substr(x,2,nchar(x)))
noby <- function(x,tab) capsRight(substr(x,3,nchar(x)),tab)



### simple tibble-of-lists to data.frame function:
ff <- function(tt){
  out <- cbind(select(tt,-x),do.call('rbind',tt$x))
  out[,-grep(' SE',colnames(out))]
}


#################################################################################################
#### Enrollment
#################################################################################################

source('enrollmentEst.r')
print('enrollment end')

#################################################################################################
#### Educational attainment
#################################################################################################
source('attainmentEst.r')
print('attainment end')

#################################################################################################
#### Subgroup percentages
#################################################################################################
source('subgroupEst.r')
print('subgroup end')

#################################################################################################
#### Employment, LFP
#################################################################################################
source('employmentEst.r')
print('employment end')

#################################################################################################
#### ssip, biz ownership, top 5 occupations, industry category
#################################################################################################

source('randomEst.r')

#################################################################################################
#### estimates over time 2008-2017
#################################################################################################
source('estOverTime.r')

print('all done!')

