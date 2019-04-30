library(tidyverse)
library(reshape2)

if(exists('dat')) print('using dataset "dat" already in workspace')

if(!exists("dat")){
  if('attainmentEmploymentDataACS13-17.RData'%in%list.files()){
    print('loading attainmentEmploymentDataACS13-17.RData from folder')
    load('attainmentEmploymentDataACS13-17.RData')
  } else{
    print('running makeData.r')
    source('makeData.r')
  }
}

print(xtabs(~raceEth+attainCum+Sex,data=dat))
print(xtabs(~raceEth+Sex+deaf,data=dat))
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

stand1 <- function(x,FUN,dat,...){
  fun1 <- function(grp,data){
    if(substr(grp,1,2)=='by') grp <- noby(grp,names(data))
    data%>%group_by(deaf,blackORwhite,!!sym(grp))%>%do(x=FUN(x,.))
  }
  fun2 <- function(.data)
    append(
      append(
        list(overall=.data%>%group_by(deaf,blackORwhite)%>%do(x=FUN(x,.))),
        sapply(paste0('by',c('Age','Sex','Nativity','Lanx')),fun1,data=.data,simplify=FALSE)),
      list(byDiss=lapply(c('diss','blind','selfCare','indLiv','amb','cogDif'),fun1,data=.data))
    )
  fun3 <- function(fil,data)
    data%>%filter(!!sym(fil))%>%group_by(deaf)%>%do(x=FUN(x,.))

  out <-
    append(
      append(
        list(
          NationalAverage=dat%>%do(x=FUN(x,.)),
          blackMulti=dat%>%filter(blackMulti!='NotBlack')%>%group_by(deaf,blackMulti)%>%do(x=FUN(x,.))
        ),
        dat%>%filter(blackORwhite!='Other')%>%fun2(.)),
      sapply(paste0('black',c('Latinx','Asian','ANDwhite')),fun3,data=dat,simplify=FALSE)
    )
  names(out) <- gsub('ANDw','W',names(out))

  out
}


### general info to append to spreadsheets
info <- data.frame(c('Dataset: ACS',
                     'Year: 2017',
                     'Ages: 25-64',
                     'Excludes Institutionalized People',
                     'Occupational Category for full-time employed people only',
                     'Field of Degree for people with Bachelors degrees or higher'),
                   stringsAsFactors=FALSE)

names(info) <- c('')



#################################################################################################
#### Educational attainment
#################################################################################################

attainment1 <- filter(dat,agep>24)%>%stand1('attainCum',factorProps,.)
gc()
##### Field of degree
fod <- dat%>%filter(agep>24,blackORwhite!='Other',attainCum>'Associates')%>%group_by(deaf,blackORwhite)%>%
  do(x=factorProps('fodSmall',.,cum=FALSE))
gc()
#### current enrollment
overallEnr <- dat%>%group_by(deaf)%>%do(x=factorProps('enrolled',.,cum=FALSE))
raceEnr <- dat%>%filter(blackORwhite!='Other')%>%group_by(deaf,blackORwhite)%>%
  do(x=factorProps('enrolled',.,cum=FALSE))
gc()
save(attainment1,fod,overallEnr,raceEnr,file='attainment.RData')


#################################################################################################
#### Employment, LFP
#################################################################################################

dat <- filter(dat,agep>24)
gc()

emp1 <- dat%>%group_by(deaf,blackORwhite)%>%do(x=factorProps('employment',.))



ft <- dat%>%filter(employment=='Employed')%>%group_by(deaf,blackORwhite)%>%
  summarize(ft=svmean(fulltime,pwgtp),pt=1-ft,n=n())

ft$ft <- ft$ft*100
ft$pt <- ft$pt*100

openxlsx::write.xlsx(ft,'fulltimePercentage.xlsx')
### employment and fulltime and median wages
emp <- list()



for(vv in c('Age','Sex','nativity','lanx'))
  emp[[paste0('by',capitalize(vv))]] <-
    full_join(
      dat%>%filter(blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,!!sym(vv))%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp),
          `% FT`=svmean(fulltime,pwgtp)),
      dat%>%filter(fulltime,blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,!!sym(vv))%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE))
    )

empDis <- list()

empDis[['disabled']] <-
  full_join(
      dat%>%filter(blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,diss)%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp),
          `% FT`=svmean(fulltime,pwgtp)),
      dat%>%filter(fulltime,blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,diss)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE))
    )


for(vv in c('ddrs','dout','dphy','drem','deye')){
  nm <- c(ddrs='selfCare',dout='indLiv',dphy='amb',drem='cogDif',deye='blind')[vv]
  empDis[[nm]] <-
    full_join(
      dat%>%filter(blackORwhite!='Other',deaf=='deaf',!!sym(vv)==1)%>%
        group_by(blackORwhite)%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp),
          `% FT`=svmean(fulltime,pwgtp)),
      dat%>%filter(fulltime,blackORwhite!='Other',deaf=='deaf',!!sym(vv)==1)%>%
        group_by(blackORwhite)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE))
    )
}

library(openxlsx)
write.xlsx(emp,file='employmentSubgroups.xlsx')


save(emp1,emp,ft,empDis,file='employment.RData')

