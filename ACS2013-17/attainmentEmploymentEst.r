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
### translate from dplyr:do to matrix
###############



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




tab1 <- rbind(
  attainment1$NationalAverage[[1]][[1]],
  do.call('rbind',attainment1$overall$x))

tab1 <- cbind(race=c('National Average',attainment1$overall$blackORwhite),
  deaf=c('',as.character(attainment1$overall$deaf)),
  as.data.frame(round(tab1,1)))

tab1 <- tab1%>%
  select(-ends_with('SE',ignore.case=FALSE),-contains('No HS'))%>%
  arrange(match(race,c('National Average','Black','White')))%>%
  rename( HS="% >=HS Diploma","Some College"="% >=Some College",AA="% >=Associates",
    BA= "% >=Bachelors",MA="% >=Masters/Professional",PhD="% PhD")

tab2 <- as.data.frame(do.call('rbind',attainment1$byAge$x))
tab2 <- cbind(race=attainment1$byAge$blackORwhite,
  deaf=attainment1$byAge$deaf,
  age=attainment1$byAge$Age,
  tab2)
tab2 <- tab2%>%
  select(-ends_with('SE',ignore.case=FALSE),-contains('No HS'))%>%
  rename( HS="% >=HS Diploma","Some College"="% >=Some College",AA="% >=Associates",
      BA= "% >=Bachelors",MA="% >=Masters/Professional",PhD="% PhD")%>%
  arrange(race)%>%
  melt()%>%
  dcast(race+deaf+variable~age)%>% # so that age categories are columns
  select(Attainment=variable,race,deaf,everything())%>%
  arrange(Attainment,race,deaf)

lilTab1 <- function(ss){
  tab <- as.data.frame(do.call('rbind',ss$x))
  tab%>%select(-ends_with('SE',ignore.case=FALSE),-contains('No HS'))%>%
    rename( HS="% >=HS Diploma","Some College"="% >=Some College",AA="% >=Associates",
      BA= "% >=Bachelors",MA="% >=Masters/Professional",PhD="% PhD")
}

lilTab <- function(varb){
  nn <- paste0('by',toupper(substr(varb,1,1)),substr(varb,2,nchar(varb)))
  tab <- lilTab1(attainment1[[nn]])%>%
    mutate(race=attainment1[[nn]]$blackORwhite,
      deaf=attainment1[[nn]]$deaf,
      vvv=attainment1[[nn]][[varb]])%>%
    arrange(race,vvv)%>%
    select(race,vvv,deaf,everything())

  #colnames(tab)[colnames(tab)=='vvv'] <- varb

  tab
}

tab3 <- lilTab('Sex')

sraceTab <- function(nn)
  lilTab1(attainment1[[nn]])%>%
    mutate(race=nn,vvv=NA,deaf=attainment1[[nn]]$deaf)%>%
    select(race,vvv,deaf,everything())


tab4 <- rbind(lilTab('nativity'),lilTab('lanx'),
  lilTab1(attainment1$blackMulti)%>%
    mutate(race=attainment1$blackMulti$blackMulti, vvv=NA,deaf=attainment1$blackMulti$deaf)%>%
    arrange(race)%>%select(race,vvv,deaf,everything()),
  sraceTab('blackLatinx'),sraceTab('blackAsian'),sraceTab('blackWhite')
  )


disTab <- function(ss)
  ss%>%filter(deaf=='deaf')%>%
    lilTab1()%>%
    mutate(race=filter(ss,deaf=='deaf')$blackORwhite,diss=filter(ss,deaf=='deaf')[[3]])%>%
    select(diss,race,everything())%>%
    filter(diss%in%grep('No|seeing',ss[[3]],value=TRUE,invert=TRUE))


tab5 <- do.call('rbind',lapply(attainment1$byDiss,disTab))%>%
  mutate(diss=ifelse(diss%in%c('disabled','blind'),paste0('Deaf',diss),
    ifelse(diss=='nondisabled',paste('Deaf',diss),paste0('Deaf+',diss))))%>%
  arrange(match(diss,unique(diss)))

tab6 <- do.call('cbind',lapply(fod$x, function(y) y[-grep(' SE',names(y))]))
colnames(tab6) <- paste(fod$deaf,fod$blackORwhite)

openxlsx::write.xlsx(tab6,'fieldOfDegreePercent.xlsx')

overallEnr$blackORwhite <- 'overall'

enr <- rbind(overallEnr[,names(raceEnr)],raceEnr)

enr[['% enrolled']] <- sapply(enr$x,function(y) y[1])
enr[['% not enrolled']] <- 1-enr[['% enrolled']]
enr$n <- sapply(enr$x,function(y) y['n'])
enr$x <- NULL
openxlsx::write.xlsx(enr,'enrollment.xlsx')


#################################################################################################
#### Employment, LFP
#################################################################################################

dat <- filter(dat,agep>24)
gc()

emp1 <- dat%>%group_by(deaf,blackORwhite)%>%do(x=factorProps('employment',.))

emp11 <- do.call('rbind',lapply(emp1$x,function(y) y[-grep(' SE',names(y))]))
emp1$x <- NULL
emp1 <- cbind(emp1,emp11)

openxlsx::write.xlsx(emp1,'overallEmployment.xlsx')

ft <- dat%>%filter(employment=='Employed')%>%group_by(deaf,blackORwhite)%>%
  summarize(ft=svmean(.$fulltime,.$pwgtp),pt=1-ft,n=n())

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
        summarize(`% Employed`=svmean(.$employment=='Employed',.$pwgtp),
          `% FT`=svmean(.$fulltime,.$pwgtp)),
      dat%>%filter(fulltime,blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,!!sym(vv))%>%
        summarize(`Med. Earn (FT)`=med(~pernp,.,se=FALSE))
    )

empDis <- list()

empDis[['disabled']] <-
  full_join(
      dat%>%filter(blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,diss)%>%
        summarize(`% Employed`=svmean(.$employment=='Employed',.$pwgtp),
          `% FT`=svmean(.$fulltime,.$pwgtp)),
      dat%>%filter(fulltime,blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,diss)%>%
        summarize(`Med. Earn (FT)`=med(~pernp,.,se=FALSE))
    )


for(vv in c('ddrs','dout','dphy','drem','deye')){
  nm <- c(ddrs='selfCare',dout='indLiv',dphy='amb',drem='cogDif',deye='blind')[vv]
  empDis[[nm]] <-
    full_join(
      dat%>%filter(blackORwhite!='Other',deaf=='deaf',!!sym(vv)==1)%>%
        group_by(blackORwhite)%>%
        summarize(`% Employed`=svmean(.$employment=='Employed',.$pwgtp),
          `% FT`=svmean(.$fulltime,.$pwgtp)),
      dat%>%filter(fulltime,blackORwhite!='Other',deaf=='deaf',!!sym(vv)==1)%>%
        group_by(blackORwhite)%>%
        summarize(`Med. Earn (FT)`=med(~pernp,.,se=FALSE))
    )
}

library(openxlsx)
write.xlsx(emp,file='employmentSubgroups.xlsx')


save(emp1,emp,ft,empDis,file='employment.RData')

