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

print(xtabs(~raceEth+attainCum+Sex,data=dat))
print(xtabs(~raceEth+Sex+deaf,data=dat))
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

### simple tibble-of-lists to data.frame function:
ff <- function(tt){
  out <- cbind(select(tt,-x),do.call('rbind',tt$x))
  out[,-grep(' SE',colnames(out))]
}


#################################################################################################
#### Enrollment
#################################################################################################
stopifnot(min(dat$agep)==18)
dat18 <- dat
rm(dat); gc()

overallEnr <- dat18%>%group_by(deaf)%>%do(x=factorProps('enrolled',.,cum=FALSE))
raceEnr <- dat18%>%filter(blackORwhite!='Other')%>%group_by(deaf,blackORwhite)%>%
  do(x=factorProps('enrolled',.,cum=FALSE))
raceGenderEnr <- dat18%>%filter(blackORwhite!='Other')%>%group_by(deaf,blackORwhite,sex)%>%
  do(x=factorProps('enrolled',.,cum=FALSE))

enr <- bind_rows(overallEnr,raceEnr,raceGenderEnr)
## SEX         Character   1
## Sex
##             1    .Male
##             2    .Female
enr$sex <- c('Male','Female')[enr$sex]
write.xlsx(ff(enr),'enrollment.xlsx')

print('enrollment end')
#################################################################################################
#### Educational attainment
#################################################################################################
gc()
dat25 <- filter(dat18,agep>24)
rm(dat18); gc()

attainment1 <- dat25%>%stand1('attainCum',factorProps,.)
gc()
##### Field of degree
fod <- dat25%>%filter(blackORwhite!='Other',attainCum>'Associates')%>%group_by(deaf,blackORwhite)%>%
  do(x=factorProps('fodSmall',.,cum=FALSE))
gc()
#### current enrollment

gc()
save(attainment1,fod,overallEnr,raceEnr,raceGenderEnr,file='attainment.RData')
print('attainment end')
#################################################################################################
#### Subgroup percentages
#################################################################################################
gc()

perDeaf <- dat25%>%group_by(blackORwhite)%>%mutate(DEAF=deaf=='deaf')%>%
  summarize(percent.deaf=svmean(DEAF,pwgtp)*100,n=n())

perDeaf <- bind_rows(dat25%>%mutate(DEAF=deaf=='deaf')%>%
                       summarize(blackORwhite='Overall',percent.deaf=svmean(DEAF,pwgtp)*100,n=n()),
  perDeaf)

write.xlsx(perDeaf,file='percentDeafbyRace.xlsx')

subgroups <- c('Age','Sex','nativity','lanx','diss','blind','selfCare','indLiv','amb','cogDif')
subPer <- sapply(subgroups,
  function(ss) dat25%>%
                 group_by(deaf,blackORwhite)%>%
                 group_map(~as.data.frame(rbind(factorProps(ss,.x,cum=FALSE))))%>%
                 select(-ends_with('SE')),
  simplify=FALSE)

## subPer <- sapply(subPer,function(dd){
##   dd <- cbind(dd[,-which(names(dd)=='x')],do.call('rbind',dd$x))
##   dd[,-grep(' SE',names(dd),fixed=TRUE)]
## },
## simplify=FALSE)

## names(subPer) <- subgroups

for(gg in paste0('black',c('Latinx','Asian','ANDwhite'))){
  subPer[[gg]] <- dat25%>%filter(blackORwhite=='Black')%>%
    group_by(deaf)%>%summarize(blackORwhite=blackORwhite[1],x=svmean(!!sym(gg),pwgtp)*100,n=n())
  names(subPer[[gg]])[names(subPer[[gg]])=='x'] <- paste('%',gg)
}
subPer[['% blackMulti']] <- dat25%>%filter(blackORwhite=='Black')%>%
  group_by(deaf)%>%mutate(blackMulti=blackMulti=='BlackMulti')%>%
  summarize(blackORwhite=blackORwhite[1],x=svmean(blackMulti,pwgtp)*100,n=n())
names(subPer[['% blackMulti']])[names(subPer[['% blackMulti']])=='x'] <- '% blackMulti'

subPer[['% blackAlone']] <- dat25%>%filter(blackORwhite=='Black')%>%
  group_by(deaf)%>%mutate(blackAlone=blackMulti=='BlackAlone')%>%
  summarize(blackORwhite=blackORwhite[1],x=svmean(blackAlone,pwgtp)*100,n=n())
names(subPer[['% blackAlone']])[names(subPer[['% blackAlone']])=='x'] <- '% blackAlone'



subPer2 <- subPer[[1]]
for(i in 2:length(subPer)) subPer2 <- full_join(subPer2,subPer[[i]])

nnn <- subPer2$n
subPer2$n <- NULL
subPer2 <- cbind(subPer2,n=nnn)

subPer2 <- t(subPer2)
write.xlsx(subPer2,'subgroupPercentages.xlsx',row.names=TRUE,col.names=FALSE)

print('subgroup end')
#################################################################################################
#### Employment, LFP
#################################################################################################

gc()
emp1 <- dat25%>%group_by(deaf,blackORwhite)%>%do(x=factorProps('employment',.))
emp1 <- bind_cols(emp1%>%select(-x),as_tibble(do.call('rbind',emp1$x)))


ft <- dat25%>%filter(employment=='Employed')%>%group_by(deaf,blackORwhite)%>%
  summarize(ft=svmean(fulltime,pwgtp),pt=1-ft,n=n(),minAge=min(agep))

ft$ft <- ft$ft*100
ft$pt <- ft$pt*100

openxlsx::write.xlsx(ft,'fulltimePercentage.xlsx')

ern1 <- dat25%>%filter(fulltime)%>%
        group_by(deaf,blackORwhite)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),n=n(),minAge=min(agep))

write.xlsx(ern1,'FTearningsOverall.xlsx')

print('emp fulltime end')

### employment and fulltime and median wages
emp <- list()



for(vv in c('Age','Sex','nativity','lanx'))
  emp[[paste0('by',capitalize(vv))]] <-
    full_join(
      dat25%>%filter(blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,!!sym(vv))%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp),
          `% FT`=svmean(fulltime,pwgtp),n=n(),minAge=min(agep)),
      dat25%>%filter(fulltime,blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,!!sym(vv))%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))
    )


empRace <- dat25%>%filter(deaf=='deaf')%>%
  mutate(emp=employment=='Employed')%>%
  group_by(blackMulti)%>%
        summarize(`% Employed`=svmean(emp,pwgtp),
          `% FT`=svmean(fulltime,pwgtp),
          `Med. Earn (FT)`=med1(pernp[fulltime],pwgtp[fulltime],se=FALSE),
          n=n(),minAge=min(agep))

for(rr in paste0('black',c('Latinx','Asian','ANDwhite')))
  empRace <- bind_rows(empRace,
    dat25%>%filter(deaf=='deaf',!!sym(rr)==1)%>%
      mutate(emp=employment=='Employed')%>%
      summarize(`% Employed`=svmean(emp,pwgtp),
          `% FT`=svmean(fulltime,pwgtp),
          `Med. Earn (FT)`=med1(pernp[fulltime],pwgtp[fulltime],se=FALSE),
          n=n(),minAge=min(agep))%>%
      mutate(blackMulti=rr))


emp$byRace <- empRace



empDis <- list()

empDis[['disabled']] <-
  full_join(
      dat25%>%filter(blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,diss)%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp),
          `% FT`=svmean(fulltime,pwgtp),n=n(),minAge=min(agep)),
      dat25%>%filter(fulltime,blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,diss)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))
    )


for(vv in c('ddrs','dout','dphy','drem','deye')){
  nm <- c(ddrs='selfCare',dout='indLiv',dphy='amb',drem='cogDif',deye='blind')[vv]
  empDis[[nm]] <-
    full_join(
      dat25%>%filter(blackORwhite!='Other',deaf=='deaf',!!sym(vv)==1)%>%
        group_by(blackORwhite)%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp),
          `% FT`=svmean(fulltime,pwgtp),n=n(),minAge=min(agep)),
      dat25%>%filter(fulltime,blackORwhite!='Other',deaf=='deaf',!!sym(vv)==1)%>%
        group_by(blackORwhite)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))
    )
}


for(dd in names(empDis)[-1]){
  empDis[[dd]] <- cbind(deaf='deaf',empDis[[dd]]$blackORwhite,diss=dd,empDis[[dd]][,-1])
  names(empDis[[dd]]) <- names(empDis[[1]])
}
empDis[[1]] <- as.data.frame(empDis[[1]])

empDis <- do.call('rbind',empDis)

emp$byDisability <- empDis




### employment/earnings by ed level
empEd <- sapply(levels(dat25$attainCum)[-1],
  function(edLev)
    dat25%>%filter(blackORwhite!='Other',attainCum>=edLev)%>%
      group_by(deaf,blackORwhite)%>%
      mutate(emp=employment=='Employed')%>%
      summarize(`% Employed`=svmean(emp,pwgtp),
        `% FT`=svmean(fulltime,pwgtp),n=n(),minAge=min(agep)),
  simplify=FALSE)

empEd[['No HS']] <-
  dat25%>%filter(blackORwhite!='Other',attainCum=='No HS')%>%
   group_by(deaf,blackORwhite)%>%
   mutate(emp=employment=='Employed')%>%
   summarize(`% Employed`=svmean(emp,pwgtp),
    `% FT`=svmean(fulltime,pwgtp),n=n(),minAge=min(agep))

for(ee in names(empEd)) empEd[[ee]]$edLev <- ee

empEd <- do.call('rbind',empEd)

ernEd <- sapply(levels(dat25$attainCum)[-1],
  function(edLev)
    dat25%>%filter(fulltime, blackORwhite!='Other',attainCum>=edLev)%>%
      group_by(deaf,blackORwhite)%>%
      summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep)),
  simplify=FALSE)

ernEd[['No HS']] <-
   dat25%>%filter(fulltime, blackORwhite!='Other',attainCum=='No HS')%>%
      group_by(deaf,blackORwhite)%>%
      summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))

for(ee in names(ernEd)) ernEd[[ee]]$edLev <- ee

ernEd <- do.call('rbind',ernEd)

empEd <- full_join(empEd,ernEd)

empEd <- empEd%>%select(deaf,blackORwhite,edLev,everything())

empEd <- rbind(filter(empEd,edLev=='No HS'), filter(empEd,edLev!='No HS'))

emp$byEducation <- empEd


write.xlsx(emp,file='employmentSubgroups.xlsx')

### ssip
ssip <- dat25%>%group_by(deaf,blackORwhite)%>%
  mutate(ssip=ssip>0)%>%summarize(perSSIP=svmean(ssip,pwgtp)*100,n=n(),minAge=min(agep))

write.xlsx(ssip,'ssip.xlsx')


### business ownership self employment
bizOwn <- dat25%>%group_by(deaf,blackORwhite)%>%
  mutate(ssip=ssip>0)%>%
  summarize(
    perSelfEmp=svmean(selfEmp,pwgtp)*100,
    perOwnBiz=svmean(bizOwner,pwgtp)*100,
    n=n(),minAge=min(agep))

write.xlsx(bizOwn,'bizOwn.xlsx')

save(ern1,empEd,emp1,emp,ft,empRace,empDis,ssip,bizOwn,file='employment.RData')


################ top 5 occupations


#################################################################################################
#### top 5 occupations
#################################################################################################
jd <- dat25%>%filter(deaf=='deaf',fulltime,blackORwhite=='Black')%>%select(job,pwgtp)
jh <- dat25%>%filter(deaf=='hearing',fulltime,blackORwhite=='Black')%>%select(job,pwgtp)
jobD <- sapply(levels(jd$job),
  function(j) svmean(eval(parse(text=paste0("job=='",j,"'")),jd),jd$pwgtp))
jobH <- sapply(levels(jh$job),
  function(j) svmean(eval(parse(text=paste0('job=="',j,'"')),jh),jh$pwgtp))

top5D <- sort(jobD*100,decreasing=TRUE)[1:5]
top5H <- sort(jobH*100,decreasing=TRUE)[1:5]

sink('top5jobs.txt')
cat('DEAF\n')
cat(paste(paste0(names(top5D),' ',round(top5D,1),'%'),collapse='\n'))
cat('\n\n\n')
cat('HEARING \n')
cat(paste(paste0(names(top5H),' ',round(top5H,1),'%'),collapse='\n'))
sink()



###############
### by industry category
###############
employmentByIndustry <- dat25%>%filter(fulltime,blackORwhite=='Black')%>%
  group_by(deaf)%>%do(x=factorProps('industry',.))
## write results to spreadsheet

employmentByIndustry <- cbind(employmentByIndustry[,-which(names(employmentByIndustry)=='x')],
  do.call('rbind',employmentByIndustry$x))

ebi <- t(employmentByIndustry[,-grep(' SE',names(employmentByIndustry),fixed=TRUE)])

openxlsx::write.xlsx(ebi,
  'BlackIndustryPercentagesFT2012-17.xlsx', rowNames=TRUE,colWidths='auto')

