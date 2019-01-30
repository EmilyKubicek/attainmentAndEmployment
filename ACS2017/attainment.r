library(dplyr)
library(broom)
library(ggplot2)

if(!exists("dat")){
    if('attainmentEmploymentDataACS17.RData'%in%list.files()){
        load('attainmentEmploymentDataACS17.RData')
    } else source('makeData.r')
}
print(xtabs(~Sex,data=dat))

source('../generalCode/estimationFunctions.r')
source('../generalCode/median.r')

#################################################################################################
#### useful general functions
#################################################################################################

###############
### translate from dplyr:do to matrix
###############

#### with significance tests
FIXsig1 <- function(tib1,med=FALSE){
  stopifnot(nrow(tib1)==2)
  #stopifnot(ncol(tib1)==1)
  lst <- sapply(tib1,is.list)
  stopifnot(sum(lst)==1)
  names(tib1)[lst] <- 'x'

  sig <- if(med) testsMed(tib1$x[[1]],tib1$x[[2]]) else tests(tib1$x[[1]],tib1$x[[2]])

  out <- as.data.frame(rbind(round(tib1$x[[1]],1),round(tib1$x[[2]],1),round(sig,2)))
  v1 <- as.character(tib1[['deaf']])
  out <- cbind(dh=c(v1,'p-val (deaf-hearing)'),out)
  out
 }


FIXsig <- function(tib,med=FALSE){
  stopifnot('deaf'%in%names(tib))
  lst <- sapply(tib,is.list)

  other <- setdiff(names(tib)[!lst],'deaf')

  if(length(other)==0)
    return(FIXsig1(tib,med=med))

  qqq <- distinct(tib[,other])
  out <- NULL
  for(j in 1:nrow(qqq)){
    rows <- which(sapply(1:nrow(tib), function(i) all(tib[i,other]==qqq[j,])))
    res <- FIXsig1(tib[rows,c('deaf',names(tib)[lst])],med=med)
    res <- cbind(qqq[j,],res)
    out <- rbind(out,res)
  }

  names(out)[1:sum(!lst)] <- ''
  out
}

ttest <- function(m1,m2,s1,s2){
  T <- abs(m1-m2)/sqrt(s1^2+s2^2)
  2*pnorm(-T)
}

testsMed <- function(mat1,mat2){
  if(all(c('est','se')%in%names(mat1))){
    names(mat2)[names(mat2)=='est'] <- names(mat1)[names(mat1)=='est'] <- 'median'
    names(mat2)[names(mat2)=='se'] <- names(mat1)[names(mat1)=='se'] <- 'SE'
  }

  c(ttest(mat1['median'],mat2['median'],mat1['SE'],mat2['SE']),NA,NA)
}

tests <- function(mat1,mat2){
  stopifnot(all(names(mat1)==names(mat2)))
  nnn <- names(mat1)
  nnn <- gsub('%','',nnn)
  nnn <- gsub(' ','',nnn)

  SEcols <- grep('SE',nnn)

  sig <- rep(NA,length(nnn))

  for(i in 1:length(SEcols)){
    seN <- nnn[SEcols[i]]
    vv <- gsub('SE','',seN)
    sig[nnn==vv] <- ttest(mat1[nnn==vv],mat2[nnn==vv],mat1[nnn==seN],mat2[nnn==seN])
  }
  sig
}


#### without significance tests

FIX <- function(tib){
    lst <- sapply(tib,is.list)

    out <- NULL
    for(nn in names(tib)[lst]){
        out <- round(cbind(out,
                     do.call('rbind',tib[[nn]])),1)
    }
    out <- cbind(tib[,!lst],out)
    names(out)[1:sum(!lst)] <- ''
    out
}

FIX2 <- function(tib){
    tib <- FIX(tib)
    out <- t(tib[,-c(1,ncol(tib))])
    colnames(out) <- tib[,1]
    out
}


###############
### standard calculations+translation thereof
###############


stand1 <- function(x,FUN,dat,...)
  list(
    overall=dat%>%group_by(deaf)%>%do(x=FUN(x,.)),
    byAge=dat%>%group_by(deaf,Age)%>%do(x=FUN(x,.)),
    bySex=dat%>%group_by(deaf,Sex)%>%do(x=FUN(x,.)),
    byRace=dat%>%group_by(deaf,raceEth)%>%do(x=FUN(x,.)),
    byRaceGender=dat%>%group_by(deaf,raceEth,Sex)%>%do(x=FUN(x,.)),
    byParenthood=dat%>%group_by(deaf,liveWkids)%>%do(x=FUN(x,.)),
    byDiss=list(
      dat%>%group_by(deaf,diss)%>%do(x=FUN(x,.)),
      dat%>%group_by(deaf,blind)%>%do(x=FUN(x,.)),
      dat%>%group_by(deaf,selfCare)%>%do(x=FUN(x,.)),
      dat%>%group_by(deaf,indLiv)%>%do(x=FUN(x,.)),
      dat%>%group_by(deaf,amb)%>%do(x=FUN(x,.)),
      dat%>%group_by(deaf,servDis)%>%do(x=FUN(x,.)),
      dat%>%group_by(deaf,cogDif)%>%do(x=FUN(x,.))),
    ...
  )

stand2 <- function(s1,med=FALSE){
  out <- list()
  for(nn in names(s1)){
    print(nn)
     if(is.data.frame(s1[[nn]])){
       out[[nn]] <- FIXsig(s1[[nn]],med=med)
     } else out[[nn]] <- do.call('rbind',lapply(s1[[nn]],FIXsig,med=med))
  }
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

attainment1 <- stand1('attainCum',factorProps,dat)
attainment <- stand2(attainment1)

## write results to spreadsheet
attainment$info <- info
openxlsx::write.xlsx(attainment,'EducatonalAttainment2017.xlsx',colWidths='auto')

#################################################################################################
#### Employment, LFP
#################################################################################################

###############
### employment
###############

empFun <- function(x,.data){
    emp <- factorProps('employment',.data)
    ft <- estExpr(fulltime,employment=='Employed',sdat=.data)
    ft <- c(ft[1],100-ft[1],ft[2:3])
    names(ft) <- c('% Employed Full-Time','% Employed Part-Time','Full/Part-Time SE','Employed n')
    c(emp,ft)
}

employment1 <- stand1('employment',empFun,dat,
                       byAttainment=dat%>%group_by(deaf,attainCum)%>%do(x=empFun(1,.)),
                       `Field of Degree (small)`=
                           dat%>%
                           filter(attainCum>='Bachelors')%>%
                           group_by(deaf,fodSmall)%>%
                           do(x=empFun(1,.)),
                       `Field of Degree (big)`=
                           dat%>%
                           filter(attainCum>='Bachelors')%>%
                           group_by(deaf,fodBig)%>%
                           do(x=empFun(1,.)))

employment <- stand2(employment1)

## write results to spreadsheet
employment$info <- rbind(info,'Full/Part-time expressed as percentage of employed people')
openxlsx::write.xlsx(employment,'employment2017.xlsx',colWidths='auto')

###############
### by industry category
###############
employmentByIndustry <- FIX(dat%>%filter(fulltime)%>%group_by(deaf)%>%do(x=factorProps('industry',.)))
## write results to spreadsheet
employmentByIndustry <- info <- rbind(info,'Full-time employed people only')
openxlsx::write.xlsx(list(employmentByIndustry=employmentByIndustry,
  info=rbind(info,'Full-time employed people only')),
  'industryPercentagesFT2017.xlsx', rowNames=TRUE,colWidths='auto')

###############
### employment by age (plot)
###############
empByAge <- FIX(dat%>%group_by(deaf,agep)%>%do(x=estExpr(employment=="Employed",sdat=.)))
names(empByAge)[1:2] <- c('deaf','Age')
openxlsx::write.xlsx(list(empByAge=empByAge), info=info),'EmploymentByAge2017.xlsx')

ggplot(empByAge,aes(Age,est,color=deaf,group=deaf))+geom_smooth()+labs(color=NULL,y='% Employed')
ggsave('employmentByAge.jpg')

###############
## LFP
###############

### basically just extract info from 'employment'
inLaborForce <-
  lapply(employment,
    function(x)
      setNames(
        data.frame(x[,seq(ncol(x)-11)],
          vapply(1:nrow(x),
            function(i) ifelse(i%%3==0,x[['% Not In Labor Force']][i],100-x[['% Not In Labor Force']][i]),1.2),
          x[['Not In Labor Force SE']],x$n),
        c(rep('',ncol(x)-11),'% In Labor Force','SE','n')))

## write results to spreadsheet
inLaborForce$info <- rbind(info,'Calculated from employment numbers')
openxlsx::write.xlsx(inLaborForce,'LaborForceParticipation2017.xlsx',colWidths='auto')

#################################################################################################
#### Earnings
#################################################################################################


medianEarnings1 <-
    c(stand1(~pernp,med,filter(dat,fulltime),
             byAttainment=dat%>%filter(fulltime)%>%group_by(deaf,attainCum)%>%do(x=med(~pernp,sdat=.)),
             `Field of Degree (small)`=
                 dat%>%
                 filter(attainCum>='Bachelors',fulltime)%>%
                 group_by(deaf,fodSmall)%>%
                 do(x=med(~pernp,sdat=.)),
             `Field of Degree (big)`=
                 dat%>%
                 filter(attainCum>='Bachelors',fulltime)%>%
                 group_by(deaf,fodBig)%>%
                 do(x=med(~pernp,sdat=.)),
             byIndustry=
                 dat%>%
                 filter(fulltime)%>%
                 group_by(deaf,industry)%>%
                 do(x=med(~pernp,sdat=.))),
             everyone=list(dat%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.))),
             employed=list(dat%>%filter(employment=='Employed')%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.))))
medianEarnings <- stand2(medianEarnings1,med=TRUE)
names(medianEarnings)[!names(medianEarnings)%in%c('everyone','employed')] <-
  paste(names(medianEarnings)[!names(medianEarnings)%in%c('everyone','employed')],'(Full-time)')

medianEarnings$info <- rbind(info,c('Earnings are for full-time employed people, except in "everyone" and "employed" tabs'))
openxlsx::write.xlsx(medianEarnings,'medianEarnings2017.xlsx',colWidths='auto')



## earnings by age
ernByAge <- FIX(dat%>%filter(fulltime)%>%group_by(deaf,agep)%>%do(x=med(~pernp,sdat=.)))
names(ernByAge) <- c('deaf','Age','median_earnings','se','n')
openxlsx::write.xlsx(ernByAge,'medianEarningsByAgeFullTime2017.xlsx')

ggplot(ernByAge,aes(Age,median_earnings,color=deaf,group=deaf))+geom_smooth()+labs(color=NULL,y='Median Earnings (Full-Time Employed)')
ggsave('earningsByAge.jpg')

## mean earnings by age
ernByAgeMean <- FIX(dat%>%filter(fulltime)%>%group_by(deaf,agep)%>%do(x=estSEstr('pernp',sdat=.)))
names(ernByAgeMean) <- c('deaf','Age','mean_earnings','se','n')
openxlsx::write.xlsx(ernByAgeMean,'meanEarningsByAgeFullTime2017.xlsx')

ggplot(ernByAgeMean,aes(Age,mean_earnings,color=deaf,group=deaf))+geom_smooth()+geom_point()+labs(color=NULL,y='Mean Earnings (Full-Time Employed)')
ggsave('earningsByAgeMean.jpg')


#################################################################################################
#### Self employment and business ownership
#################################################################################################

bizOwner1 <- list(
  overall= dat%>%group_by(deaf)%>%do(x=estExpr(bizOwner,sdat=.)),
  employed=dat%>%filter(employment=='Employed')%>%group_by(deaf)%>%do(x=estExpr(bizOwner,sdat=.)),
  under40=dat%>%filter(agep<40)%>%group_by(deaf)%>%do(x=estExpr(bizOwner,sdat=.)),
  under40Employed=dat%>%filter(agep<40,employment=='Employed')%>%group_by(deaf)%>%do(x=estExpr(bizOwner,sdat=.)))

bizOwner <- do.call('rbind',lapply(bizOwner1,FIXsig,med=TRUE))

names(bizOwner)[2] <- '% Owns Business'

selfEmp1 <- list(
  overall= dat%>%group_by(deaf)%>%do(x=estExpr(selfEmp,sdat=.)),
  employed=dat%>%filter(employment=='Employed')%>%group_by(deaf)%>%do(x=estExpr(selfEmp,sdat=.)),
  under40=dat%>%filter(agep<40)%>%group_by(deaf)%>%do(x=estExpr(selfEmp,sdat=.)),
  under40Employed=dat%>%filter(agep<40,employment=='Employed')%>%group_by(deaf)%>%do(x=estExpr(selfEmp,sdat=.)))
selfEmp <- do.call('rbind',lapply(selfEmp1,FIXsig,med=TRUE))
names(selfEmp)[2] <- '% Self Employed'

medEarnBizOwner1 <- list(
  bizOwner=dat%>%filter(fulltime,bizOwner)%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.)),
  bizOwnerUnder40=dat%>%filter(fulltime,bizOwner,agep<40)%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.)),
  selfEmp=dat%>%filter(fulltime,selfEmp)%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.)),
  selfEmpUnder40=dat%>%filter(fulltime,selfEmp,agep<40)%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.)))
medEarnBizOwner <- do.call('rbind',lapply(medEarnBizOwner1,FIXsig,med=TRUE))
names(medEarnBizOwner) <- c('','Median Earnings','SE','n')

selfEmployment=list(`Owns Business`=bizOwner,
  `Self Employed`=selfEmp,
  `Earnings by Self Employment`=medEarnBizOwner,
  info=info)

### write to spreadsheet
openxlsx::write.xlsx(selfEmployment,'selfEmployment2017.xlsx',rowNames=TRUE,colWidths='auto')


#################################################################################################
#### Population breakdown (i.e. how many ppl in each category)
#################################################################################################


popBreakdown <- list(
    percentDeaf=factorProps('deaf',dat),
    byAge=FIX(dat%>%group_by(deaf)%>%do(x=factorProps('Age',.))),
    bySex=FIX(dat%>%group_by(deaf)%>%do(x=factorProps('Sex',.))),
    byRace=FIX(dat%>%group_by(deaf)%>%do(x=factorProps('raceEth',.))),
    byDiss=rbind(
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('diss',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('blind',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('selfCare',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('indLiv',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('amb',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('servDis',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('cogDif',.)))))


##### field of degree
fod1D <- factorProps('fodSmall',filter(dat,deaf=='deaf',attain>='Bachelors degree'))
fod1H <- factorProps('fodSmall',filter(dat,deaf=='hearing',attain>='Bachelors degree'))
seCol1 <- grep(' SE',names(fod1D),fixed=TRUE)

fod1D <- cbind(`Deaf %`=c(n=fod1D['n'],fod1D[-c(seCol1,length(fod1D))]),
              `Deaf SE`=c(NA,fod1D[seCol1]))
fod1H <- cbind(`Hearing %`=c(n=fod1H['n'],fod1H[-c(seCol1,length(fod1H))]),
              `Hearing SE`=c(NA,fod1H[seCol1]))

popBreakdown$`Field of Degree (small)`=round(cbind(fod1D,fod1H),1)

fod2D <- factorProps('fodBig',filter(dat,deaf=='deaf',attain>='Bachelors degree'))
fod2H <- factorProps('fodBig',filter(dat,deaf=='hearing',attain>='Bachelors degree'))
seCol2 <- grep(' SE',names(fod2D),fixed=TRUE)

fod2D <- cbind(`Deaf %`=c(n=fod2D['n'],fod2D[-c(seCol2,length(fod2D))]),
              `Deaf SE`=c(NA,fod2D[seCol2]))
fod2H <- cbind(`Hearing %`=c(n=fod2H['n'],fod2H[-c(seCol2,length(fod2H))]),
              `Hearing SE`=c(NA,fod2H[seCol2]))

popBreakdown$`Field of Degree (big)`=round(cbind(fod2D,fod2H),1)

##### industry codes
occcodeD <- factorProps('industry',filter(dat,deaf=='deaf',fulltime))
occcodeH <- factorProps('industry',filter(dat,deaf=='hearing',fulltime))
seColOcc <- grep(' SE',names(occcodeD),fixed=TRUE)

occcodeD <- cbind(`Deaf %`=c(n=occcodeD['n'],occcodeD[-c(seColOcc,length(occcodeD))]),
              `Deaf SE`=c(NA,occcodeD[seColOcc]))
occcodeH <- cbind(`Hearing %`=c(n=occcodeH['n'],occcodeH[-c(seColOcc,length(occcodeH))]),
              `Hearing SE`=c(NA,occcodeH[seColOcc]))

popBreakdown$`Industry`=round(cbind(occcodeD,occcodeH),1)

popBreakdown[1:4] <- lapply(popBreakdown[1:4],t)

### write to spreadsheet
popBreakdown$info <- info
openxlsx::write.xlsx(popBreakdown,'populationBreakdown2017.xlsx',rowNames=TRUE,colWidths='auto')




### this is mostly messing around
### test (log) earnings disparities

mod0 <- lm(log(pernp)~deaf+as.factor(agep)+fodBig,data=dat,weights=pwgtp,subset=pernp>0&attain>="Bachelors degree")
betas <- matrix(nrow=length(coef(mod0)),ncol=80)

for(i in 1:80){
  cat(i,' ')
  dat$wrep <- dat[[paste0('pwgtp',i)]]
  dat$wrep[dat$wrep<0] <- 0
  betas[,i] <- coef(update(mod0,weights=wrep))
}
beta0 <- coef(mod0)
se <- sapply(1:length(beta0),function(i) sqrt(mean((betas[i,]-beta0[i])^2)*4))
T <- beta0/se
pval <- 2*pnorm(-abs(T))

print(cbind(beta0,se,T,pval)[1:4,])

### what about only full-time employed?
mod0 <- lm(log(pernp)~deaf+as.factor(agep)+fodBig,data=dat,weights=pwgtp,
  subset=pernp>0&attain>="Bachelors degree"&fulltime)
betas <- matrix(nrow=length(coef(mod0)),ncol=80)

for(i in 1:80){
  cat(i,' ')
  dat$wrep <- dat[[paste0('pwgtp',i)]]
  dat$wrep[dat$wrep<0] <- 0
  betas[,i] <- coef(update(mod0,weights=wrep))
}
beta0 <- coef(mod0)
se <- sapply(1:length(beta0),function(i) sqrt(mean((betas[i,]-beta0[i])^2)*4))
T <- beta0/se
pval <- 2*pnorm(-abs(T))

print(cbind(beta0,se,T,pval)[1:4,])


save(setdiff(ls(),'dat'),file='results.RData')
