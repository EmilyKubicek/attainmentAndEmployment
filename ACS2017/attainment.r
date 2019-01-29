library(dplyr)
library(broom)
library(ggplot2)

if(!exists("dat")){
    if('attainmentEmploymentDataACS17.RData'%in%list.files()){
        load('attainmentEmploymentDataACS17.RData')
    } else source('makeData.r')
}


source('../generalCode/estimationFunctions.r')
source('../generalCode/median.r')


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

standard <- function(x,FUN,dat,...)
    list(
        overall=FIX(dat%>%group_by(deaf)%>%do(x=FUN(x,.))),
        byAge=FIX(dat%>%group_by(deaf,Age)%>%do(x=FUN(x,.))),
        bySex=FIX(dat%>%group_by(deaf,sex)%>%do(x=FUN(x,.))),
        byRace=FIX(dat%>%group_by(deaf,raceEth)%>%do(x=FUN(x,.))),
        byRaceGender=FIX(dat%>%group_by(deaf,raceEth,sex)%>%do(x=FUN(x,.))),
        byParenthood=FIX(dat%>%group_by(deaf,liveWkids)%>%do(x=FUN(x,.))),
        byDiss=rbind(
            FIX(dat%>%group_by(deaf,diss)%>%do(x=FUN(x,.))),
            FIX(dat%>%group_by(deaf,blind)%>%do(x=FUN(x,.))),
            FIX(dat%>%group_by(deaf,selfCare)%>%do(x=FUN(x,.))),
            FIX(dat%>%group_by(deaf,indLiv)%>%do(x=FUN(x,.))),
            FIX(dat%>%group_by(deaf,amb)%>%do(x=FUN(x,.))),
            FIX(dat%>%group_by(deaf,servDis)%>%do(x=FUN(x,.))),
            FIX(dat%>%group_by(deaf,cogDif)%>%do(x=FUN(x,.)))),
        ...
    )



attainment <- standard('attainCum',factorProps,dat)

empFun <- function(x,.data){
    emp <- factorProps('employment',.data)
    ft <- estExpr(fulltime,employment=='Employed',sdat=.data)
    ft <- c(ft[1],100-ft[1],ft[2:3])
    names(ft) <- c('% Employed Full-Time','% Employed Part-Time','Full/Part-Time SE','Employed n')
    c(emp,ft)
}

employment <- standard('employment',empFun,dat,
                       byAttainment=FIX(dat%>%group_by(deaf,attainCum)%>%do(x=empFun(1,.))),
                       `Field of Degree (small)`=FIX(
                           dat%>%
                           filter(attainCum>='Bachelors')%>%
                           group_by(deaf,fodSmall)%>%
                           do(x=empFun(1,.))),
                       `Field of Degree (big)`=FIX(
                           dat%>%
                           filter(attainCum>='Bachelors')%>%
                           group_by(deaf,fodBig)%>%
                           do(x=empFun(1,.)))
                       )

inLaborForce <- lapply(employment,
                       function(x) setNames(data.frame(x[,seq(ncol(x)-11)],
                                                       100-x[['% Not In Labor Force']],
                                                       x[['Not In Labor Force SE']],x$n),
                                            c(rep('',ncol(x)-11),'% In Labor Force','SE','n')))


medianEarnings <-
    standard(~pernp,med,filter(dat,fulltime),
             byAttainment=FIX(dat%>%filter(fulltime)%>%group_by(deaf,attainCum)%>%do(x=med(~pernp,sdat=.))),
             `Field of Degree (small)`=FIX(
                 dat%>%
                 filter(attainCum>='Bachelors',fulltime)%>%
                 group_by(deaf,fodSmall)%>%
                 do(x=med(~pernp,sdat=.))),
             `Field of Degree (big)`=FIX(
                 dat%>%
                 filter(attainCum>='Bachelors',fulltime)%>%
                 group_by(deaf,fodBig)%>%
                 do(x=med(~pernp,sdat=.))),
             byIndustry=FIX(
                 dat%>%
                 filter(fulltime)%>%
                 group_by(deaf,industry)%>%
                 do(x=med(~pernp,sdat=.))),
             overall=FIX(dat%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.))),
             employed=FIX(dat%>%filter(employment=='Employed')%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.))))


medianEarnings <- lapply(medianEarnings,
                         function(x) {
                             names(x)[names(x)=='1'] <- 'Med. Earnings'
                             names(x)[names(x)=='2'] <- 'SE'
                             names(x)[names(x)=='3'] <- 'n'
                             x
                         })

employmentByIndustry <- FIX(dat%>%filter(fulltime)%>%group_by(deaf)%>%do(x=factorProps('industry',.)))

############# self employed & business owners
## percent own small biz
bizOwner <- rbind(
  overall= FIX(dat%>%group_by(deaf)%>%do(x=estExpr(bizOwner,sdat=.))),
  employed=FIX(dat%>%filter(employment=='Employed')%>%group_by(deaf)%>%do(x=estExpr(bizOwner,sdat=.))),
  under40=FIX(dat%>%filter(agep<40)%>%group_by(deaf)%>%do(x=estExpr(bizOwner,sdat=.))),
  under40Employed=FIX(dat%>%filter(agep<40,employment=='Employed')%>%group_by(deaf)%>%do(x=estExpr(bizOwner,sdat=.))))
names(bizOwner)[2] <- '% Owns Business'

selfEmp <- rbind(
  overall= FIX(dat%>%group_by(deaf)%>%do(x=estExpr(selfEmp,sdat=.))),
  employed=FIX(dat%>%filter(employment=='Employed')%>%group_by(deaf)%>%do(x=estExpr(selfEmp,sdat=.))),
  under40=FIX(dat%>%filter(agep<40)%>%group_by(deaf)%>%do(x=estExpr(selfEmp,sdat=.))),
  under40Employed=FIX(dat%>%filter(agep<40,employment=='Employed')%>%group_by(deaf)%>%do(x=estExpr(selfEmp,sdat=.))))
names(selfEmp)[2] <- '% Self Employed'



medEarnBizOwner <- rbind(
  bizOwner=FIX(dat%>%filter(fulltime,bizOwner)%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.))),
  bizOwnerUnder40=FIX(dat%>%filter(fulltime,bizOwner,agep<40)%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.))),
  selfEmp=FIX(dat%>%filter(fulltime,selfEmp)%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.))),
  selfEmpUnder40=FIX(dat%>%filter(fulltime,selfEmp,agep<40)%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.))))
names(medEarnBizOwner) <- c('','Median Earnings','SE','n')

selfEmployment=list(`Owns Business`=bizOwner,`Self Employed`=selfEmp,`Earnings by Self Employment`=medEarnBizOwner)


popBreakdown <- list(
    percentDeaf=factorProps('deaf',dat),
    byAge=FIX(dat%>%group_by(deaf)%>%do(x=factorProps('Age',.))),
    bySex=FIX(dat%>%group_by(deaf)%>%do(x=factorProps('sex',.))),
    byRace=FIX(dat%>%group_by(deaf)%>%do(x=factorProps('raceEth',.))),
    byDiss=rbind(
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('diss',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('blind',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('selfCare',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('indLiv',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('amb',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('servDis',.))),
        FIX2(dat%>%group_by(deaf)%>%do(x=factorProps('cogDif',.)))))


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


occcodeD <- factorProps('industry',filter(dat,deaf=='deaf',fulltime))
occcodeH <- factorProps('industry',filter(dat,deaf=='hearing',fulltime))
seColOcc <- grep(' SE',names(occcodeD),fixed=TRUE)

occcodeD <- cbind(`Deaf %`=c(n=occcodeD['n'],occcodeD[-c(seColOcc,length(occcodeD))]),
              `Deaf SE`=c(NA,occcodeD[seColOcc]))
occcodeH <- cbind(`Hearing %`=c(n=occcodeH['n'],occcodeH[-c(seColOcc,length(occcodeH))]),
              `Hearing SE`=c(NA,occcodeH[seColOcc]))

popBreakdown$`Industry`=round(cbind(occcodeD,occcodeH),1)

info <- data.frame(c('Dataset: ACS',
                     'Year: 2017',
                     'Ages: 25-64',
                     'Excludes Institutionalized People',
                     'Occupational Category for full-time employed people only',
                     'Field of Degree for people with Bachelors degrees or higher'),
                   stringsAsFactors=FALSE)

names(info) <- c('')

attainment$info <- info
employment$info <- rbind(info,'Full/Part-time expressed as percentage of employed people')
medianEarnings$info <- rbind(info,c('Earnings are for full-time employed people, except in "overall" and "employed" tabs'))
popBreakdown$info <- info

popBreakdown[1:4] <- lapply(popBreakdown[1:4],t)
inLaborForce$info <- rbind(info,'Calculated from employment numbers')

openxlsx::write.xlsx(inLaborForce,'LaborForceParticipation2017.xlsx',colWidths='auto')
openxlsx::write.xlsx(attainment,'EducatonalAttainment2017.xlsx',colWidths='auto')
openxlsx::write.xlsx(employment,'employment2017.xlsx',colWidths='auto')
openxlsx::write.xlsx(medianEarnings,'medianEarnings2017.xlsx',colWidths='auto')
openxlsx::write.xlsx(popBreakdown,'populationBreakdown2017.xlsx',rowNames=TRUE,colWidths='auto')
openxlsx::write.xlsx(employmentByIndustry,'industryPercentagesFT2017.xlsx', rowNames=TRUE,colWidths='auto')
openxlsx::write.xlsx(selfEmployment,'selfEmployment2017.xlsx',rowNames=TRUE,colWidths='auto')

library(ggplot2)

## employment by age
empByAge <- FIX(dat%>%group_by(deaf,agep)%>%do(x=estExpr(employment=="Employed",sdat=.)))
names(empByAge)[1:2] <- c('deaf','Age')
openxlsx::write.xlsx(empByAge,'EmploymentByAge2017.xlsx')

ggplot(empByAge,aes(Age,est,color=deaf,group=deaf))+geom_smooth()+labs(color=NULL,y='% Employed')
ggsave('employmentByAge.jpg')

## earnings by age
ernByAge <- FIX(dat%>%filter(fulltime)%>%group_by(deaf,agep)%>%do(x=med(~pernp,dat=.)))
names(ernByAge) <- c('deaf','Age','median_earnings','se','n')
openxlsx::write.xlsx(ernByAge,'medianEarningsByAgeFullTime2017.xlsx')

ggplot(ernByAge,aes(Age,median_earnings,color=deaf,group=deaf))+geom_smooth()+labs(color=NULL,y='Median Earnings (Full-Time Employed)')
ggsave('earningsByAge.jpg')

## mean earnings by age
ernByAgeMean <- FIX(dat%>%filter(fulltime)%>%group_by(deaf,agep)%>%do(x=estSEstr('pernp',dat=.)))
names(ernByAgeMean) <- c('deaf','Age','mean_earnings','se','n')
openxlsx::write.xlsx(ernByAgeMean,'meanEarningsByAgeFullTime2017.xlsx')

ggplot(ernByAgeMean,aes(Age,mean_earnings,color=deaf,group=deaf))+geom_smooth()+geom_point()+labs(color=NULL,y='Mean Earnings (Full-Time Employed)')
ggsave('earningsByAgeMean.jpg')


