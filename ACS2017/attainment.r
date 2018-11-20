library(dplyr)
library(broom)

if(!exists("dat")){
    if('attainmentEmploymentData.RData'%in%list.files()){
        load('attainmentEmploymentData.RData')
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

employment <- standard('employment',factorProps,dat)
employment$byAttainment=FIX(dat%>%group_by(deaf,attainCum)%>%do(x=factorProps('employment',.)))

inLaborForce <- lapply(employment,
                       function(x) setNames(data.frame(x[,seq(ncol(x)-7)],
                                                       100-x[['% Not In Labor Force']],
                                                       x[['Not In Labor Force.se']],x$n),
                                            c(rep('',ncol(x)-7),'% In Labor Force','SE','n')))


medianEarnings <-
    standard(~pernp,med,filter(dat,fulltime),
             byAttainment=FIX(dat%>%filter(fulltime)%>%group_by(deaf,attainCum)%>%do(x=med(~pernp,sdat=.))),
             overall=FIX(dat%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.))),
             employed=FIX(dat%>%filter(employment=='Employed')%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.))),
             fulltime=FIX(dat%>%filter(fulltime)%>%group_by(deaf)%>%do(x=med(~pernp,sdat=.))))


medianEarnings <- lapply(medianEarnings,
                         function(x) {
                             names(x)[names(x)=='1'] <- 'Med. Earnings'
                             names(x)[names(x)=='2'] <- 'SE'
                             names(x)[names(x)=='3'] <- 'n'
                             x
                         })

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


fodD <- factorProps('degree',filter(dat,deaf=='deaf',attain>='Bachelors degree'))
fodH <- factorProps('degree',filter(dat,deaf=='hearing',attain>='Bachelors degree'))
seCol <- grep('.se',names(fodD),fixed=TRUE)

fodD <- cbind(`Deaf %`=c(n=fodD['n'],fodD[-c(seCol,length(fodD))]),
              `Deaf SE`=c(NA,fodD[seCol]))
fodH <- cbind(`Hearing %`=c(n=fodH['n'],fodH[-c(seCol,length(fodH))]),
              `Hearing SE`=c(NA,fodH[seCol]))

popBreakdown$`Field of Degree`=cbind(fodD,fodH)


occcodeD <- factorProps('industrycode',filter(dat,deaf=='deaf',fulltime))
occcodeH <- factorProps('industrycode',filter(dat,deaf=='hearing',fulltime))
seColOcc <- grep('.se',names(occcodeD),fixed=TRUE)

occcodeD <- cbind(`Deaf %`=c(n=occcodeD['n'],occcodeD[-c(seColOcc,length(occcodeD))]),
              `Deaf SE`=c(NA,occcodeD[seColOcc]))
occcodeH <- cbind(`Hearing %`=c(n=occcodeH['n'],occcodeH[-c(seColOcc,length(occcodeH))]),
              `Hearing SE`=c(NA,occcodeH[seColOcc]))

popBreakdown$`Occupation Category`=cbind(occcodeD,occcodeH)

info <- data.frame(c('Dataset',
                     'Years',
                     'Ages',
                     'Excludes',
                     'Earnings and Occupational Category',
                     'Field of Degree'),
                   c('ACS','2017','25-64','Institutionalized People',
                     'For full-time employed people only',
                     'For people with Bachelors degrees or higher'))
names(info) <- c('','')

attainment$info <- info
employment$info <- info
medianEarnings$info <- info
popBreakdown$info <- info


openxlsx::write.xlsx(attainment,'EducatonalAttainment2017.xlsx',colWidths='auto')
openxlsx::write.xlsx(employment,'employment2017.xlsx',colWidths='auto')
openxlsx::write.xlsx(medianEarnings,'medianEarnings2017.xlsx',colWidths='auto')
openxlsx::write.xlsx(popBreakdown,'populationBreakdown2017.xlsx',,colWidths='auto')
