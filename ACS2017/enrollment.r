library(dplyr)
library(broom)
library(ggplot2)

if(exists('dat')) print('using dataset "dat" already in workspace')

if(!exists("dat")){
  if('enrollmentDataACS17.RData'%in%list.files()){
    print('loading enrollmentDataACS17.RData from folder')
    load('enrollmentDataACS17.RData')
  } else{
    print('running makeDataEnr.r')
    source('makeDataEnr.r')
  }
}

print(xtabs(~raceEth+attainCum+Sex,data=dat))

source('../generalCode/estimationFunctions.r')


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

  out <- as.data.frame(rbind(tib1$x[[1]],tib1$x[[2]],sig))
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

  #names(out)[1:sum(!lst)] <- ''
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
  if(all(nnn==c('est','se','n')))
    return(c(ttest(mat1['est'],mat2['est'],mat1['se'],mat2['se']),NA,NA))
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

overall <- FIXsig(dat%>%group_by(deaf)%>%do(x=estSEstr('enrolledPS',sdat=.)))
bySex <- FIXsig(dat%>%group_by(deaf,Sex)%>%do(x=estSEstr('enrolledPS',sdat=.)))
byRace <- FIXsig(dat%>%group_by(deaf,raceEth)%>%do(x=estSEstr('enrolledPS',sdat=.)))
byRaceSex <- FIXsig(dat%>%group_by(deaf,raceEth,Sex)%>%do(x=estSEstr('enrolledPS',sdat=.)))

tot <- cbind(overall$est,bySex$est[bySex$Sex=='Male'],bySex$est[bySex$Sex=='Female'])
rownames(tot) <- c('Deaf','Hearing','p-value')
colnames(tot) <- c('All','Male','Female')
tot[1:2,] <- tot[1:2,]*100

makeSmallTab <- function(oa,gen){
  tab <- matrix(nrow=3,ncol=3)
  rownames(tab) <- c('deaf','hearing','p-value')
  colnames(tab) <- c('All','Male','Female')
  for(dh in c('deaf','hearing')){
    tab[dh,'All'] <- oa$est[oa$dh==dh]*100
    for(ss in c('Male','Female'))
      tab[dh,ss] <- gen$est[gen$dh==dh&gen$Sex==ss]*100
  }
  tab['p-value','All'] <- oa$est[oa$dh=='p-val (deaf-hearing)']
  for(ss in c('Male','Female'))
    tab['p-value',ss] <- gen$est[gen$dh=='p-val (deaf-hearing)'&gen$Sex==ss]
  tab
}

smallTabs <- sapply(unique(dat$raceEth),
  function(rr) makeSmallTab(subset(byRace,raceEth==rr),subset(byRaceSex,raceEth==rr)),simplify=FALSE)


byDiss <- FIXsig(dat%>%group_by(deaf,diss)%>%do(x=estSEstr('enrolledPS',sdat=.)))
byDissSex <- FIXsig(dat%>%group_by(deaf,diss,Sex)%>%do(x=estSEstr('enrolledPS',sdat=.)))

byBlind <- FIXsig(dat%>%group_by(deaf,blind)%>%do(x=estSEstr('enrolledPS',sdat=.)))
byBlindSex <- FIXsig(dat%>%group_by(deaf,blind,Sex)%>%do(x=estSEstr('enrolledPS',sdat=.)))

smallTabsDiss <- sapply(unique(dat$diss),
  function(rr) makeSmallTab(subset(byDiss,diss==rr),subset(byDissSex,diss==rr)),simplify=FALSE)

smallTabsBlind <- sapply(unique(dat$blind),
  function(rr) makeSmallTab(subset(byBlind,blind==rr),subset(byBlindSex,blind==rr)),simplify=FALSE)

info <- data.frame(c('Dataset: ACS',
                     'Year: 2017',
                     'Ages: 18-64',
                     'Excludes Institutionalized People',
                     'Enrollment=Post-secondary Enrollment'
                     ),
                   stringsAsFactors=FALSE)


openxlsx::write.xlsx(list(overall=overall,bySex=bySex,byRaceSex=byRaceSex,byDiss=byDiss,byDissSex=byDissSex,byBlindSex=byBlindSex,info=info),file='post-secondary enrollment/fullResultsEnrollment.xlsx',row.names=TRUE,col.names=TRUE)

openxlsx::write.xlsx(c(smallTabs,info=info),file='post-secondary enrollment/niceTabsRaceEnrollment.xlsx',,row.names=TRUE,col.names=TRUE)
openxlsx::write.xlsx(c(smallTabsDiss,info=info),file='post-secondary enrollment/niceTabsDissEnrollment.xlsx',,row.names=TRUE,col.names=TRUE)
openxlsx::write.xlsx(c(smallTabsBlind,info=info),file='post-secondary enrollment/niceTabsBlindEnrollment.xlsx',,row.names=TRUE,col.names=TRUE)


dat$enr <- dat$enrolled=='enrolled'

overall2 <- FIXsig(dat%>%group_by(deaf)%>%do(x=estSEstr('enr',sdat=.)))
bySex2 <- FIXsig(dat%>%group_by(deaf,Sex)%>%do(x=estSEstr('enr',sdat=.)))
byRace2 <- FIXsig(dat%>%group_by(deaf,raceEth)%>%do(x=estSEstr('enr',sdat=.)))
byRaceSex2 <- FIXsig(dat%>%group_by(deaf,raceEth,Sex)%>%do(x=estSEstr('enr',sdat=.)))

tot2 <- cbind(overall2$est,bySex2$est[bySex2$Sex=='Male'],bySex2$est[bySex2$Sex=='Female'])
rownames(tot2) <- c('Deaf','Hearing','p-value')
colnames(tot2) <- c('All','Male','Female')
tot2[1:2,] <- tot2[1:2,]*100

smallTabs2 <- sapply(unique(dat$raceEth),
  function(rr) makeSmallTab(subset(byRace2,raceEth==rr),subset(byRaceSex2,raceEth==rr)),simplify=FALSE)


byDiss2 <- FIXsig(dat%>%group_by(deaf,diss)%>%do(x=estSEstr('enr',sdat=.)))
byDissSex2 <- FIXsig(dat%>%group_by(deaf,diss,Sex)%>%do(x=estSEstr('enr',sdat=.)))

byBlind2 <- FIXsig(dat%>%group_by(deaf,blind)%>%do(x=estSEstr('enr',sdat=.)))
byBlindSex2 <- FIXsig(dat%>%group_by(deaf,blind,Sex)%>%do(x=estSEstr('enr',sdat=.)))

smallTabsDiss2 <- sapply(unique(dat$diss),
  function(rr) makeSmallTab(subset(byDiss2,diss==rr),subset(byDissSex2,diss==rr)),simplify=FALSE)

smallTabsBlind2 <- sapply(unique(dat$blind),
  function(rr) makeSmallTab(subset(byBlind2,blind==rr),subset(byBlindSex2,blind==rr)),simplify=FALSE)


info2 <- data.frame(c('Dataset: ACS',
                     'Year: 2017',
                     'Ages: 18-64',
                     'Excludes Institutionalized People',
                     'Enrollment=Any Enrollment'
                     ),
                   stringsAsFactors=FALSE)


openxlsx::write.xlsx(list(overall=overall2,bySex=bySex2,byRaceSex=byRaceSex2,byDiss=byDiss2,byDissSex=byDissSex2,byBlindSex=byBlindSex2,info=info2),file='any enrollment/fullResultsAnyEnrollment.xlsx',row.names=TRUE,col.names=TRUE)

openxlsx::write.xlsx(c(smallTabs2,info=info),file='any enrollment/niceTabsRaceAnyEnrollment.xlsx',,row.names=TRUE,col.names=TRUE)
openxlsx::write.xlsx(c(smallTabsDiss2,info=info),file='any enrollment/niceTabsDissAnyEnrollment.xlsx',,row.names=TRUE,col.names=TRUE)
openxlsx::write.xlsx(c(smallTabsBlind2,info=info),file='any enrollment/niceTabsBlindAnyEnrollment.xlsx',,row.names=TRUE,col.names=TRUE)


save(list=setdiff(ls(),'dat'),file='resultsEnr.RData')

