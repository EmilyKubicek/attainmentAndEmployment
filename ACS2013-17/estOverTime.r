library(tidyverse)
source('../generalCode/estimationFunctions.r')

## need employment (let's do full/part time for fun) % BA and %HS for 2008-2017
## black/white hearing/deaf, men/women
## data exclusion: age>24 age<65 relp!=16

varNames <- c('AGEP','SEX','DEAR','ESR','SCHL','WKHP','WKW','PWGTP','RELP','REL','RAC1P','RACBLK','RACWHT')
ctypes <- rep('i',length(varNames))
names(ctypes) <- varNames
ctypes$.default <- '_'
colTypes <- do.call('cols',as.list(ctypes))


est1year <- function(yr){
  year <- ifelse(yr<10,paste0(0,yr),as.character(yr))
  print(year)
  dat <-
    rbind(
      read_csv(paste0('../../../data/byYear/ss',year,'pusa.csv'),col_types=colTypes),
      read_csv(paste0('../../../data/byYear/ss',year,'pusb.csv'),col_types=colTypes)
    )
  gc()
  names(dat) <- tolower(names(dat))
  if(is.element('rel',names(dat))& !is.element('relp',names(dat))) dat$relp <- dat$rel

  dat <- dat%>%
    filter(agep>24,agep<65,relp!=16)%>%
    transmute(
      deaf=factor(ifelse(dear==1,'deaf','hearing')),
      hs=schl>=16,
      ba=schl>=21,
      employed=esr%in%c(1,2,4,5),
      fulltime=employed&(wkw==1)&(wkhp>=35),
      sex=ifelse(sex==1,'male','female'),
      blackORwhite=ifelse(racblk==1,'Black',
        ifelse(rac1p==1,'White','Other')),
      pwgtp=pwgtp)
  gc()

  overall <-
    dat%>%group_by(deaf)%>%
    summarize(hs=svmean(hs,pwgtp),
      ba=svmean(ba,pwgtp),
      employed=svmean(employed,pwgtp),
      fulltime=svmean(fulltime,pwgtp))

  byRace <-
    dat%>%filter(blackORwhite!='Other')%>%group_by(deaf,blackORwhite)%>%
    summarize(hs=svmean(hs,pwgtp),
      ba=svmean(ba,pwgtp),
      employed=svmean(employed,pwgtp),
      fulltime=svmean(fulltime,pwgtp))


  byRaceSex <-
     dat%>%filter(blackORwhite!='Other')%>%group_by(deaf,blackORwhite,sex)%>%
     summarize(hs=svmean(hs,pwgtp),
      ba=svmean(ba,pwgtp),
      employed=svmean(employed,pwgtp),
      fulltime=svmean(fulltime,pwgtp))

  rm(dat); gc()

  out <- bind_rows(overall,byRace,byRaceSex)%>%
    select(deaf,blackORwhite,sex,everything())
  out
}

overTime <- sapply(8:17,est1year,simplify=FALSE)

makeTab <- function(varb){
  tab <- overTime[[1]]%>%select(deaf,blackORwhite,sex,!!sym(varb))
  for(i in 2:length(overTime))
    tab <- cbind(tab,overTime[[i]][[varb]])
  tab[,4:ncol(tab)] <- tab[,4:ncol(tab)]*100
  names(tab) <- c('deaf','race','sex',2000+8:17)
  tab
}

tabs <- sapply(c('hs','ba','employed','fulltime'),makeTab,simplify=FALSE)

openxlsx::write.xlsx(tabs,'educationEmploymentByYear08-17.xlsx')
