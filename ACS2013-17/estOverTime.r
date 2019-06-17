library(tidyverse)
library(reshape2)
source('../generalCode/estimationFunctions.r')
capitalize <- function(x) paste0(toupper(substr(x,1,1)),substr(x,2,nchar(x)))


## need employment (let's do full/part time for fun) % BA and %HS for 2008-2017
## black/white hearing/deaf, men/women
## data exclusion: age>24 age<65 relp!=16

varNames <- c('AGEP','SEX','DEAR','ESR','SCHL','PWGTP','RELP','RACBLK','REL')
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
    filter(agep>24,agep<65,relp!=16,racblk==1)
  gc()

  dat%>%
    transmute(
      deaf=factor(ifelse(dear==1,'deaf','hearing')),
      hs=schl>=16,
      ba=schl>=21,
      employed=esr%in%c(1,2,4,5),
      sex=ifelse(sex==1,'Male','Female'),
      pwgtp=pwgtp
    )%>%
    bind_rows(.,mutate(.,sex='Overall'))%>%
    group_by(deaf,sex)%>%
    summarize(
      hs=svmean(hs,pwgtp),
      ba=svmean(ba,pwgtp),
      employed=svmean(employed,pwgtp)
    )
}

repBlank <- function(x) c(x[1],vapply(2:length(x), function(i) if(x[i]==x[i-1]) '' else x[i],x[1]))

overTime <- sapply(8:17,est1year,simplify=FALSE)

save(overTime,file='output/overTimeEstimates.RData')

overTime <- map_dfr(1:length(overTime),~mutate(overTime[[.]],year=2007+.))

edOverall <-
  overTime%>%
  filter(sex=='Overall')%>%
  select(deaf,hs,ba,year)%>%
  melt(id.vars=c('deaf','year'))%>%
  mutate(value=round(value*100,1))%>%
  dcast(deaf+variable~year)%>%
  select(variable,deaf,everything())%>%
  mutate(growth=`2017`-`2008`,growth=paste0(ifelse(growth>0,'+',''),growth))%>%
  arrange(match(variable,c('hs','ba')),match(deaf,c('deaf','hearing')))%>%
  mutate(
    deaf=repBlank(capitalize(as.character(deaf))),
    variable=repBlank(toupper(as.character(variable)))
  )%>%t()

edSex <- overTime%>%
  filter(sex!='Overall')%>%
  select(deaf,sex,hs,ba,year)%>%
  melt(id.vars=c('deaf','sex','year'))%>%
  mutate(value=round(value*100,1))%>%
  dcast(sex+variable+deaf~year)%>%
#  select(variable,deaf,everything())%>%
  mutate(
    growth=round(`2017`-`2008`,1),
    growth=paste0(ifelse(growth>0,'+',''),growth)
  )%>%
  arrange(
    match(sex,c('Female','Male')),
    match(variable,c('hs','ba')),
    match(deaf,c('deaf','hearing'))
  )%>%
  mutate(
    sex=repBlank(c(Female="Women",Male='Men')[sex]),
    deaf=capitalize(as.character(deaf)),
    variable=repBlank(toupper(as.character(variable)))
  )%>%t()

emp <-
  overTime%>%
  filter(sex=='Overall')%>%
  select(deaf,employed,year)%>%
  mutate(employed=round(employed*100,1))%>%
  spread(year,employed)%>%
  ungroup()%>%
  mutate(deaf=capitalize(as.character(deaf)))


openxlsx::write.xlsx(
  list(
    attainment=edOverall,
    attainmentBySex=edSex,
    employment=emp
  ),
  'output/educationEmploymentByYear08-17.xlsx',
  rowNames=c(TRUE,TRUE,FALSE),
  colNames=c(FALSE,FALSE,TRUE)
)
