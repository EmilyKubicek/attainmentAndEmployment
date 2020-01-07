library(readr) ## read in the csvs faster
library(tidyverse)
library(pryr)
library(openxlsx)
states <- read_csv('../generalCode/states.csv')
jobs <- read_csv('../generalCode/occupations.csv')
hispCats <- read_csv('../generalCode/hisp.csv') # copied from data dictionary
raceCats <- read_csv('../generalCode/race1.csv') # copied from data dictionary

varNames <- c('SERIALNO','ST','AGEP','DEAR','FDEARP','RAC1P','HISP','SEX','PWGTP','RELP','RAC3P','NATIVITY','LANX')


ctypes <- rep('i',length(varNames))
names(ctypes) <- varNames
ctypes[c('SERIALNO')] <- 'c'
ctypes$.default <- '_'
colTypes <- do.call('cols',as.list(ctypes))


dat <- read_csv('../../../data/acs5yr2017/psam_pusa.csv',col_types=colTypes)
dim(dat)
setdiff(varNames,names(dat))
setdiff(names(dat),varNames)

for(ll in c('b','c','d'))
  dat <- bind_rows(dat,
    read_csv(paste0('../../../data/acs5yr2017/psam_pus',ll,'.csv'),col_types=colTypes))
gc()

names(dat) <- tolower(names(dat))

stopifnot(all(seq_along(hispCats$num)==as.numeric(hispCats$num)))
stopifnot(all(seq_along(raceCats$num)==as.numeric(raceCats$num)))

dat <- dat%>%
  filter(agep>24,agep<64,relp!=16)%>%
  mutate(
    hisp2=hispCats$hisp[hisp],
    race=raceCats$race[rac1p],
    deaf=ifelse(dear==1,'deaf','hearing')
    )

hisp2 <- xtabs(~hisp2+deaf,dat)

dat <- mutate(dat,
  hisp3=ifelse(
    hisp2%in%c(
      'Argentinean',
      'Bolivian',
      'Chilean',
      'Colombian',
      'Ecuadorian',
      'Other South American',
      'Paraguayan',
      'Peruvian',
      'Uruguayan',
      'Venezuelan'
    ),
    'South American',
    ifelse(hisp2%in%c(
      'Costa Rican',
      'Guatemalan',
      'Honduran',
      'Nicaraguan',
      'Other Central American',
      'Panamanian',
      'Salvadoran'
    ),
    'Central American',
    hisp2
    )
  )
)

hisp3 <- xtabs(~hisp3+deaf,dat)

raceTab <-
  dat%>%
  filter(deaf=='deaf')%$%
  table(hisp3,race)

raceTab2 <-
  map_dfc(1:nrow(raceTab),~raceTab[,.])%>%
  as.data.frame()%>%
  `rownames<-`(rownames(raceTab))%>%
  `names<-`(colnames(raceTab))

raceTab1 <- dat%>%
  mutate(latinx=ifelse(hisp>1,'latinx','not latinx'))%>%
  filter(deaf=='deaf')%>%
  xtabs(~race+latinx,.)

write.xlsx(list(
  detailed=hisp2,
  centralSouth=hisp3,
  `byRace1 (deaf only)`=raceTab1,
  `byRace2 (deaf only)`=raceTab2),
  'crosstabs.xlsx',
  rowNames=TRUE,
  colNames=TRUE)
