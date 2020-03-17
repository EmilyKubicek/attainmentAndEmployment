library(readr) ## read in the csvs faster
library(tidyverse)
library(pryr)
library(openxlsx)
library(magrittr)

states <- read_csv('../generalCode/states.csv')
jobs <- read_csv('../generalCode/occupations.csv')
hispCats <- read_csv('../generalCode/hisp.csv') # copied from data dictionary
raceCats <- read_csv('../generalCode/race1.csv') # copied from data dictionary



varNames <- c('SERIALNO','ST','AGEP','DEAR','FDEARP','RAC1P','HISP','SEX','PWGTP','RELP','RAC3P','NATIVITY','LANX')

tryDat <- read_csv('../../../data/acs5yr2018/psam_pusa.csv',n_max=10)
varNames <- c(varNames,grep('^RAC',names(tryDat),value=TRUE))

ctypes <- rep('i',length(varNames))
names(ctypes) <- varNames
ctypes[c('SERIALNO')] <- 'c'
ctypes$.default <- '_'
colTypes <- do.call('cols',as.list(ctypes))

##################### read in US data (50 states + DC)
datUS <- read_csv('../../../data/acs5yr2018/psam_pusa.csv',col_types=colTypes)
dim(datUS)
setdiff(varNames,names(datUS))
setdiff(names(datUS),varNames)

for(ll in c('b','c','d'))
  datUS <- bind_rows(datUS,
    read_csv(paste0('../../../data/acs5yr2018/psam_pus',ll,'.csv'),col_types=colTypes))
gc()

names(datUS) <- tolower(names(datUS))

##################### download PR data
datPR <- read_csv('../../../data/acs5yr2018/psam_p72.csv',col_types=colTypes)
names(datPR) <- tolower(names(datPR))


stopifnot(all(seq_along(hispCats$num)==as.numeric(hispCats$num)))
stopifnot(all(seq_along(raceCats$num)==as.numeric(raceCats$num)))

##################################################
#### find types of hispanic with >= 500 deaf people in the sample
##################################################

### smaller, nimbler dataset: only hispanic deaf ppl
hdus <- datUS%>%
  filter(agep>24,agep<64,relp!=16,hisp>1,dear==1)%>%
  mutate(
      hisp2=hispCats$hisp[hisp],
      race=raceCats$race[rac1p],
      black=racblk==1,
      asian=racasn==1
    )

### hisp categories with >500
us500 <- hdus%>%group_by(hisp2)%>%summarize(n=n())%>%filter(n>=500)%>%pull(hisp2)

hdus <- hdus%>%
  mutate(
    hisp3=fct_collapse(hisp2,
      "South American"=c("Argentinean","Bolivian",'Chilean','Colombian','Ecuadorian','Other South American','Paraguayan','Peruvian','Uruguayan','Venezuelan'),
      "Central American"=c("Costa Rican","Panamanian","Guatemalan","Honduran","Nicaraguan","Other Central American","Peruvian","Salvadoran")
    ),
    hisp4=fct_lump_min(hisp2,500,other_level="All Other Spanish/Hispanic/Latino")
  )

###### types of "Hispanic"

fct_count(hdus$hisp3)
fct_count(hdus$hisp4)

hdus <- mutate(hdus,hisp2=fct_relevel(hisp2,levels(hisp4)),hisp3=fct_relevel(hisp3,levels(hisp4)))

hispTypes <- with(hdus,bind_cols(
  fct_count(hisp2),
  rbind(fct_count(hisp3),data.frame(f=rep(NA,nlevels(hisp2)-nlevels(hisp3)),n=rep(NA,nlevels(hisp2)-nlevels(hisp3)))),
  rbind(fct_count(hisp4),data.frame(f=rep(NA,nlevels(hisp2)-nlevels(hisp4)),n=rep(NA,nlevels(hisp2)-nlevels(hisp4))))
))%>%
  rename("ACS Levels"=f,"Grouped by Continent"=f1,"ACS Levels n>500"=f2)

####### multi-ethnic categories
fct_count(hdus$race)

for(rac in grep('^rac',names(hdus),value=TRUE)) print(tab(as.formula(paste('~',rac)),hdus))

hdus <- hdus%>%
  mutate(
    black=racblk==1,
    white=racwht==1,
    AmIndAKNat=racaian==1,
    Asian=racasn==1,
    SomeOther=racsor==1
    )

nRaces=rbind(
  allHisp=hdus%>%summarize_at(vars(black,white:SomeOther),sum),
  map_dfr(levels(hdus$hisp3),function(x) hdus%>%filter(hisp3==x)%>%summarize_at(vars(black,white:SomeOther),sum))
)%>%
  mutate(hisp=c('All Latinx',levels(hdus$hisp3)))%>%
  select(hisp,everything())%>%
  add_case(hisp="Race categories are NOT mutually exclusive")


openxlsx::write.xlsx(list(LatinxType=hispTypes,MultiRacial=nRaces),file="results/latinxCategories.xlsx",rowNames=TRUE)



ct <- function(dat){
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

  raceTab2 <-
    dat%>%
    filter(deaf=='deaf')%$%
  table(hisp3,race)

  ## raceTab2 <-
  ##   map_dfc(1:nrow(raceTab),~raceTab[,.])%>%
  ##   as.data.frame()%>%
  ##   `rownames<-`(rownames(raceTab))%>%
  ##   `names<-`(colnames(raceTab))

  raceTab1 <- dat%>%
    mutate(latinx=ifelse(hisp>1,'latinx','not latinx'))%>%
    filter(deaf=='deaf')%>%
    xtabs(~race+latinx,.)

  list(
  detailed=hisp2,
  centralSouth=hisp3,
  `byRace1 (deaf only)`=raceTab1,
  `byRace2 (deaf only)`=raceTab2)
}

write.xlsx(ct(datUS),
  'crosstabs.xlsx',
  rowNames=TRUE,
  colNames=TRUE)

write.xlsx(ct(datPR),
  'crosstabsPR.xlsx',
  rowNames=TRUE,
  colNames=TRUE)
