library(tidyverse)
library(pryr)
library(openxlsx)
states <- read.csv('../../generalCode/states.csv')
jobs <- read.csv('../../generalCode/occupations.csv')
hispCats <- read_csv('../generalCode/hisp.csv') # copied from data dictionary
raceCats <- read_csv('../generalCode/race1.csv') # copied from data dictionary

#1) a simple breakdown of current enrollment, and completion data, across type of institution (4 year colleges, community colleges, etc) using all the 'type of institution' data we have, so that would give us some nice descriptives and allow us to make a final decision on how we want to categorize 'community colleges and 2-year institutions'

varNames <- c('SERIALNO','ST','AGEP','DDRS','DEAR','DEYE','DOUT','DPHY','DRATX','DREM','FDEARP','ESR','SCHL','SCHG','SCH','RAC1P','HISP','SEX','PERNP','PINCP','SSIP','WKHP','WKW','ADJINC','PWGTP','RELP','FOD1P','NAICSP','OCCP','INDP','COW','RAC2P','RAC3P','RACBLK','RACASN','RACWHT','RACAIAN','RACSOR','ADJINC','NATIVITY','LANX','MAR','JWTR',paste0('PWGTP',1:80))


ctypes <- rep('i',length(varNames))
names(ctypes) <- varNames
ctypes[c('SERIALNO','NAICSP','FOD1P','OCCP','INDP')] <- 'c'
ctypes$.default <- '_'
colTypes <- do.call('cols',as.list(ctypes))


dat <- read_csv('../../../data/acs5yr2018/psam_pusa.csv',col_types=colTypes)
dim(dat)
setdiff(varNames,names(dat))
setdiff(names(dat),varNames)

for(ll in c('b','c','d'))
  dat <- bind_rows(dat,
    read_csv(paste0('../../../data/acs5yr2018/psam_pus',ll,'.csv'),col_types=colTypes))
gc()

names(dat) <- tolower(names(dat))

print(mem_used())

#save(dat,file='fullDat.RData')

gc()

print(dim(dat))
fodCat <- read.csv('../generalCode/fieldOfDegree/fodCategories.csv')
dat$fodSmall <- fodCat$small[match(dat$fod1p,fodCat$num)]
dat$fodBig <- fodCat$big[match(dat$fod1p,fodCat$num)]
print(dim(dat))

### Taken from factor labels.R (mark bond):
indCode <- read.csv('../generalCode/naicsCodes.csv') ## copied from data dictionary
### get three-letter descriptions
indCode$ind2 <- substr(indCode$ind,1,3)

dat$industry <- indCode$ind2[match(dat$naicsp,indCode$code)]
#dat <- full_join(dat,indCode,by=c("naicsp"="code"))
#all.equal(dat$ind2,dat$industry)
#[1] TRUE
print(dim(dat))

dat$industry <- fct_recode(dat$industry,
"Agriculture"="AGR",
"Extraction"="EXT",
"Utilities"="UTL",
"Construction"="CON",
"Manufacturing"="MFG",
"Wholesale"="WHL",
"Retail"="RET",
"Transportation"="TRN",
"Information Services"="INF",
"Finance"="FIN",
"Professional Services"="PRF",
"Education"="EDU",
"Medical"="MED",
"Medical"="SCA",
"Entertainment"="ENT",
"Service Industry"="SRV",
"Government, Military, Administration"="ADM",
"Government, Military, Administration"="MIL")
dat$industry <- as.character(dat$industry)
### separate out travel/food/drink from ENT:
dat$industry[dat$naicsp%in%c('7211','721M','7224','722Z')] <- "Accommodation and Food Services"
dat$industry[dat$industry=='UNE'] <- NA

### aside: print out 3 most common industries in each classification
indCode$ind <- as.character(indCode$ind)
indCode$ind3 <- tolower(substr(indCode$ind, 5, nchar(indCode$ind)))

sink('common industries by category.txt')
for(ind in unique(na.omit(dat$industry))){
  cat(ind,'\n')
  ttt <- table(dat$naicsp[dat$industry==ind])
  for(i in 1:min(length(ttt),3)) cat('\t\t',indCode$ind3[indCode$code==names(ttt)[i]],'\n')
  cat('\n')
}
sink()

print(dim(dat))

dat$state <- states$abb[match(dat$st,states$x)]

print(dim(dat))

jobs$code <- jobs[,1]
jobs$job <- factor(substr(as.character(jobs$occupation),5,nchar(as.character(jobs$occupation))))
dat$occpN <- as.numeric(dat$occp)
dat <- full_join(dat,jobs,by=c("occpN"="code"))

print(dim(dat))


edlevs <- c(
    '<Grade 10',
    'Grade 10',
    'Grade 11',
    '12th grade - no diploma',
    'Regular high school diploma',
    'GED or alternative credential',
    'Some college, but less than 1 year',
    '1 or more years of college credit, no degree',
    'Associates degree',
    'Bachelors degree',
    'Masters degree',
    'Professional degree beyond a bachelors degree',
    'Doctorate degree')

dat$attain <- ifelse(dat$schl<13,1,dat$schl-11)
dat$attain <- factor(edlevs[dat$attain],levels=edlevs,ordered=TRUE)

print(mem_used())
print('filtering on age and relp')
dat <- dat%>%filter(agep>17,agep<65,relp!=16)%>% ## relp==16 for institutionalized
    mutate(
        selfCare=factor(ifelse(ddrs==1,'Self-Care Difficulty','No Self-Care Difficulty')),
        indLiv=factor(ifelse(dout==1,'Independent Living Difficulty','No Independent Living Difficulty')),
        amb=factor(ifelse(dphy==1,'Ambulatory Difficulty','No Ambulatory Difficulty')),
        cogDif=factor(ifelse(drem==1,'Cognitive Difficulty','No Cognitive Difficulty')),
        deaf=factor(ifelse(dear==1,'deaf','hearing')),
        Age=ordered(
          ifelse(agep<25,'18-24',
          ifelse(agep<35,'25-34',
            ifelse(agep<45,'35-44',
            ifelse(agep<55,'45-54','55-64'))))),
        attainCum=ordered(
            ifelse(attain<'Regular high school diploma','No HS',
            ifelse(attain<'Some college, but less than 1 year','HS Diploma',
            ifelse(attain<'Associates degree','Some College',
            ifelse(attain<'Bachelors degree','Associates',
              ifelse(attain<'Masters degree','Bachelors',
                ifelse(attain<'Doctorate degree','Masters/Professional','PhD')))))),
            levels=c('No HS','HS Diploma','Some College','Associates','Bachelors','Masters/Professional','PhD')),
        attainBA=factor(
          ifelse(attain<'Bachelors degree','Less than BA',
            ifelse(attain=='Bachelors degree','BA','>BA'))),
        employment=factor(ifelse(esr%in%c(1,2,4,5),'Employed',
                   ifelse(esr==3,'Unemployed','Not In Labor Force'))),

        fulltime=(employment=='Employed')&(wkw==1 & wkhp>=35),

        latinx=hisp>1,
        hispType=hispCats$hisp[hisp],
        hispType=fct_lump_min(hispType,500,w=ifelse((agep>24&deaf=='deaf'&latinx)|!latinx,1,0),other_level="All Other Spanish/Hispanic/Latino"),
        race=raceCats$race[rac1p],
        race=fct_collapse(race,AIAN=c("Alaska Native alone","Am Ind OR Al Nat","American Indian alone")),
        black=racblk==1,
        asian=racasn==1,
        AmIndAKNat=racaian==1,

        diss=ifelse(ddrs==1|deye==1|dout==1|dphy==1|drem==1,'disabled','nondisabled'),
        blind=ifelse(deye==1,'blind','seeing'),

        Sex=ifelse(sex==1,'Male','Female'),

        selfEmp=cow%in%(6:7),
        bizOwner=cow==7,

        Nativity=ifelse(nativity==1,'Native','Foreign born'),
        Language=ifelse(lanx==1,'UsesOtherLanguage','JustEnglish'),
        enrolled=ifelse(is.na(schg),'not enrolled','enrolled'),
        enrolledPS=!is.na(schg)&(schg>14),
        enrolledPro=!is.na(schg)&(schg==16)

      )
gc()
print(mem_used())



dat <- mutate(dat,
  race2=ifelse(
    black,"black+",
    ifelse(AmIndAKNat,'AIAN+',as.character(race))
  ),
  race2=fct_lump_min(race2,500,w=ifelse(agep>24&deaf=='deaf'&latinx,1,0),other_level="Other/Multi")
)

save(dat,file='data/attainmentEmploymentDataACS14-18.RData')
