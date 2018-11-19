library(gridExtra)
library(ggplot2)
library(cluster)
library(readr) ## read in the csvs faster
library(survey)
library(dplyr)
library(openxlsx)
states <- read.csv('../../../data/states.csv')

#1) a simple breakdown of current enrollment, and completion data, across type of institution (4 year colleges, community colleges, etc) using all the 'type of institution' data we have, so that would give us some nice descriptives and allow us to make a final decision on how we want to categorize 'community colleges and 2-year institutions'




varNames <- c('ST','AGEP','DDRS','DEAR','DEYE','DOUT','DPHY','DRATX','DREM','FDEARP','ESR','SCHL','RAC1P','HISP','SEX','PERNP','PINCP','SSIP','WKHP','WKW','ADJINC','PWGTP','RELP',paste0('PWGTP',1:80))

firstTry <- read_csv(paste0('../../../data/byYear/ss17pusa.csv'), n_max=5)
colTypes <- ifelse(names(firstTry)%in%varNames,'i','-')
missingVars <- setdiff(varNames,names(firstTry)[colTypes=='i'])
if(length(missingVars)) cat('WARNING: Missing these variables:\n',missingVars,'\n')

colTypes <- paste(colTypes,collapse='')


datA <- read_csv('../../../data/byYear/ss17pusa.csv',col_types=colTypes)
datB <- read_csv('../../../data/byYear/ss17pusb.csv',col_types=colTypes)
## need: DEAR, attain, employment,PERNP, fulltime
dat <- rbind(datA[,varNames],datB[,varNames])

rm(datA,datB); gc()

names(dat) <- tolower(names(dat))

dat$state <- states$abb[match(dat$st,states$x)]


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



dat <- dat%>%filter(agep>24,agep<65,relp!=16)%>% ## relp==16 for institutionalized
    mutate(
        deaf=ifelse(dear==1,1,0),
        Age=ifelse(agep<35,'25-34',
            ifelse(agep<45,'35-44',
            ifelse(agep<55,'45-54','55-64'))),
        attainCum=ifelse(attain<'Regular high school diploma','No HS',
                  ifelse(attain<'Some College, but less than 1 year','HS Diploma',
                  ifelse(attain<'Associates degree','Some College',
                  ifelse(attain<'Bachelors degree','Associates',
                  ifelse(attain<'Masters degree','Bachelors','Post-Graduate'))))),
        employment=ifelse(esr%in%c(1,2,4,5),'Employed',
                   ifelse(esr==3,'Unemployed','Not In Labor Force')),

        fulltime=(employment=='Employed')&(wkw==1 & wkhp>=35),

        raceEth=ifelse(hisp>1,"Hispanic",
                ifelse(rac1p==2,"African American",
                ifelse(rac1p==6| rac1p==7,"Asian/PacIsl",
                ifelse(rac1p%in%c(3,4,5),'American Indian',
                ifelse(rac1p==1,"White","Other"))))),

        diss=ifelse(ddrs==1|deye==1|dout==1|dphy==1|(!is.na(dratx)&dratx==1)|drem==1,1,0),
        blind=ifelse(deye==1,1,0),

        sex=ifelse(sex==1,'Male','Female'))



