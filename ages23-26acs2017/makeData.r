library(readr) ## read in the csvs faster
library(dplyr)
library(openxlsx)
states <- read.csv('../generalCode/states.csv')
jobs <- read.csv('../generalCode/occupations.csv')

 ## was wondering if you could pull the 2017 ACS data for this age group (ages 23-26). My questions are:

 ##    educational attainment
 ##    employment, unemployed, not in the labor force
 ##    median income (is this household or individual, I'm not remembering . I'm looking for individual if possible)
 ##    collecting social security
 ##    live without a parent (is that possible?)


varNames <- c('SERIALNO','ST','AGEP','DDRS','DEAR','DEYE','DOUT','DPHY','DRATX','DREM','FDEARP','ESR','SCHL','SCH','SCHG','RAC1P','HISP','SEX','PERNP','PINCP','SSP','SSIP','WKHP','WKW','ADJINC','PWGTP','RELP','FOD1P','NAICSP','OCCP','INDP','COW',paste0('PWGTP',1:80))


ctypes <- rep('i',length(varNames))
names(ctypes) <- varNames
ctypes[c('SERIALNO','NAICSP','FOD1P','OCCP','INDP')] <- 'c'
ctypes$.default <- '_'
colTypes <- do.call('cols',as.list(ctypes))


datA <- read_csv('../../../data/byYear/ss17pusa.csv',col_types=colTypes)
datB <- read_csv('../../../data/byYear/ss17pusb.csv',col_types=colTypes)
## need: DEAR, attain, employment,PERNP, fulltime
dat <- rbind(datA[,varNames],datB[,varNames])

rm(datA,datB); gc()

names(dat) <- tolower(names(dat))



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


 ##    educational attainment
 ##    employment, unemployed, not in the labor force
 ##    median income (is this household or individual, I'm not remembering . I'm looking for individual if possible)
 ##    collecting social security
 ##    live without a parent (is that possible?)
 ## oh... and % of currently enrolled students (postsecondary)
dat <- dat%>%filter(agep>22,agep<27,relp!=16)%>% ## relp==16 for institutionalized
    mutate(
         deaf=factor(ifelse(dear==1,'deaf','hearing')),
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
        ## relp:
        ## 02     .Biological son or daughter
        ##   03     .Adopted son or daughter
        ##   04     .Stepson or stepdaughter
        ##  07     .Grandchild
        ##   14     .Foster child
        dontLiveWparent=!relp%in%c(2:4,7,14), ## this assumes the parent is filling out the survey
        ss=ssp>0,
        ssi=ssip>0,
        Sex=ifelse(sex==1,'Male','Female'),
        enrolled=sch>1,
        ## schg:
        ## 14     .Grade 12
        ## 15     .College undergraduate years (freshman to senior)
        ## 16     .Graduate or professional school beyond a bachelor's degree
        enrolledPS=enrolled&schg>14
      )

print(xtabs(~agep+deaf,data=dat))
print(xtabs(~attainCum,data=dat))

save(dat,file='ages23-26ACS17.RData')
