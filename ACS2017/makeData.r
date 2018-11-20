library(readr) ## read in the csvs faster
library(dplyr)
library(openxlsx)
states <- read.csv('../../../data/states.csv')

#1) a simple breakdown of current enrollment, and completion data, across type of institution (4 year colleges, community colleges, etc) using all the 'type of institution' data we have, so that would give us some nice descriptives and allow us to make a final decision on how we want to categorize 'community colleges and 2-year institutions'




varNames <- c('ST','AGEP','DDRS','DEAR','DEYE','DOUT','DPHY','DRATX','DREM','FDEARP','ESR','SCHL','RAC1P','HISP','SEX','PERNP','PINCP','SSIP','WKHP','WKW','ADJINC','PWGTP','RELP','FOD1P','NAICSP',paste0('PWGTP',1:80))

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

#### from mark bond's code
dat$degree <- ifelse(dat$FOD1P < 1200, "Agriculture", NA)
dat$degree <- ifelse(dat$FOD1P > 1300 & dat$FOD1P < 1400, "Environmental Science", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 1400 & dat$FOD1P < 1500, "Architecture", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 1500 & dat$FOD1P < 1600, "Ethnic studies", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 1900 & dat$FOD1P < 2000, "Journalism, Communications, PR", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 2000 & dat$FOD1P < 2100, "Communication Technologies", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 2099 & dat$FOD1P < 2200, "Computer Science", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 2200 & dat$FOD1P < 2300, "Cosmetology/Culinary Arts", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 2299 & dat$FOD1P < 2400, "Education", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 2399 & dat$FOD1P < 2600, "Engineering", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 2599 & dat$FOD1P < 2700, "Foreign Language", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 2900 & dat$FOD1P < 3000, "Family and Consumer Sciences", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 3200 & dat$FOD1P < 3300, "Pre-Law and Court Reporting", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 3400 & dat$FOD1P < 3500, "Liberal Arts", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 3499 & dat$FOD1P < 3600, "Library Science", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 3599 & dat$FOD1P < 3700, "Biology", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 3699 & dat$FOD1P < 3800, "Mathematics", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 3800 & dat$FOD1P < 3900, "Military Technologies", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 4000 & dat$FOD1P < 4100, "Interdisciplinary studies", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 4100 & dat$FOD1P < 4200, "Physical fitness, parks, recreation", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 4800 & dat$FOD1P < 4900, "Philosophy", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 4900 & dat$FOD1P < 5000, "Theology", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 5000 & dat$FOD1P < 5200, "Physial Sciences", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 5199 & dat$FOD1P < 5300, "Psychology", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 5300 & dat$FOD1P < 5400, "Criminal Justice", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 5400 & dat$FOD1P < 5500, "Public Policy, social work", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 5499 & dat$FOD1P < 5600, "Social Sciences", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 5600 & dat$FOD1P < 5700, "Construction Services", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 5700 & dat$FOD1P < 5800, "Electrical, Mechanical, and Precision Technologies, Production", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 5900 & dat$FOD1P < 6000, "Transportation science", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 5999 & dat$FOD1P < 6100, "Fine Arts", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 6099 & dat$FOD1P < 6200, "Medical/Health Services", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 6199 & dat$FOD1P < 6300, "Business", dat$degree)
dat$degree <- ifelse(dat$FOD1P > 6400, "History", dat$degree)
dat$degree <- factor(dat$degree)

dat$industryCode <- NA
dat$x <- substring(dat$NAICSP, first = 1, last=2)
dat$industryCode <- ifelse(dat$x == "11", "Agriculture", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "21", "Extraction", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "22", "Utilities", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "23", "Construction", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "31", "Manufacturing", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "32", "Manufacturing", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "33", "Manufacturing", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "3M", "Manufacturing", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "42", "Wholesale", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "44", "Retail", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "45", "Retail", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "4M", "Retail", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "48", "Transportation", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "49", "Transportation", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "51", "Information services", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "52", "Finance", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "53", "Finance", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "54", "Professional services", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "55", "Professional services", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "56", "Professional services", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "61", "Education", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "62", "Medical", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "71", "Entertainment", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "72", "Entertainment", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "81", "Service", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "92", "GOV/MIL/ADM", dat$industryCode)
dat$industryCode <- ifelse(dat$x == "99", "Unemployed", dat$industryCode)


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
        selfCare=factor(ifelse(ddrs==1,'Self-Care Difficulty','No Self-Care Difficulty')),
        indLiv=factor(ifelse(dout==1,'Independent Living Difficulty','No Independent Living Difficulty')),
        amb=factor(ifelse(dphy==1,'Ambulatory Difficulty','No Ambulatory Difficulty')),
        servDis=factor(ifelse(is.na(dratx),'Not Vet',
                       ifelse(dratx==1,'Vet. Service Connected Disability','No Service Connected Disability'))),
        cogDif=factor(ifelse(drem==1,'Cognitive Difficulty','No Cognitive Difficulty')),
        deaf=factor(ifelse(dear==1,'deaf','hearing')),
        Age=ordered(ifelse(agep<35,'25-34',
            ifelse(agep<45,'35-44',
            ifelse(agep<55,'45-54','55-64')))),
        attainCum=ordered(
            ifelse(attain<'Regular high school diploma','No HS',
            ifelse(attain<'Some college, but less than 1 year','HS Diploma',
            ifelse(attain<'Associates degree','Some College',
            ifelse(attain<'Bachelors degree','Associates',
            ifelse(attain<'Masters degree','Bachelors','Post-Graduate'))))),
           levels=c('No HS','HS Diploma','Some College','Associates','Bachelors','Post-Graduate')),
        employment=factor(ifelse(esr%in%c(1,2,4,5),'Employed',
                   ifelse(esr==3,'Unemployed','Not In Labor Force'))),

        fulltime=(employment=='Employed')&(wkw==1 & wkhp>=35),

        raceEth=ifelse(hisp>1,"Hispanic",
                ifelse(rac1p==2,"African American",
                ifelse(rac1p==6| rac1p==7,"Asian/PacIsl",
                ifelse(rac1p%in%c(3,4,5),'American Indian',
                ifelse(rac1p==1,"White","Other"))))),

        diss=ifelse(ddrs==1|deye==1|dout==1|dphy==1|(!is.na(dratx)&dratx==1)|drem==1,'disabled','nondisabled'),
        blind=ifelse(deye==1,'blind','seeing'),

        sex=ifelse(sex==1,'Male','Female'))



save(dat,file='attainmentEmploymentData.RData')
