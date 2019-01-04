library(readr) ## read in the csvs faster
library(dplyr)
library(openxlsx)
states <- read.csv('../../../data/states.csv')

#1) a simple breakdown of current enrollment, and completion data, across type of institution (4 year colleges, community colleges, etc) using all the 'type of institution' data we have, so that would give us some nice descriptives and allow us to make a final decision on how we want to categorize 'community colleges and 2-year institutions'




varNames <- c('ST','AGEP','DDRS','DEAR','DEYE','DOUT','DPHY','DRATX','DREM','FDEARP','ESR','SCHL','RAC1P','HISP','SEX','PERNP','PINCP','SSIP','WKHP','WKW','ADJINC','PWGTP','RELP','FOD1P','NAICSP','OCCP','INDP',paste0('PWGTP',1:80))

firstTry <- read_csv(paste0('../../../data/byYear/ss17pusa.csv'), n_max=5)
colTypes <- ifelse(names(firstTry)%in%varNames,
                   ifelse(names(firstTry)%in%c('NAICSP','FOD1P','OCCP','INDP'),'c','i'),'-')
missingVars <- setdiff(varNames,names(firstTry)[colTypes%in%c('c','i')])
if(length(missingVars)) cat('WARNING: Missing these variables:\n',missingVars,'\n')

colTypes <- paste(colTypes,collapse='')


datA <- read_csv('../../../data/byYear/ss17pusa.csv',col_types=colTypes)
datB <- read_csv('../../../data/byYear/ss17pusb.csv',col_types=colTypes)
## need: DEAR, attain, employment,PERNP, fulltime
dat <- rbind(datA[,varNames],datB[,varNames])

rm(datA,datB); gc()


names(dat) <- tolower(names(dat))

fodCat <- read.csv('../generalCode/fieldOfDegree/fodCategories.csv')
dat$fodSmall <- fodCat$small[match(dat$fod1p,fodCat$num)]
dat$fodBig <- fodCat$big[match(dat$fod1p,fodCat$num)]


## ### step 1: download census-2012-final-code-list.xls
## ### from https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/census-2012-final-code-list.xls

## ### step 2: save as .xlsx file

## codes <- read.xlsx('../census-2012-final-code-list.xlsx',startRow=5)
## codes <- codes[is.na(codes$X1)|codes$X1!="and Waste Management Services",]
## names(codes) <- c('X1','desc','code1','code2')
## ccc <- subset(codes,!is.na(X1)|grepl('-',code1))
## newcode <- NULL
## for(i in 1:(nrow(ccc)-1)){
##   if(is.na(ccc$X1[i])){
##     newcode <- rbind(newcode,setNames(ccc[i,c('desc','code1')],c('ind','code')))
##   } else if(!is.na(ccc$X1[i+1])){
##     newcode <- rbind(newcode,setNames(ccc[i,c('X1','code1')],c('ind','code')))
##   }
## }
## newcode <- rbind(newcode,setNames(ccc[nrow(ccc),c('X1','code1')],c('ind','code')))
## codesplit <- strsplit(newcode$code,'-')
## newcode$code1 <- sapply(codesplit,function(x) x[1])
## newcode$code2 <- sapply(codesplit,function(x) x[length(x)])
## newcode$ind2 <- c('Agriculture','Extraction','Construction','Manufacturing','Wholesale','Retail','Transportation','Utilities','Information Services','Finance','Finance','Professional Services','Government, Military, Administration','Education','Medical','Entertainment','Service Industry','Service Industry','Government, Military, Administration','Government, Military, Administration')

## dat$ind1 <- NA
## for(i in 1:nrow(newcode)){
##   print(i)
##   dat$ind1[dat$indp>=newcode$code1[i]&dat$indp<=newcode$code2[i]] <- newcode$ind2[i]
## }
## dat$ind1[dat$indp==7570] <- 'Professional Services'


### Taken from factor labels.R (mark bond):
dat$industry <- NA
dat$x <- substring(dat$naicsp, first = 1, last=2)
dat$industry <- ifelse(dat$x == "11", "Agriculture", dat$industry)
dat$industry <- ifelse(dat$x == "21", "Extraction", dat$industry)
dat$industry <- ifelse(dat$x == "22", "Utilities", dat$industry)
dat$industry <- ifelse(dat$x == "23", "Construction", dat$industry)
dat$industry <- ifelse(dat$x == "31", "Manufacturing", dat$industry)
dat$industry <- ifelse(dat$x == "32", "Manufacturing", dat$industry)
dat$industry <- ifelse(dat$x == "33", "Manufacturing", dat$industry)
dat$industry <- ifelse(dat$x == "3M", "Manufacturing", dat$industry)
dat$industry <- ifelse(dat$x == "42", "Wholesale", dat$industry)
dat$industry <- ifelse(dat$x == "44", "Retail", dat$industry)
dat$industry <- ifelse(dat$x == "45", "Retail", dat$industry)
dat$industry <- ifelse(dat$x == "4M", "Retail", dat$industry)
dat$industry <- ifelse(dat$x == "48", "Transportation", dat$industry)
dat$industry <- ifelse(dat$x == "49", "Transportation", dat$industry)
dat$industry <- ifelse(dat$x == "51", "Information services", dat$industry)
dat$industry <- ifelse(dat$x == "52", "Finance", dat$industry)
dat$industry <- ifelse(dat$x == "53", "Finance", dat$industry)
dat$industry <- ifelse(dat$x == "54", "Professional services", dat$industry)
dat$industry <- ifelse(dat$x == "55", "Professional services", dat$industry)
dat$industry <- ifelse(dat$x == "56", "Professional services", dat$industry)
dat$industry <- ifelse(dat$x == "61", "Education", dat$industry)
dat$industry <- ifelse(dat$x == "62", "Medical", dat$industry)
dat$industry <- ifelse(dat$x == "71", "Entertainment", dat$industry)
dat$industry <- ifelse(dat$x == "72", "Entertainment", dat$industry)
dat$industry <- ifelse(dat$x == "81", "Service", dat$industry)
dat$industry <- ifelse(dat$x == "92", "GOV/MIL/ADM", dat$industry)
dat$industry <- ifelse(dat$x == "99", "Unemployed", dat$industry)


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



save(dat,file='attainmentEmploymentDataACS17.RData')
