source('datCheck25.r')
### ssip
ssip <- dat25%>%group_by(deaf,blackORwhite)%>%
  mutate(ssip=ssip>0)%>%summarize(perSSIP=svmean(ssip,pwgtp)*100,n=n(),minAge=min(agep))

write.xlsx(ssip,'output/ssip.xlsx')


### business ownership self employment
bizOwn <- dat25%>%group_by(deaf,blackORwhite)%>%
  mutate(ssip=ssip>0)%>%
  summarize(
    perSelfEmp=svmean(selfEmp,pwgtp)*100,
    perOwnBiz=svmean(bizOwner,pwgtp)*100,
    n=n(),minAge=min(agep))

write.xlsx(bizOwn,'output/bizOwn.xlsx')

save(ern1,empEd,emp1,emp,ft,empRace,empDis,ssip,bizOwn,file='output/employment.RData')



### top 5 occupations
jd <- dat25%>%filter(deaf=='deaf',fulltime,blackORwhite=='Black')%>%select(job,pwgtp)
jh <- dat25%>%filter(deaf=='hearing',fulltime,blackORwhite=='Black')%>%select(job,pwgtp)
jobD <- sapply(levels(jd$job),
  function(j) svmean(eval(parse(text=paste0("job=='",j,"'")),jd),jd$pwgtp))
jobH <- sapply(levels(jh$job),
  function(j) svmean(eval(parse(text=paste0('job=="',j,'"')),jh),jh$pwgtp))

top5D <- sort(jobD*100,decreasing=TRUE)[1:5]
top5H <- sort(jobH*100,decreasing=TRUE)[1:5]

sink('output/top5jobs.txt')
cat('DEAF\n')
cat(paste(paste0(names(top5D),' ',round(top5D,1),'%'),collapse='\n'))
cat('\n\n\n')
cat('HEARING \n')
cat(paste(paste0(names(top5H),' ',round(top5H,1),'%'),collapse='\n'))
sink()


### by industry category
employmentByIndustry <- dat25%>%
  filter(fulltime)%>%
  bind_rows(mutate(.,blackORwhite='Overall'))%>%
  filter(blackORwhite!='Other')%>%
  group_by(blackORwhite,deaf)%>%
  group_map(~as.data.frame(rbind(factorProps('industry',.))))

raceTab <- xtabs(~raceEth+blackORwhite,data=dat25)
blackWith <- raceTab%>%as_tibble%>%
  filter(blackORwhite=='Black',raceEth!='African American',n>0)%>%
  pull(raceEth)
whiteWith <- raceTab%>%as_tibble%>%
  filter(blackORwhite=='White',raceEth!='White',n>0)%>%
  pull(raceEth)

ebiInfo <- data.frame(
  info=
    c('Years','Ages','Race/Ethnicity Definition:','Black','White','Industry Code Definition','total n'),
  what=
    c('2013-2017',
      paste(min(dat25$agep),'-',max(dat25$agep)),
      '',
      paste('Includes Black/African American alone',
        if(length(blackWith)) paste('Black',blackWith,collapse=', ') else NULL,collapse=','),
       paste('Includes White alone',
         if(length(whiteWith)) paste('White',whiteWith,collapse=', ') else NULL,collapse=','),
      'Industry codes represent a compromise between the NAICS sectors and the abbreviations in the ACS codebook. In practice, they are the NAICS sectors, with "Finance  and  Insurance" and "Real  Estate  and  Rental  and  Leasing" combined, and "Professional, Scientific, and Technical Services", "Management of Companies and Enterprises", and "Administrative  and  Support  and  Waste  Management  and  Remediation  Services" combined into one category'))





employmentByIndustryOverall <- dat25%>%filter(fulltime)%>%
  group_by(deaf)%>%do(x=factorProps('industry',.))
## write results to spreadsheet

employmentByIndustry <- setNames(do.call('data.frame',employmentByIndustry$x),employmentByIndustry$deaf)
employmentByIndustryOverall <- setNames(do.call('data.frame',employmentByIndustryOverall$x),employmentByIndustryOverall$deaf)



openxlsx::write.xlsx(ebi,
  'output/BlackIndustryPercentagesFT2012-17.xlsx', rowNames=TRUE,colWidths='auto')

