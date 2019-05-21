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
employmentByIndustry <- dat25%>%filter(fulltime,blackORwhite=='Black')%>%
  group_by(deaf)%>%do(x=factorProps('industry',.))
## write results to spreadsheet

employmentByIndustry <- cbind(employmentByIndustry[,-which(names(employmentByIndustry)=='x')],
  do.call('rbind',employmentByIndustry$x))

ebi <- t(employmentByIndustry[,-grep(' SE',names(employmentByIndustry),fixed=TRUE)])

openxlsx::write.xlsx(ebi,
  'output/BlackIndustryPercentagesFT2012-17.xlsx', rowNames=TRUE,colWidths='auto')

