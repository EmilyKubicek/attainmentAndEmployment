source('code/datCheck25.r')

### employment and fulltime and median wages
emp <- list()

gc()
emp$overall <- dat25%>%group_by(deaf,latinx)%>%group_modify(~as.data.frame(rbind(factorProps('employment',.))))

emp$overall <- dat25%>%filter(employment=='Employed')%>%group_by(deaf,latinx)%>%
  summarize(`% Fulltime (Emp)`=svmean(fulltime,pwgtp),pt=1-`% Fulltime (Emp)`,n.emp=n(),minAge=min(agep))%>%
  mutate_at(vars(`% Fulltime (Emp)`,pt),~.*100)%>%full_join(emp$overall,.)

emp$overall <- dat25%>%filter(fulltime)%>%
        group_by(deaf,latinx)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),n.ft=n(),minAge=min(agep))%>%full_join(emp$overall,.)

emp$hispType <-
  dat25%>%group_by(deaf,hispType)%>%group_modify(~as.data.frame(rbind(factorProps('employment',.))))

emp$hispType <- dat25%>%filter(employment=='Employed')%>%group_by(deaf,hispType)%>%
  summarize(`% Fulltime (Emp)`=svmean(fulltime,pwgtp),pt=1-`% Fulltime (Emp)`,n.emp=n(),minAge=min(agep))%>%
  mutate_at(vars(`% Fulltime (Emp)`,pt),~.*100)%>%full_join(emp$hispType,.)

emp$hispType <- dat25%>%filter(fulltime)%>%
        group_by(deaf,hispType)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),n.ft=n(),minAge=min(agep))%>%full_join(emp$hispType,.)


print('emp fulltime end')




for(vv in c('Age','Sex','Nativity','Language','race2'))
  emp[[paste0('by',capitalize(vv))]] <-
    full_join(
      dat25%>%
        group_by(deaf,latinx,!!sym(vv))%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp)*100,
          `% FT`=svmean(fulltime,pwgtp)*100,n=n(),minAge=min(agep)),
      dat25%>%filter(fulltime)%>%
        group_by(deaf,latinx,!!sym(vv))%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))
    )





empDis <- list()

empDis[['disabled']] <-
  full_join(
      dat25%>%
        group_by(deaf,latinx,diss)%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp)*100,
          `% FT`=svmean(fulltime,pwgtp)*100,n=n(),minAge=min(agep)),
      dat25%>%filter(fulltime)%>%
        group_by(deaf,latinx,diss)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))
    )


for(vv in c('ddrs','dout','dphy','drem','deye')){
  nm <- c(ddrs='selfCare',dout='indLiv',dphy='amb',drem='cogDif',deye='blind')[vv]
  empDis[[nm]] <-
    full_join(
      dat25%>%filter(deaf=='deaf',!!sym(vv)==1)%>%
        group_by(latinx)%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp)*100,
          `% FT`=svmean(fulltime,pwgtp)*100,n=n(),minAge=min(agep)),
      dat25%>%filter(fulltime,deaf=='deaf',!!sym(vv)==1)%>%
        group_by(latinx)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))
    )
}


for(dd in names(empDis)[-1]){
  empDis[[dd]] <- cbind(deaf='deaf',empDis[[dd]]$latinx,diss=dd,empDis[[dd]][,-1])
  names(empDis[[dd]]) <- names(empDis[[1]])
}
empDis[[1]] <- as.data.frame(empDis[[1]])

empDis <- do.call('rbind',empDis)

emp$byDisability <- empDis




### employment/earnings by ed level
empEd <- sapply(levels(dat25$attainCum)[-1],
  function(edLev)
    dat25%>%filter(attainCum>=edLev)%>%
      group_by(deaf,latinx)%>%
      mutate(emp=employment=='Employed')%>%
      summarize(`% Employed`=svmean(emp,pwgtp)*100,
        `% FT`=svmean(fulltime,pwgtp)*100,n=n(),minAge=min(agep)),
  simplify=FALSE)

empEd[['No HS']] <-
  dat25%>%filter(attainCum=='No HS')%>%
   group_by(deaf,latinx)%>%
   mutate(emp=employment=='Employed')%>%
   summarize(`% Employed`=svmean(emp,pwgtp)*100,
    `% FT`=svmean(fulltime,pwgtp)*100,n=n(),minAge=min(agep))

for(ee in names(empEd)) empEd[[ee]]$edLev <- ee

empEd <- do.call('rbind',empEd)

ernEd <- sapply(levels(dat25$attainCum)[-1],
  function(edLev)
    dat25%>%filter(fulltime, attainCum>=edLev)%>%
      group_by(deaf,latinx)%>%
      summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep)),
  simplify=FALSE)

ernEd[['No HS']] <-
   dat25%>%filter(fulltime, attainCum=='No HS')%>%
      group_by(deaf,latinx)%>%
      summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))

for(ee in names(ernEd)) ernEd[[ee]]$edLev <- ee

ernEd <- do.call('rbind',ernEd)

empEd <- full_join(empEd,ernEd)

empEd <- empEd%>%select(deaf,latinx,edLev,everything())

empEd <- rbind(filter(empEd,edLev=='No HS'), filter(empEd,edLev!='No HS'))

emp$byEducation <- empEd

emp <- map(emp,~mutate(.,`Med. Earn (FT)`=paste0('$',format(round(`Med. Earn (FT)`,-2),big.mark=','))))
emp <- map(emp, function(x) {
  for(nn in grep('%',names(x), fixed=TRUE,value=TRUE)) x[[nn]] <- paste0(round(x[[nn]],1),'%')
  x
  })

write.xlsx(emp,file='results/employment.xlsx')
