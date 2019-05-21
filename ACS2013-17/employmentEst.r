source('datCheck25.r')

### employment and fulltime and median wages
emp <- list()

gc()
emp1 <- dat25%>%group_by(deaf,blackORwhite)%>%do(x=factorProps('employment',.))

emp$overall <- ff(emp1)

ft <- dat25%>%filter(employment=='Employed')%>%group_by(deaf,blackORwhite)%>%
  summarize(ft=svmean(fulltime,pwgtp),pt=1-ft,n=n(),minAge=min(agep))

ft$ft <- ft$ft*100
ft$pt <- ft$pt*100

openxlsx::write.xlsx(ft,'output/fulltimePercentage.xlsx')

ern1 <- dat25%>%filter(fulltime)%>%
        group_by(deaf,blackORwhite)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),n=n(),minAge=min(agep))

write.xlsx(ern1,'output/FTearningsOverall.xlsx')

print('emp fulltime end')




for(vv in c('Age','Sex','nativity','lanx'))
  emp[[paste0('by',capitalize(vv))]] <-
    full_join(
      dat25%>%filter(blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,!!sym(vv))%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp)*100,
          `% FT`=svmean(fulltime,pwgtp)*100,n=n(),minAge=min(agep)),
      dat25%>%filter(fulltime,blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,!!sym(vv))%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))
    )


empRace <- dat25%>%filter(deaf=='deaf')%>%
  mutate(emp=employment=='Employed')%>%
  group_by(blackMulti)%>%
        summarize(`% Employed`=svmean(emp,pwgtp)*100,
          `% FT`=svmean(fulltime,pwgtp)*100,
          `Med. Earn (FT)`=med1(pernp[fulltime],pwgtp[fulltime],se=FALSE),
          n=n(),minAge=min(agep))

for(rr in paste0('black',c('Latinx','Asian','ANDwhite')))
  empRace <- bind_rows(empRace,
    dat25%>%filter(deaf=='deaf',!!sym(rr)==1)%>%
      mutate(emp=employment=='Employed')%>%
      summarize(`% Employed`=svmean(emp,pwgtp)*100,
          `% FT`=svmean(fulltime,pwgtp)*100,
          `Med. Earn (FT)`=med1(pernp[fulltime],pwgtp[fulltime],se=FALSE),
          n=n(),minAge=min(agep))%>%
      mutate(blackMulti=rr))


emp$byRace <- empRace



empDis <- list()

empDis[['disabled']] <-
  full_join(
      dat25%>%filter(blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,diss)%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp)*100,
          `% FT`=svmean(fulltime,pwgtp)*100,n=n(),minAge=min(agep)),
      dat25%>%filter(fulltime,blackORwhite!='Other')%>%
        group_by(deaf,blackORwhite,diss)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))
    )


for(vv in c('ddrs','dout','dphy','drem','deye')){
  nm <- c(ddrs='selfCare',dout='indLiv',dphy='amb',drem='cogDif',deye='blind')[vv]
  empDis[[nm]] <-
    full_join(
      dat25%>%filter(blackORwhite!='Other',deaf=='deaf',!!sym(vv)==1)%>%
        group_by(blackORwhite)%>%
        mutate(emp=employment=='Employed')%>%
        summarize(`% Employed`=svmean(emp,pwgtp)*100,
          `% FT`=svmean(fulltime,pwgtp)*100,n=n(),minAge=min(agep)),
      dat25%>%filter(fulltime,blackORwhite!='Other',deaf=='deaf',!!sym(vv)==1)%>%
        group_by(blackORwhite)%>%
        summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))
    )
}


for(dd in names(empDis)[-1]){
  empDis[[dd]] <- cbind(deaf='deaf',empDis[[dd]]$blackORwhite,diss=dd,empDis[[dd]][,-1])
  names(empDis[[dd]]) <- names(empDis[[1]])
}
empDis[[1]] <- as.data.frame(empDis[[1]])

empDis <- do.call('rbind',empDis)

emp$byDisability <- empDis




### employment/earnings by ed level
empEd <- sapply(levels(dat25$attainCum)[-1],
  function(edLev)
    dat25%>%filter(blackORwhite!='Other',attainCum>=edLev)%>%
      group_by(deaf,blackORwhite)%>%
      mutate(emp=employment=='Employed')%>%
      summarize(`% Employed`=svmean(emp,pwgtp)*100,
        `% FT`=svmean(fulltime,pwgtp)*100,n=n(),minAge=min(agep)),
  simplify=FALSE)

empEd[['No HS']] <-
  dat25%>%filter(blackORwhite!='Other',attainCum=='No HS')%>%
   group_by(deaf,blackORwhite)%>%
   mutate(emp=employment=='Employed')%>%
   summarize(`% Employed`=svmean(emp,pwgtp)*100,
    `% FT`=svmean(fulltime,pwgtp)*100,n=n(),minAge=min(agep))

for(ee in names(empEd)) empEd[[ee]]$edLev <- ee

empEd <- do.call('rbind',empEd)

ernEd <- sapply(levels(dat25$attainCum)[-1],
  function(edLev)
    dat25%>%filter(fulltime, blackORwhite!='Other',attainCum>=edLev)%>%
      group_by(deaf,blackORwhite)%>%
      summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep)),
  simplify=FALSE)

ernEd[['No HS']] <-
   dat25%>%filter(fulltime, blackORwhite!='Other',attainCum=='No HS')%>%
      group_by(deaf,blackORwhite)%>%
      summarize(`Med. Earn (FT)`=med1(pernp,pwgtp,se=FALSE),nFT=n(),minAge=min(agep))

for(ee in names(ernEd)) ernEd[[ee]]$edLev <- ee

ernEd <- do.call('rbind',ernEd)

empEd <- full_join(empEd,ernEd)

empEd <- empEd%>%select(deaf,blackORwhite,edLev,everything())

empEd <- rbind(filter(empEd,edLev=='No HS'), filter(empEd,edLev!='No HS'))

emp$byEducation <- empEd


write.xlsx(emp,file='output/employment.xlsx')
