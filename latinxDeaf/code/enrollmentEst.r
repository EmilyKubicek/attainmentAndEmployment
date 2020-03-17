source('datCheck18.r')

overallEnr <- dat18%>%group_by(deaf)%>%do(x=factorProps('enrolled',.,cum=FALSE))
raceEnr <- dat18%>%filter(blackORwhite!='Other')%>%group_by(deaf,blackORwhite)%>%
  do(x=factorProps('enrolled',.,cum=FALSE))
raceGenderEnr <- dat18%>%filter(blackORwhite!='Other')%>%group_by(deaf,blackORwhite,sex)%>%
  do(x=factorProps('enrolled',.,cum=FALSE))

enr <- bind_rows(overallEnr,raceEnr,raceGenderEnr)
## SEX         Character   1
## Sex
##             1    .Male
##             2    .Female
enr$sex <- c('Male','Female')[enr$sex]
write.xlsx(ff(enr),'output/enrollment.xlsx')
