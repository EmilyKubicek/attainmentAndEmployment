source('code/datCheck18.r')

overallEnr <- dat18%>%group_by(deaf)%>%group_modify(~as.data.frame(rbind(factorProps('enrolled',.,cum=FALSE))))
latinxEnr <- dat18%>%group_by(deaf,latinx)%>%group_modify(~as.data.frame(rbind(factorProps('enrolled',.,cum=FALSE))))
hispType <- dat18%>%group_by(deaf,hispType)%>%group_modify(~as.data.frame(rbind(factorProps('enrolled',.,cum=FALSE))))
raceGenderEnr <- dat18%>%group_by(deaf,latinx,Sex)%>%
  group_modify(~as.data.frame(rbind(factorProps('enrolled',.,cum=FALSE))))


enr <- bind_rows(overallEnr,latinxEnr,hispType,raceGenderEnr)%>%
  select(deaf,latinx,hispType,Sex,everything())%>%
  mutate(latinx=ifelse(latinx,'Latinx','Not Latinx'))%>%
  ungroup()%>%
  add_case(deaf='Ages 18-64')
## SEX         Character   1
## Sex
##             1    .Male
##             2    .Female

openxlsx::write.xlsx(enr,'results/enrollment.xlsx')
