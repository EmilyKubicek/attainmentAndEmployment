

tab1 <- rbind(
  attainment1$NationalAverage[[1]][[1]],
  do.call('rbind',attainment1$overall$x))

tab1 <- cbind(race=c('National Average',attainment1$overall$blackORwhite),
  deaf=c('',as.character(attainment1$overall$deaf)),
  as.data.frame(round(tab1,1)))

tab1 <- tab1%>%
  select(-ends_with('SE',ignore.case=FALSE),-contains('No HS'))%>%
  arrange(match(race,c('National Average','Black','White')))%>%
  rename( HS="% >=HS Diploma","Some College"="% >=Some College",AA="% >=Associates",
    BA= "% >=Bachelors",MA="% >=Masters/Professional",PhD="% PhD")

tab2 <- as.data.frame(do.call('rbind',attainment1$byAge$x))
tab2 <- cbind(race=attainment1$byAge$blackORwhite,
  deaf=attainment1$byAge$deaf,
  age=attainment1$byAge$Age,
  tab2)
tab2 <- tab2%>%
  select(-ends_with('SE',ignore.case=FALSE),-contains('No HS'))%>%
  rename( HS="% >=HS Diploma","Some College"="% >=Some College",AA="% >=Associates",
      BA= "% >=Bachelors",MA="% >=Masters/Professional",PhD="% PhD")%>%
  arrange(race)%>%
  melt()%>%
  dcast(race+deaf+variable~age)%>% # so that age categories are columns
  select(Attainment=variable,race,deaf,everything())%>%
  arrange(Attainment,race,deaf)

lilTab1 <- function(ss){
  tab <- as.data.frame(do.call('rbind',ss$x))
  tab%>%select(-ends_with('SE',ignore.case=FALSE),-contains('No HS'))%>%
    rename( HS="% >=HS Diploma","Some College"="% >=Some College",AA="% >=Associates",
      BA= "% >=Bachelors",MA="% >=Masters/Professional",PhD="% PhD")
}

lilTab <- function(varb){
  nn <- paste0('by',toupper(substr(varb,1,1)),substr(varb,2,nchar(varb)))
  tab <- lilTab1(attainment1[[nn]])%>%
    mutate(race=attainment1[[nn]]$blackORwhite,
      deaf=attainment1[[nn]]$deaf,
      vvv=attainment1[[nn]][[varb]])%>%
    arrange(race,vvv)%>%
    select(race,vvv,deaf,everything())

  #colnames(tab)[colnames(tab)=='vvv'] <- varb

  tab
}

tab3 <- lilTab('Sex')

sraceTab <- function(nn)
  lilTab1(attainment1[[nn]])%>%
    mutate(race=nn,vvv=NA,deaf=attainment1[[nn]]$deaf)%>%
    select(race,vvv,deaf,everything())


tab4 <- rbind(lilTab('nativity'),lilTab('lanx'),
  lilTab1(attainment1$blackMulti)%>%
    mutate(race=attainment1$blackMulti$blackMulti, vvv=NA,deaf=attainment1$blackMulti$deaf)%>%
    arrange(race)%>%select(race,vvv,deaf,everything()),
  sraceTab('blackLatinx'),sraceTab('blackAsian'),sraceTab('blackWhite')
  )


disTab <- function(ss)
  ss%>%filter(deaf=='deaf')%>%
    lilTab1()%>%
    mutate(race=filter(ss,deaf=='deaf')$blackORwhite,diss=filter(ss,deaf=='deaf')[[3]])%>%
    select(diss,race,everything())%>%
    filter(diss%in%grep('No|seeing',ss[[3]],value=TRUE,invert=TRUE))


tab5 <- do.call('rbind',lapply(attainment1$byDiss,disTab))%>%
  mutate(diss=ifelse(diss%in%c('disabled','blind'),paste0('Deaf',diss),
    ifelse(diss=='nondisabled',paste('Deaf',diss),paste0('Deaf+',diss))))%>%
  arrange(match(diss,unique(diss)))


openxlsx::write.xlsx(list(overall=tab1,byAge=tab2,bySex=tab3,subGroups=tab4,byDisability=tab5),
  'output/attainment.xlsx',row.names=TRUE)


## FOD table
tab6 <- do.call('cbind',lapply(fod$x, function(y) y[-grep(' SE',names(y))]))
colnames(tab6) <- paste(fod$deaf,fod$blackORwhite)

openxlsx::write.xlsx(tab6,'output/fieldOfDegreePercent.xlsx',row.names=TRUE)

