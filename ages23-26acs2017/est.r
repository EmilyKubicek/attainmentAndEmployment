library(tidyverse)
library(reshape2)
source('../generalCode/estimationFunctions.r')
source('../generalCode/median.r')
##    educational attainment
 ##    employment, unemployed, not in the labor force
 ##    median income (is this household or individual, I'm not remembering . I'm looking for individual if possible)
 ##    collecting social security
 ##    live without a parent (is that possible?)
 ## oh... and % of currently enrolled students (postsecondary)



results <- dat%>%group_by(deaf)%>%
  group_map(~as.data.frame(rbind(
    c(factorProps('attainCum',.x),
      factorProps('employment',.x),
      incomeFT=med(~pincp,filter(.x,fulltime)),
      receivesSocialSecurity=estSEstr('ss',sdat=.x)*100,
      dontLiveWithParent=estSEstr('dontLiveWparent',sdat=.x)*100,
      enrolled=estSEstr('enrolled',sdat=.x)*100,
      enrolledPS=estSEstr('enrolledPS',sdat=.x)*100))))%>%
  melt(id.vars='deaf')%>%dcast(variable~deaf)%>%
  filter(!grepl('n[1-9]',variable),!grepl('\\.n$',variable))%>%
  arrange(variable=='n')


openxlsx::write.xlsx(results,'ages23-26.xlsx')
