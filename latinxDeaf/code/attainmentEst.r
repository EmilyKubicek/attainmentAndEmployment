source('code/datCheck25.r')

capsRight <- function(x,tab)
  if(x%in%tab) x else tab[tolower(tab)==tolower(x)]

noby <- function(x,tab) capsRight(substr(x,3,nchar(x)),tab)

fun1 <- function(grp,data){
    if(substr(grp,1,2)=='by') grp <- noby(grp,names(data))
    data%>%group_by(deaf,latinx,!!sym(grp))%>%group_modify(~as.data.frame(rbind(FUN(x,.))))
}


dat25$deaf <- as.character(dat25$deaf)

attainment1 <-
  bind_rows(
    tibble(table='National Average',deaf=NA,latinx=NA,subgroup=NA),
    as.data.frame(rbind(factorProps('attainCum',dat25))),
    tibble(table='Overall By Latinx'),
    dat25%>%group_by(deaf,latinx)%>%group_modify(~as.data.frame(rbind(factorProps('attainCum',.)))),
    tibble(table='By Latinx Subgroups'),
    dat25%>%group_by(deaf,hispType)%>%group_modify(~as.data.frame(rbind(factorProps('attainCum',.))))%>%rename(subgroup=hispType)
  )

for(ggg in c('race2','Age','Sex','Nativity','Language','diss','blind','selfCare','indLiv','amb','cogDif'))
  attainment1 <- attainment1%>%
    bind_rows(
      tibble(table=paste('By',ggg)),
      fun1(ggg,dat25)%>%rename(subgroup=!!ggg)
    )


attainment1 <- mutate(attainment1,latinx=ifelse(latinx,'Latinx','Not Latinx'))

##### Field of degree
fod <- dat25%>%filter(attainCum>'Associates')%>%group_by(deaf,latinx)%>%
  group_modify(~as.data.frame(rbind(factorProps('fodSmall',.,cum=FALSE))))
gc()


openxlsx::write.xlsx(list(attainment=attainment1,fieldOfDegree=fod),file='results/attainment.xlsx')

