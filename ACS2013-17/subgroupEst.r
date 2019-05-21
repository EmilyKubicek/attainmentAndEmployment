source('datCheck25.r')
gc()

perDeaf <- dat25%>%group_by(blackORwhite)%>%mutate(DEAF=deaf=='deaf')%>%
  summarize(percent.deaf=svmean(DEAF,pwgtp)*100,n=n())

perDeaf <- bind_rows(dat25%>%mutate(DEAF=deaf=='deaf')%>%
                       summarize(blackORwhite='Overall',percent.deaf=svmean(DEAF,pwgtp)*100,n=n()),
  perDeaf)

write.xlsx(perDeaf,file='output/percentDeafbyRace.xlsx')

subgroups <- c('Age','Sex','nativity','lanx','diss','blind','selfCare','indLiv','amb','cogDif')
subPer <- sapply(subgroups,
  function(ss) dat25%>%
                 group_by(deaf,blackORwhite)%>%
                 group_map(~as.data.frame(rbind(factorProps(ss,.x,cum=FALSE))))%>%
                 select(-ends_with('SE')),
  simplify=FALSE)

## subPer <- sapply(subPer,function(dd){
##   dd <- cbind(dd[,-which(names(dd)=='x')],do.call('rbind',dd$x))
##   dd[,-grep(' SE',names(dd),fixed=TRUE)]
## },
## simplify=FALSE)

## names(subPer) <- subgroups

for(gg in paste0('black',c('Latinx','Asian','ANDwhite'))){
  subPer[[gg]] <- dat25%>%filter(blackORwhite=='Black')%>%
    group_by(deaf)%>%summarize(blackORwhite=blackORwhite[1],x=svmean(!!sym(gg),pwgtp)*100,n=n())
  names(subPer[[gg]])[names(subPer[[gg]])=='x'] <- paste('%',gg)
}
subPer[['% blackMulti']] <- dat25%>%filter(blackORwhite=='Black')%>%
  group_by(deaf)%>%mutate(blackMulti=blackMulti=='BlackMulti')%>%
  summarize(blackORwhite=blackORwhite[1],x=svmean(blackMulti,pwgtp)*100,n=n())
names(subPer[['% blackMulti']])[names(subPer[['% blackMulti']])=='x'] <- '% blackMulti'

subPer[['% blackAlone']] <- dat25%>%filter(blackORwhite=='Black')%>%
  group_by(deaf)%>%mutate(blackAlone=blackMulti=='BlackAlone')%>%
  summarize(blackORwhite=blackORwhite[1],x=svmean(blackAlone,pwgtp)*100,n=n())
names(subPer[['% blackAlone']])[names(subPer[['% blackAlone']])=='x'] <- '% blackAlone'



subPer2 <- subPer[[1]]
for(i in 2:length(subPer)) subPer2 <- full_join(subPer2,subPer[[i]])

nnn <- subPer2$n
subPer2$n <- NULL
subPer2 <- cbind(subPer2,n=nnn)

subPer2 <- t(subPer2)
write.xlsx(subPer2,'output/subgroupPercentages.xlsx',row.names=TRUE,col.names=FALSE)
