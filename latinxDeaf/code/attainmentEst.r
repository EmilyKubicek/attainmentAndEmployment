source('datCheck25.r')

capsRight <- function(x,tab)
  if(x%in%tab) x else tab[tolower(tab)==tolower(x)]

noby <- function(x,tab) capsRight(substr(x,3,nchar(x)),tab)

stand1 <- function(x,FUN,dat,...){
  fun1 <- function(grp,data){
    if(substr(grp,1,2)=='by') grp <- noby(grp,names(data))
    data%>%group_by(deaf,blackORwhite,!!sym(grp))%>%do(x=FUN(x,.))
  }
  fun2 <- function(.data)
    append(
      append(
        list(overall=.data%>%group_by(deaf,blackORwhite)%>%do(x=FUN(x,.))),
        sapply(paste0('by',c('Age','Sex','Nativity','Lanx')),fun1,data=.data,simplify=FALSE)),
      list(byDiss=lapply(c('diss','blind','selfCare','indLiv','amb','cogDif'),fun1,data=.data))
    )
  fun3 <- function(fil,data)
    data%>%filter(!!sym(fil))%>%group_by(deaf)%>%do(x=FUN(x,.))

  out <-
    append(
      append(
        list(
          NationalAverage=dat%>%do(x=FUN(x,.)),
          blackMulti=dat%>%filter(blackMulti!='NotBlack')%>%group_by(deaf,blackMulti)%>%do(x=FUN(x,.))
        ),
        dat%>%filter(blackORwhite!='Other')%>%fun2(.)),
      sapply(paste0('black',c('Latinx','Asian','ANDwhite')),fun3,data=dat,simplify=FALSE)
    )
  names(out) <- gsub('ANDw','W',names(out))

  out
}



attainment1 <- dat25%>%stand1('attainCum',factorProps,.)
gc()
##### Field of degree
fod <- dat25%>%filter(blackORwhite!='Other',attainCum>'Associates')%>%group_by(deaf,blackORwhite)%>%
  do(x=factorProps('fodSmall',.,cum=FALSE))
gc()


gc()
save(attainment1,fod,overallEnr,raceEnr,raceGenderEnr,file='output/attainment.RData')

source('makeTables.r')

