needDat <- FALSE
if(!exists("dat18")){
  if(exists("dat")){
    nnn <- with(dat,sum(latinx&agep>24&deaf=='deaf'))
    if(nnn!=19655) warning('Sample size wrong! is this the right "dat"?')
    if(min(dat$agep)==18){
      dat18 <- dat
      rm(dat);gc()
    } else if(min(dat$agep)<18){
      warning('why is min age <18?')
      dat18 <- filter(dat,agep>17)
      rm(dat); gc()
    } else{
      print('something is weird with data, reloading')
      needDat <- TRUE
    }
  } else needDat <- TRUE
} else if(exists("dat18")){
  if(min(dat18$agep!=18)){
    print(paste('min age',min(dat18$agep)))
    needDat <- TRUE
  }
}

if(needDat){
  print('loading dataset')
  gc()
  load('data/attainmentEmploymentDataACS14-18.RData')
  dat18 <- filter(dat,agep>17)
  rm(dat); gc()
} else print('using dataset "dat18" already in workspace')
