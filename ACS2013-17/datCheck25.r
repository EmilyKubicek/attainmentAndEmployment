needDat <- FALSE
if(!exists("dat25")){
  if(exists("dat18")){
    if(exists("dat")) rm(dat)
    gc()
    dat25 <- filter(dat18,agep>24)
    rm(dat18); gc()
  } else if(exists("dat")){
    nnn <- with(dat,sum(blackORwhite=='Black'&agep>24&deaf=='deaf'))
    if(nnn!=16601) warning('Sample size wrong! is this the right "dat"?')
    if(min(dat$agep)==25){
      dat25 <- dat
      rm(dat);gc()
    } else if(min(dat$agep)<25){
       dat25 <- filter(dat,agep>24)
       rm(dat); gc()
    } else{
      print('something is weird with data, reloading')
      needDat <- TRUE
    }
  }
} else if(exists("dat25")){
  if(min(dat25$agep!=25)){
    print(paste('min age',min(dat25$agep)))
    needDat <- TRUE
  }
} else needDat <- TRUE

if(needDat){
  print('loading dataset')
  gc()
  load('attainmentEmploymentDataACS13-17.RData')
  dat25 <- filter(dat,agep>24)
  rm(dat); gc()
} else print('using dataset "dat25" already in workspace')
