####
# functions:
#  findCorrect
#  createdir
#  write1D
####

# use event name (ending in L C or R) to indentify the index
# use bit ops to see if that event was answered correctly
findCorrect <- function(event,acc) {
  # position 1 in str is last postion as number, so reverse
  sideOrder <- rev(c('L','C','R'));

  # find index of event, 1=left ,2=center,3=right
  idx<-lapply( as.character(event),  function(x){    a<-which( substr( x,nchar(x),nchar(x) ) == sideOrder )  } )
  # if empty, make NaN
  idx.nan<-unlist(ifelse(idx,idx,NaN))

  binrep <- intToBin(acc);
  # add padding incase rep is less than 3 digits long
  strrep <- sprintf("%03s",binrep);
  val    <- substr(strrep,idx.nan,idx.nan)
  return( as.numeric(val)==1 | is.na(val) ) 
}


# given a file name, make sure a directory exists
createdir<-function(fn) {
  dn<-dirname(fn);
  # create folder if dne
  if( ! file.exists(dn) ){ 
    if( ! file.exists(dirname(dn)) ){ 
     createdir(dn)
    }
    dir.create(dn) 
  }
}

# given a df (with only one subj, exp, and event
write1D <- function(df,correct=1,suffix="1D/correct") {
  sexep <- lapply(FUN=as.character, df[ 1, c('subj','exp','event') ]  )
  if(df$subj[1]  != df$subj [ nrow(df) ] ||
     df$exp[1]   != df$exp  [ nrow(df) ] ||
     df$event[1] != df$event[ nrow(df) ])
       warning('making 1d file for df with different subj exp or event names!')

  fn <- do.call(sprintf, c( "Y1_%s/%s/%s/%s.1D",suffix,sexep) )

  # if we dont care (-1) we get all rows
  # if we want incorrect (0) we want not correct
  # otherwise only use correct trials (rows)
  idxs <- ( function() {
    if(correct <0) return(1:nrow(df))
    if(correct==0) return(!df$eventCorrect )
    return(df$eventCorrect)
  })()

  # get onsets, * if none
  onsets <- df$onset[idxs]
  if(length(onsets)==0L) {onsets <- '*' }

  # write to file
  createdir(fn)
  try((function(){
     sink(fn)
     cat(onsets,"\n")
  })())
  sink()

}

