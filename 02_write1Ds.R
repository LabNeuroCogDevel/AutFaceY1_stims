#!/usr/bin/env Rscript

library(reshape2)
library(plyr)
library(R.utils)

#library(r.utils)


# read in eprime log data from 102 and 103
df <- read.table('timing.txt',sep="\t",header=T)


# subset to just the fixation columns
df.fix <- df[,c(1:4,grep('Fix|RT',names(df)))]

# accuracy
# 0 -> none correct       | 4 -> rigth only
# 1 -> left only          | 5 -> left & center only
# 2 -> center only        | 6 -> center & right only
# 3 -> both left & center | 7 -> all correct
df.fix$acc <- df$TestLeft.ACC + 2*df$TestCenter.ACC + 4*df$TestRight.ACC

# # put 102 and 103 fix's on the same row
# df.rs <- reshape(df.fix,direction="wide", idvar=c('exp','trial','acc'),timevar='subj',drop='date')
# 
# # all is the same
# numdiffs <- length(which( c(
#   df.rs$Fixation1.102 != df.rs$Fixation1.103, 
#   df.rs$Fixation2.102 != df.rs$Fixation2.103, 
#   df.rs$Fixation3.102 != df.rs$Fixation3.103 
#  ) ) )
# 
# if(numdiffs>0) error('timings are not the same for each subject!')


## ORDER
# Fix 1 
# mem L -> 3000
# mem C -> 3000
# mem R -> 3000
# Fix 2
# Test L -> 4500
# Fix 3
# Test C -> 4500
# Fix 4
# Test R -> 4500
eventorder=c( 'Fixation1' ,'MemL' ,'MemC' ,'MemR' ,'Fixation2' ,
'TestL.RT','TestL','Fixation3' ,
'TestC.RT','TestC','Fixation4',
'TestR.RT','TestR' )

# grab only one subject (full timing, no repeats)
#dfw<-df.fix[df.fix$subj==102,-c('subj','date')]
dfw<-subset(df.fix,subj==102)
# or use everyone, why not
dfw<-df.fix

dfw$MemL <- dfw$MemC <- dfw$MemR <- 3000
dfw$TestL <- dfw$TestC <- dfw$TestR <- 4500

# put fixations from columns into rows (one time per row)

# rename Test[LCR]*.RT to remove whatever is in the *
testRTidxs <- grep('RT',names(dfw))
names(dfw)[testRTidxs] <- gsub('eft|enter|ight','',names(dfw)[testRTidxs] )
testRTs <- names(dfw)[testRTidxs]

# reshape (melt) dfw so each event is its own row (useful for cumsum later)
dfm<-melt(dfw,id.vars=c('subj','date','exp','trial','acc',testRTs),variable.name="event",value.name='dur')

dfm$event <- factor(dfm$event, levels=eventorder)
# sort by when it would happen
dfmo <-with(dfm, dfm[order(subj,date,exp,trial,event),]) 
# score based on test event
sideOrder <- c('L','C','R');

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
# testing for findCorrect
# all( ! findCorrect(c('L','L','L','L'),c(0,2,4,6)) )
# all( findCorrect(c('C','C','C','C'),c(2,3,6)) )


# given a file name, make sure a directory exists
createdir<-function(fn) {
  dn<-dirname(fn);
  # create folder if dne
  if( ! file.exists(dn) ){ dir.create(dn) }
}

# list if event should be included (based on correct response)
dfmo$eventCorrect <-  with(dfmo, findCorrect(event,acc) )

# write out file per subj+experiement and file per subj+experiment+event (for correct only)
df.onsets <- ddply(dfmo,.(subj,date, exp), function(x) {

  # generate onsets
  x$onset <- cumsum(x$dur)/1000

  # create RSP time event
  dfx <- subset(x,grepl('Test',event))
  #dfx$onset - dfx$dur + dfx[testype]

  # not an eligant solution
  RTonset <- dfx;
  for(i in 1:nrow(dfx)){
    testRTidx <- sprintf('%s.RT',dfx$event[i])
    RT <- dfx[i,testRTidx]
    if(RT==0) { RT <- Inf }

    # set event type
    RTonset$event[i] <- testRTidx
    # set duration as RT time
    RTonset$dur[i] <- RT 
    # set onset as onset - total test duration + actual RT
    RTonset$onset[i] <- 
        (dfx$onset[i]*1000 - dfx$dur[i] + RT)/1000
  }

  # drop any Infs
  RTonset<- RTonset[is.finite(RTonset$dur),]

  # add new onsets and sort again
  x<-rbind(x,RTonset)
  x<-with(x, x[order(subj,date,exp,trial,event),]) 
  # remove test columns
  x<-x[,-c(grep('RT$',names(x)))]

  
  # get file name
  fn<-sprintf('Y1/%s/%s.txt',x$subj[1],x$exp[1])
  # and dirname
  createdir(fn)

  # write subj+experiment
  write.table(x,col.names=T,row.names=F, file=fn)


  # put all fixations into the same class
  x$event <- as.character(x$event)
  x$event[grep('Fixation',x$event)] <- 'Fixation'
  # put all RTs into the same class
  x$event[grep('RT$',x$event)] <- 'RT'

  # so we dont create different files for correct and incorrect RT
  # make all RTs correct
  x$evetCorrect[x$event=='RT'] <- TRUE
  
  # write out subject event file
  ddply(x,.(event),function(xe){
    fn<-sprintf('Y1/%s/%s/%s.1D',xe$subj[1],xe$exp[1],xe$event[1])
    createdir(fn)
    # correct
    d <- xe$onset[xe$eventCorrect]
    if(length(d)==0L) {d <- '*' }
    sink(fn)
    cat(d,"\n")
    sink()

    # done if fixation
    if(xe$event[1] == 'Fixation') { return() }
    if(xe$event[1] == 'RT') { return() }

    # incorrect
    fn<-sprintf('Y1/%s/%s/%s_incorrect.1D',xe$subj[1],xe$exp[1],xe$event[1])
    d <- xe$onset[!xe$eventCorrect]
    if(length(d)==0L) {d <- '*' }
    sink(fn)
    cat(d,"\n")
    sink()
  })
  
  return(x)
})

