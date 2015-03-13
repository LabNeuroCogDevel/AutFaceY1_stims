#!/usr/bin/env Rscript

library(reshape2)
library(R.utils)
library(plyr)
library(dplyr)
library(tidyr)


# read in eprime log data
df <- read.table('timing.txt',sep="\t",header=T)
#   subj	date	        exp     	trial	
#   FaceLeft	FaceCenter	FaceRight	
#   TestLeft	TestCenter	TestRight	
#  CorrectResp1	CorrectResp2	CorrectResp3	
#   Fixation1	Fixation2	Fixation3	Fixation4	
#   Running	TrialList.Cycle	TrialList.Sample	
#  TestLeft.ACC	TestLeft.RT	TestLeft.RESP	
#TestLeft.CRESP	TestCenter.ACC	TestCenter.RT	
#  TestCenter.RESP	TestCenter.CRESP	
#  TestRight.ACC	TestRight.RT
#  TestRight.RESP	TestRight.CRESP


# subset to just the fixation and RT columns
df.fix <- df[,c(1:4,grep('Fix|RT',names(df)))]

# accuracy
# 0 -> none correct       | 4 -> rigth only
# 1 -> left only          | 5 -> left & center only
# 2 -> center only        | 6 -> center & right only
# 3 -> both left & center | 7 -> all correct
df.fix$acc <- df$TestLeft.ACC + 2*df$TestCenter.ACC + 4*df$TestRight.ACC


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
eventorder=c( 'Fixation1', 'MemL'    ,'MemC' ,'MemR' ,
              'Fixation2', 'TestL.RT','TestL',
              'Fixation3', 'TestC.RT','TestC',
              'Fixation4', 'TestR.RT','TestR' )

# previously tested on a few, now use everyone
dfw<-df.fix

# Mems are 3s, Tests are 4.5s
dfw$MemL  <- dfw$MemC  <- dfw$MemR  <- 3000
dfw$TestL <- dfw$TestC <- dfw$TestR <- 4500


# rename Test[LCR]*.RT to remove whatever is in the *
testRTidxs <- grep('RT',names(dfw))
names(dfw)[testRTidxs] <- gsub('eft|enter|ight','',names(dfw)[testRTidxs] )
testRTs <- names(dfw)[testRTidxs]

# reshape (melt) dfw so each event is its own row (useful for onset=cumsum later)
dfm<-melt(dfw,id.vars=c('subj','date','exp','trial','acc',testRTs),variable.name="event",value.name='dur')

# make events a factor so we can arrange/order them
dfm$event <- factor(dfm$event, levels=eventorder)
# sort by when it would happen
dfmo <-with(dfm, dfm[order(subj,date,exp,trial,event),]) 

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


# list if event should be included (based on correct response)
dfmo$eventCorrect <-  with(dfmo, findCorrect(event,acc) )
# or as dplyr
#dfmo <- dfmo %>% mutate(eventCorrect = findCorrect(event,acc))

#####

# get onset of each event
df.onsets <- dfmo %>% 
     # make sure we are sorted by trial and event
     arrange(subj,date,exp,trial,event) %>%
     # we want to look accross runs
     group_by(subj,date,exp) %>% 
     # use all events to build onsets
     mutate(onset=(cumsum(dur)-dur)/1000) 


onsets.small <- df.onsets %>%
      # truncate num of columns to just what's needed
      select(subj,date,exp,trial,event,eventCorrect,onset) %>% 

      # collapse accross Mem (e.g. MemC -> 'Mem')
      mutate(event=gsub('Mem.*','Mem',as.character(event))) %>% 
      #   the onlything not grouped is is onset & correct
      group_by(subj,date,exp,trial,event) %>%
      #   get smallest onset, all must be correct to be correct
      summarise(onset=min(onset),eventCorrect=all(eventCorrect)) %>%

      # make all fixations the same event (e.g. Fixation1 -> Fixation)
      mutate(event=gsub('\\d$','',as.character(event))) 


# memory test events have response too
# let's get those
responses.small <- df.onsets %>%
     # grab only test events
     filter(grepl('Test',event)) %>%
     # a row for each Test[LCR].RT 
     gather(RTt,RT,TestL.RT,TestC.RT,TestR.RT) %>%
     # remove rows where RTtype doesn't match event
     filter(sprintf('%s.RT',event)==RTt) %>%
     # make sure we are sorted
     arrange(subj,date,exp,trial,onset) %>%
     # response is onset+RT
     mutate(response= onset + RT/1000) %>% 
     # set even to response and onset to response onset
     mutate(onset=response,event='response') %>%
     # select only the columns we need
     select(subj,date,exp,trial,event,eventCorrect,onset)


all.small <- rbind(onsets.small,responses.small) %>% 
     ungroup() %>%
     arrange(subj,date,exp,trial,onset) 


# write out all to a 1D file
ddply(all.small,.(subj,date,exp,event),function(x) write1D(x,1,'1D/correct') )
ddply(all.small,.(subj,date,exp,event),function(x) write1D(x,0,'1D/incorrect') )
ddply(all.small,.(subj,date,exp,event),function(x) write1D(x,-1,'1D/all') )

# Histogram of the totally correct responses by group and exp
# see also
# perl -slane 'print $#F+1 if ! /\*/' Y1 <- 1D/correct/2*/CMFT/Mem.1D |Rio -ne 'sum(df$V1)'
dfw %>% 
 group_by(subj,date,exp,acc) %>%
  mutate(group=ifelse(as.numeric(subj)>=200,'200','100')) %>%
  filter(acc==7) %>%  # 7 means all correct (1 is only L, 2 is only C, 3 is L+C, 5 L+R)
  ggplot(aes(x=exp,fill=group)) + 
  geom_histogram(position='dodge')+
  theme_bw() + ggtitle('Full correct Trial')


#PLOT: how many correct/incorrect trials are there
corrplot <- dfw %>% 
 group_by(subj,date,exp,acc) %>%
  mutate(group=ifelse(as.numeric(subj)>=200,'200','100')) %>%
  mutate(accLab=cut(acc,c(-1,1,6,8),labels=c("wrong","okay","perfect")) ) %>%
  ggplot(aes(x=exp,fill=accLab)) + 
  geom_histogram(position='stack')+ facet_wrap(~group) +
  theme_bw() + ggtitle('Mem+Test Trial correctness')

print(corrplot)
 
#PLOT: accuracy (std)
accplot <- dfw %>% 
 mutate(group=ifelse(as.numeric(subj)>=200,'200','100')) %>%
 group_by(subj,date,exp,group) %>%
 summarise(totacc=sum(acc==7)/n() ) %>%
 ggplot(aes(x=exp,y=totacc, fill=group)) + 
  geom_boxplot() +
  #geom_point(aes(color=group),alpha=.4, position='dodge') +
  theme_bw() + ggtitle('Mem+Test Trial correctness')

print(accplot)
