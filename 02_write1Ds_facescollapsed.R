#!/usr/bin/env Rscript

library(reshape2)
library(R.utils)
library(plyr)
library(dplyr)
library(tidyr)

####################
# reads in timing.txt
# makes
#  dfw       -- wide: row per 3 tests (full trial), for plotting
#  df.onsets -- long: row per event w/event onsets, eventCorrect T/F for specific Mem&Test (from acc), for writing 1Ds
# writse
#  1D/ directories


# read in eprime log data
#   timig.txt created by perl script (parse.pl) over all eprime log files
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

####
# load functions
#  findCorrect, write1D, createdir 
source('writeFuncs.R')
####

# list if event should be included (based on correct response)
dfmo$eventCorrect <-  with(dfmo, findCorrect(event,acc) )
# or as dplyr
#dfmo <- dfmo %>% mutate(eventCorrect = findCorrect(event,acc))


# get onset of each event
df.onsets <- dfmo %>% 
     # make sure we are sorted by trial and event
     arrange(subj,date,exp,trial,event) %>%
     # we want to look accross runs
     group_by(subj,date,exp) %>% 
     # use all events to build onsets
     mutate(onset=(cumsum(dur)-dur)/1000) 

##########
# NOW HAVE:
# dfw       -- wide: row per 3 tests (full trial) 
# df.onsets -- long: row per event w/event onsets, eventCorrect T/F for specific Mem&Test (from acc)

#########
# Make 1D files
#########


##### Get only Center ####
onsets.Center <- df.onsets %>%
    # grab only the center events
    filter(grepl('.C',event)) %>%
    # make output easier to see
    select(-TestL.RT,-TestR.RT) %>%
    # make sure we are sorted before writing 1D files
    arrange(subj,date,exp,trial,onset) 

# write out all to a 1D file
ddply(onsets.Center,.(subj,date,exp,event),function(x) write1D(x,1,'Y1_Center/correct') )
ddply(onsets.Center,.(subj,date,exp,event),function(x) write1D(x,0,'Y1_Center/incorrect') )
ddply(onsets.Center,.(subj,date,exp,event),function(x) write1D(x,-1,'Y1_Center/all') )



##### Collapse Memory #####

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
ddply(all.small,.(subj,date,exp,event),function(x) write1D(x,1,'Y1_AllMem_1D/correct') )
ddply(all.small,.(subj,date,exp,event),function(x) write1D(x,0,'Y1_AllMem_1D/incorrect') )
ddply(all.small,.(subj,date,exp,event),function(x) write1D(x,-1,'Y1_AllMem_1D/all') )


source('Rplots/behaveplot.R')
