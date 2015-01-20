#!/usr/bin/env perl
use strict; use warnings;
use feature 'say';
# perl -slane '$level=$1 if m/Level: (\d)/; %a=() if(/LogFrame Start/); print map {"$_\t"} sort keys %a if /LogFrame End/ and $level==4; $a{$1}=$2 if /^\s+(.*):(.*)/ and $level==4; print "$level $_" if 0' //mnt/B/bea_res/Data/Tasks/Reversal_MMY3/*.txt|sort |uniq -c
#    2360 LeftStim   NumRight Procedure   RightStim   Running  TrainProbabilistic   TrainProbabilistic.Cycle   TrainProbabilistic.Sample  TrialType   Valid acc   ansr1 incorrect   presentfeedback.DurationError presentfeedback.OnsetDelay presentfeedback.OnsetTime  presentfeedback.StartTime  showstim.ACC   showstim.CRESP showstim.OnsetTime   showstim.RESP  showstim.RT 
#    7564 LeftStim   NumRight Procedure   RightStim   Running  TrainProbabilistic   TrainProbabilistic.Cycle   TrainProbabilistic.Sample              Valid acc   ansr1 incorrect   presentfeedback.DurationError presentfeedback.OnsetDelay presentfeedback.OnsetTime  presentfeedback.StartTime  showstim.ACC   showstim.CRESP showstim.OnsetTime   showstim.RESP  showstim.RT 
#       1 LeftStim   NumRight Procedure   RightStim   Running  TrainProbabilistic   TrainProbabilistic.Cycle   TrainProbabilistic.Sample  TrialType   Valid acc   ansr1 incorrect                                                                                                                  showstim.ACC   showstim.CRESP showstim.OnsetTime   showstim.RESP  showstim.RT 
#     248 Procedure  RepNum   Running  ScanBlockType  ScanBlockType.Cycle  ScanBlockType.Sample
my $level=0;
my %a;

my $subj="";
my $date="";
my $exp="";
my $trial=0;

my @columns=qw/FaceLeft FaceCenter FaceRight TestLeft TestCenter TestRight CorrectResp1 CorrectResp2 CorrectResp3 Fixation1 Fixation2 Fixation3 Fixation4 Running TrialList.Cycle TrialList.Sample TestLeft.ACC TestLeft.RT TestLeft.RESP TestLeft.CRESP TestCenter.ACC TestCenter.RT TestCenter.RESP TestCenter.CRESP TestRight.ACC TestRight.RT TestRight.RESP TestRight.CRESP/;




say join "\t", "subj","date","exp","trial",@columns;
while(<>){
   #SessionDate: 10-07-2014
   #Subject: 11331
   $subj=$1 and $trial=0       if /Subject: (\d+)/;
   $date="$3$1$2" if /SessionDate: (\d{2})-(\d{2})-(\d{4})/;
   $exp="$1"      if /Experiment: (.*)/;
   
   # reset if end of file
   if(/^\*\*\* LogFrame End \*\*\*$/){
      $subj="";
      $date="";
   }

   ### log levels are conained within /log start/ and /log end/
   
   # keep level up to date
   $level=$1 if m/Level: (\d)/;
   # reset hash after every log frame
   %a=() if(/LogFrame Start/);

   # if this is the end of level 4
   if(/LogFrame End/) {
      $a{'TestLeft.ACC'}   = $a{'TestFront.ACC'} if $a{'TestFront.ACC'};
      $a{'TestCenter.ACC'} = $a{'TestSide.ACC'}  if $a{'TestSide.ACC'};
      $a{'TestRight.ACC'}  = $a{'TestBack.ACC'}  if $a{'TestBack.ACC'};
      $a{'TestLeft.RT'}    = $a{'TestFront.RT'}  if $a{'TestFront.RT'};
      $a{'TestCenter.RT'}  = $a{'TestSide.RT'}   if $a{'TestSide.RT'};
      $a{'TestRight.RT'}   = $a{'TestBack.RT'}   if $a{'TestBack.RT'};
      my @vals=map {$a{$_}||""} @columns;
      next if $#vals<0 ;
      if($level==3) {
       $trial+=1;
       say join "\t", $subj,$date,$exp,$trial,@vals;
      }
   }
   $a{$1}=$2 if /^\s+(.*):(.*)/;
   print "$level $_" if 0
}
