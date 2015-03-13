#!/usr/bin/env bash
set -e
trap '[ "$?" -ne 0 ] && echo "$0: exited on error!"' EXIT

## 20150116WF -- deconvolve script partially modeled from AL's (../3Ddeconvolvefiles/deconvolve_cond1)
#                AL had 1 mem/test and duration 3x as long as those here
#                TODO: verify TENT model

# where subject data is
datadir='/Volumes/TX/Autism_Faces/subject_data/'

# make output directory
scriptdir=$(cd $(dirname $0);pwd)
[ -z "$1" ] && echo "need first argument to be stimdir, where to find 1Ds and GLM.src.bash" && exit 1
stimdir="$1"
# check on stimdir 
[ ! -d $stimdir ] && stimdir="$scriptdir/$stimdir/"
[ ! -d $stimdir ] && echo "cannot find stimdir! ($1 or $stimdir)" && exit 1

# make dir to store output, if we need one
[ ! -d $scriptdir/glm ] && mkdir $scriptdir/glm

# run 3dDeconvolve for every experiment
for expdir in $datadir/*/experiment1/{faces_usa,faces_aus,cars}; do

   # how are we IDing this subject
   long_id=$(echo $expdir | perl -ne 'print $1 if m:/(\d{4})/:')
   # short id is the actual folder, symblinked from long_id
   sid=$(basename $(readlink $datadir/$long_id))

   # remove the path to get the experment name
   exp=$(basename $expdir)
   # put exp in terms of EPrime names, to get stims
   case $exp in
    faces_usa) EPname="CMFT";;
    faces_aus) EPname="AUS_CMFT";;
    cars)      EPname="Cars";;
    *)         echo "bad exp name: $exp" && continue;;
   esac

   # final preprocessed t2 from MH's script
   t2=$expdir/nfswkmt_functional.nii.gz 
   [ ! -r "$t2" ] && echo "$sid $long_id $exp: missing t2 ($t2)" && continue

   # check we have a stimdir for this exp
   expstdir="$stimdir/correct/$sid/$EPname"
   [ ! -d "$expstdir" ] && echo "$sid $long_id $exp: no stimdir! ($expstdir)" && continue


   motfile="$expdir/mcplots.par"
   [ ! -r "$motfile" ] && 
     echo "$sid $long_id $exp: cannot read motion file ($motfile)" &&
     continue 



   echo
   echo "----- $sid $long_id $exp $EPname"
   echo

   
   # make output directory if we need to
   # based on stimdir
   outdir="$scriptdir/glm/$(basename $stimdir 1D)/$sid"
   [ ! -d "$outdir" ] && mkdir -p "$outdir"

   # just so everything is dumped in the correct spot
   cd $outdir

   # and get a template brain in there
   [ ! -r mni.nii ] &&
    ln -s /Users/lncd/standard/mni_icbm152_nlin_asym_09c/mni_icbm152_t1_tal_nlin_asym_09c.nii ./mni.nii

   # 20150313 WF -- b/c we may have a few contrasts
   #GLM (3dDeconvolve) command stored in $stimdir
   # as GLM.src.bash
   source $stimdir/GLM.src.bash
   #
   # run glm
   ## 3dDeconvolve \
   ##    -overwrite \
   ##    -input $t2 \
   ##    -polort 2 \
   ##    -local_times \
   ##    -num_stimts 13 \
   ##    \
   ##    -stim_times 1  $expstdir/MemC.1D 'BLOCK(3,1)' -stim_label 1 'memC' \
   ##    -stim_times 2  $expstdir/MemL.1D 'BLOCK(3,1)' -stim_label 2 'memL' \
   ##    -stim_times 3  $expstdir/MemR.1D 'BLOCK(3,1)' -stim_label 3 'memR' \
   ##    \
   ##    -stim_times 4  $expstdir/TestC.1D 'TENT(0,6,2)' -stim_label 4 'testC' \
   ##    -stim_times 5  $expstdir/TestL.1D 'TENT(0,6,2)' -stim_label 5 'testL' \
   ##    -stim_times 6  $expstdir/TestR.1D 'TENT(0,6,2)' -stim_label 6 'testR' \
   ##    \
   ##    -stim_file 7  $motfile'[0]' -stim_base 7  \
   ##    -stim_file 8  $motfile'[1]' -stim_base 8  \
   ##    -stim_file 9  $motfile'[2]' -stim_base 9  \
   ##    -stim_file 10 $motfile'[3]' -stim_base 10 \
   ##    -stim_file 11 $motfile'[4]' -stim_base 11 \
   ##    -stim_file 12 $motfile'[5]' -stim_base 12 \
   ##    \
   ##    -stim_times 13 $expstdir/RT.1D 'BLOCK(1,1)' -stim_label 13 'RSP'\
   ##    \
   ##    -num_glt 2 \
   ##    -gltsym "SYM:.33*memC   +.33*memL   +.33*memR"   -glt_label 1 'mem' \
   ##    -gltsym "SYM:.33*testC  +.33*testL  +.33*testR"  -glt_label 2 'test' \
   ##    \
   ##    -jobs 8 \
   ##    -allzero_OK \
   ##    -GOFORIT 100 \
   ##    -float       \
   ##    \
   ##    -fout  -rout  -tout \
   ##    -bucket $outdir/${sid}_${long_id}_${exp}_1_stats \
   ##    -iresp 1 $outdir/${sid}_${long_id}_${exp}_1_iresp \

      # if we want to try to model incorrect, will have junk data for many though -- 1 or no incorrects frequent
      #\
      #-stim_times 13 $expstdir/MemC_incorrect.1D 'BLOCK(3)' -stim_label 13 'memCi' \
      #-stim_times 14 $expstdir/MemL_incorrect.1D 'BLOCK(3)' -stim_label 14 'memRi' \
      #-stim_times 15 $expstdir/MemR_incorrect.1D 'BLOCK(3)' -stim_label 15 'memLi' \
      #\
      #-stim_times 16 $expstdir/TestC_incorrect.1D 'TENT(0,6,2)' -stim_label 16 'testCi' \
      #-stim_times 17 $expstdir/TestL_incorrect.1D 'TENT(0,6,2)' -stim_label 17 'testRi' \
      #-stim_times 18 $expstdir/TestR_incorrect.1D 'TENT(0,6,2)' -stim_label 18 'testLi' \
      #\
      #-gltsym "SYM:.33*memCi  +.33*memLi  +.33*memRi"  -glt_label 3 'memi' \
      #-gltsym "SYM:.33*testCi +.33*testLi +.33*testRi" -glt_label 4 'testi' \

      #break
done
