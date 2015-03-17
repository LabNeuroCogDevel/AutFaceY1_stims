# 
# WF20150317  -- contrast asd/td face and cars
#  see AL's version: 
#  ../3dMVM/scripts/3dMVM_FacesvCars_test_2_rmbehoutliers.sh
#
# need datatable  from demog.txt (from 00_getData.R)
#
# Within subj : cars,faces(usa+aus)
# Between subj: ASD vs TD
# ---
# BTC says 
#  stim:.66 Cars - .33AUS -.33USA
#  diagnosis: .5ADS - .5TD

scriptdir=$(cd $(dirname $0);pwd)
contrast="mem"


# build datatable for 3dMVM
cut -f1,8- txt/demog.txt|while read id allother; do 
 # if first line add "InputFile"
 [[ $id == "Subj" ]] && echo "$id	$allother	stim	InputFile" && continue

 # do we have all 3 for this subj
 !  ls $scriptdir/glm/Y1_Center/$id/*{usa,aus,cars}*stats+tlrc.HEAD 2>/dev/null 1>&2 &&
  echo "missing usa aus or cars for $id" >&2 && continue

 #find the file we want to use as input
 for stim in usa aus cars; do
   echo "$id	$allother	$stim	$(ls $scriptdir/glm/Y1_Center/$id/*$stim*stats+tlrc.HEAD)[memC#0_Coef]"
 done

done> txt/demog_${contrast}.txt

[ ! -d 3dMVM ] && mkdir 3dMVM

3dMVM -prefix 3dMVM/$contrast \
  -model 'diagg*ageg' \
  -wsVars 'stim' \
  -mask /Volumes/TX/Autism_Faces/connect/mask/allctg/group_mask_intersection+tlrc.BRIK	\
  -jobs 8 \
  -num_glt 1 \
  -gltLabel 1 'faceMcars'  -gltCode 1 'stim : .33*usa +.33*aus -.66*cars' \
  -dataTable $(cat txt/demog_${contrast}.txt)

