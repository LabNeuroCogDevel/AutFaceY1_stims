#!/bin/bash

# mod of /Volumes/TX/Autism_Faces/connect/mask/createGroupMask.bash
# WF 20150410

set -ex

datadir='/Volumes/TX/Autism_Faces/subject_data/'
mni="$HOME/standard/mni_icbm152_nlin_asym_09c/mni_icbm152_gm_tal_nlin_asym_09c_3mm.nii"

# where are we suppose to be
#scriptdir=$(cd $(dirname $0);pwd)
cd $(dirname $0)

for f in $datadir/byID/*/experiment1/*faces*/subject_mask.nii.gz; do 
  subjface=$(echo $f|cut -d/ -f8,10|tr / ' ') 
  nv=$(3dBrickStat -non-zero -count $f)
  echo $subjface $nv; 
done > txt/maskVoxelCount.txt

[ ! -d masks ] && mkdir masks

# all masks together
fslmerge -t masks/allface1.nii.gz $datadir/byID/*/experiment1/*faces*/subject_mask.nii.gz
# only where all have values
fslmaths    masks/allface1.nii.gz -Tmin masks/allface1_intersect.nii.gz
# where most have values and is on template brain
fslmaths masks/allface1.nii.gz -Tmean masks/allface1_mean -odt float
fslmaths masks/allface1_mean -thr 0.95 -bin masks/allface1_95 -odt char

fslmaths $mni -thr 0.20 -bin -mul masks/allface1_95 masks/allface1_95MNIthr.2 -odt char
