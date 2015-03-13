# THIS FILE WILL BE SOURCE BY 03_deconvolve.bash
   3dDeconvolve \
      -overwrite \
      -input $t2 \
      -polort 2 \
      -local_times \
      -num_stimts 8 \
      \
      -stim_times 1  $expstdir/MemC.1D 'BLOCK(3,1)' -stim_label 1 'memC' \
      \
      -stim_times 2  $expstdir/TestC.1D 'TENT(0,6,2)' -stim_label 2 'testC' \
      \
      -stim_file 3  $motfile'[0]' -stim_base 3  \
      -stim_file 4  $motfile'[1]' -stim_base 4  \
      -stim_file 5  $motfile'[2]' -stim_base 5  \
      -stim_file 6 $motfile'[3]' -stim_base 6 \
      -stim_file 7 $motfile'[4]' -stim_base 7 \
      -stim_file 8 $motfile'[5]' -stim_base 8 \
      \
      -jobs 8 \
      -allzero_OK \
      -GOFORIT 100 \
      -float       \
      \
      -fout  -rout  -tout \
      -bucket $outdir/${sid}_${long_id}_${exp}_1_stats \
      -iresp 1 $outdir/${sid}_${long_id}_${exp}_1_iresp \
