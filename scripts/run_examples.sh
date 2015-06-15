#!/bin/sh

for example in box_constrained_control.py cbp.py portfolio.py svm.py tone_mapping.py
do
  echo "Running $example"
  yes "" | ../examples/$example
done

for example in lasso.py fir_lowpass.py
do
  echo "Running $example"
  yes "" | ../examples/$example `pwd`/../ecos
done
