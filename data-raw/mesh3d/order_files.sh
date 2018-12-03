#!/bin/bash

# Nest by atlas
for i in $(ls *zip); do 
	mkdir -p  ${i%.fsaverage*}; 
	mv $i  ${i%.fsaverage*};

	cd ${i%.fsaverage*};
	unzip $i;
done

# Nest by surface
for i in $(ls -d [rl]*inflated/); do 
	mkdir -p $(echo $i | sed -e s/.inflated//g)/inflated; 
	mv $i/* $(echo $i | sed -e s/.inflated//g)/inflated; 
	rm -rf $i
done


for i in $(ls -d [rl]*white/); do 
	mkdir -p $(echo $i | sed -e s/.white//g)/white; 
	mv $i/* $(echo $i | sed -e s/.white//g)/white; 
	rm -rf $i
done 


# Nest by hemisphere
for i in $(ls -d [rh]*/); do 
	mkdir -p  $(echo $i | sed -e s/rh.//g)/rh; 
	mv $i/* $(echo $i | sed -e s/rh.//g)/rh; 
	rm -rf $i
done

for i in $(ls -d [lh]*/); do 
	mkdir -p  $(echo $i | sed -e s/lh.//g)/lh; 
	mv $i/* $(echo $i | sed -e s/lh.//g)/lh; 
	rm -rf $i
done



# Reorganise the folders
for i in $(ls -d */); do 
	mkdir -p $i/white/rh; 
	mkdir -p $i/white/lh;
	mkdir -p $i/inflated/rh; 
	mkdir -p $i/inflated/lh; 

	mv $i/rh/white/* $i/white/rh; 
	mv $i/lh/white/* $i/white/lh; 
	mv $i/lh/inflated/* $i/inflated/lh; 
	mv $i/rh/inflated/* $i/inflated/rh; 

	rm -rf $i/rh/
	rm -rf $i/lh/
done


# Remove puncts from foldernames for easier import to R
for i in $(ls -d */); do 
	mv $i $(echo $i | sed -e s/[[:punct:]]//g); 
done


