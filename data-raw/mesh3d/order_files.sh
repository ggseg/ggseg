#!/bin/bash


# Nest by atlas
for i in $(ls *${1}*zip); do 
	mkdir -p  ${i%.fsaverage*}; 
	mv $i  ${i%.fsaverage*};

	cd ${i%.fsaverage*};
	unzip $i;
	cd -
done

# Nest by surface
for surf in white inflated LCBC; do
	echo $surf 
	for i in $(ls -d [rl]*${1}*${surf}/); do 
		echo .......$i
		new=$(echo $i | cut -d"/" -f1 | sed -e s/.${surf}//g)/${surf}
		mkdir -p $new; 
		mv $i/* $new; 
		rm -rf $(echo $i | cut -d"/" -f1 )
	done 
done


# Nest by hemisphere
for hemi in rh lh; do
	for i in $(ls -d ${hemi}*${1}*/*/); do 
		mkdir -p  $(echo $i | sed -e s/${hemi}.//g)/${hemi}; 
		mv $i/* $(echo $i | sed -e s/${hemi}.//g)/${hemi}; 
		rm -rf $(echo $i | cut -d/ -f1)
	done
done

# Reorganise the folders
# for i in $(ls -d *${1}*/); do 
# 	for surf in $(ls -d $i/*/)
# 	mkdir -p $i/white/rh; 
# 	mkdir -p $i/white/lh;
# 	mkdir -p $i/inflated/rh; 
# 	mkdir -p $i/inflated/lh; 
# 	mkdir -p $i/LCBC/rh; 
# 	mkdir -p $i/LCBC/lh; 

# 	mv $i/rh/white/* $i/white/rh; 
# 	mv $i/lh/white/* $i/white/lh; 
# 	mv $i/lh/inflated/* $i/inflated/lh; 
# 	mv $i/rh/inflated/* $i/inflated/rh; 
# 	mv $i/lh/LCBC/* $i/LCBC/lh; 
# 	mv $i/rh/LCBC/* $i/LCBC/rh; 

# 	rm -rf $i/rh/
# 	rm -rf $i/lh/
# done

# Remove puncts from foldernames for easier import to R
for i in $(ls -d *${1}*/); do 
	NEW=$(echo $i | sed -e s/[[:punct:]]//g)

 	echo $i $NEW
	if [ -d $NEW ]; then
		
		if [[ ! $i = ${NEW}/ ]]; then
			for surf in $(ls $i/); do
				rm -r $NEW/$surf
				mv $i/$surf $NEW/
			done

			rmdir $i
		fi

	else
		mkdir -p $(echo $i | sed -e s/[[:punct:]]//g)
		mv $i/* $(echo $i | sed -e s/[[:punct:]]//g); 
	fi
done

