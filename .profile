
PS1='\n[\[\033[01;32m\]\w\[\033[0m\]]$ '
export PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin

umask 22

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export R_MAX_VSIZE=34Gb R
export JAVA_HOME=/usr/libexec/java_home

# FSL Setup
FSLDIR=/usr/local/fsl
PATH=${PATH}:${FSLDIR}/bin
export FSLDIR PATH
. ${FSLDIR}/etc/fslconf/fsl.sh

#Source Python
export PYENV_ROOT=/usr/local/var/pyenv

#Freesurfer setup
export FREESURFER_HOME=/Applications/freesurfer
source $FREESURFER_HOME/SetUpFreeSurfer.sh
PATH=${PATH}:${FREESURFER_HOME}/scripts:

#Include inkscape and MATLAB in PATHS
PATH=$PATH:Library/TeX/texbin:/opt/X11/bin:/Applications/Inkscape.app/Contents/Resources/bin:/usr/local/opt/libxml2/bin


#ALIASES
alias sshfs='umask 2; sshfs'
alias Abel='sshfs athanasm@abel.uio.no:/projects/psifmri/fMRI/ADHD ~/Abel -o auto_cache,reconnect,defer_permissions,noappledouble,negative_vncache,volname=Abel'
alias UiOLin='sshfs athanasm@login.uio.no: ~/UiOLin -o auto_cache,reconnect,defer_permissions,noappledouble,negative_vncache,volname=UiOLin'
alias LCBC='sshfs athanasm@sv-tilgang.uio.no:/net/lh-evs/uio/lagringshotell/sv-psi/LCBC ~/LCBC -o auto_cache,reconnect,defer_permissions,noappledouble,negative_vncache,volname=LCBC'
alias abel='ssh -Y athanasm@abel.uio.no'
alias linux='ssh -Y athanasm@login.uio.no'
alias ls='ls -G'
alias ll='ls -Gltr'
alias fnd='ps ax | grep'
alias p23='sftp p23-athanasm@tsd-fx01.tsd.usit.no:/p23'
alias p274='sftp p274-athanasm@tsd-fx01.tsd.usit.no:/p274'
alias subl='/Applications/Sublime\ Text.app/Contents/MacOS/Sublime\ Text'
alias rstudio='open -a RStudio'
alias cat='ccat'


#NEW FUNCTIONS
function unmount { kill -9 $(ps ax | grep sshfs | grep $1 | cut -d " " -f1); umount -f $1; }

function convert_trans { convert -fuzz 10% $2 -transparent $1 ${2%.*}_trans.png; }

function catc { cat "$@" > /tmp/.tmp; pygmentize -g /tmp/.tmp; m /tmp/.tmp; }


#Functions
function FORM {
if [[ $# -eq 4 ]]; then k=$4; else k=1; fi
for ((i=$2; i<=$3; i++)); do
printf -v j "%03d" $i
printf -v l "%03d" $k
mv page_${j}.pdf ${1}_${l}.pdf
k=$((k+1))
done
}

function Merge_form {
for DOCS in $(ls *_[0-9][0-9].pdf | rev | cut -c 8- | rev | uniq); do
if [[ $DOCS = page ]] ; then NAME=Blank ; else NAME=$DOCS ; fi
echo $NAME
#This will throw some errors, but they can be ignored
gs -dNOPAUSE -q -sDEVICE=pdfwrite -sOUTPUTFILE=${NAME}.pdf -dBATCH ${DOCS}_*;
rm ${DOCS}_[0-9][0-9].pdf
done
}

function split_file {
#mkdir tiff/
gs -q -sDEVICE=pdfwrite -dSAFER -o page_%02d.pdf ${1}
#gs -q -sDEVICE=tiff24nc -dSAFER -o tiff/page_%03d.tiff ${1}
}

function add_foot_prot {
#mkdir Original
#cp $1 Original/$1
ID=${1:0:11} ; TP=${1:12:6} ; WAVE=$2
echo $ID $TP $WAVE

gs -dBATCH -dNOPAUSE -sDEVICE=pdfwrite -sOutputFile=${1%.*}_annot.pdf \
-c "<</EndPage {2 ne {0 setgray /Times-Bold 16 selectfont 350 800 moveto ($ID 	 $TP 	$WAVE) show pop true}{pop false} ifelse} >> setpagedevice" \
-f $1

}
export PATH="/usr/local/opt/icu4c/bin:$PATH"
export PATH="/usr/local/opt/icu4c/sbin:$PATH"

cat ~/.bashrc > .profile
