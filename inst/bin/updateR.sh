#!/bin/bash
#
# Generic shell script to build and install R packages.
# For a given package, you can generate roxygen documentation, then do an 
# R CMD CHECK, R CMD build (source/binary/windows), then R CMD install.
# All you need is the path to the package itself.
#
# usage:
#	updateR.sh [options] /path/to/package
#
# todo: implement a versioning updater system
# todo: update the datestamp in the DESCRIPTION file
# todo: warn about running roxygen if it's not listed as a dependency in the DESCRIPTION
#
# Mark Cowley
#
# CHANGELOG:
# 2008-11-10: v1
# 2009-06-15: made this cross platform by choosing the R_LIB dependant upon the underlying O/S
# 2009-10-12: made this a generic installer for all packages.
# 2009-12-02: major revisions. now far fewer arguments hardwired.
# 2010-01-12: support for building binary packages; switched to using getopts and -l and -m flags.
# 2011-04-11: added flags to control --no-vignettes --no-manual --no-docs
# 2011-06-03: added roxygen support
# 2011-09-21: added --vanilla flag, to skip reading .Rprofile.
usage() {
	cat << EOF
usage:
    updateR.sh -h
    updateR.sh [options] </path/to/package>
    updateR.sh [-l </usr/local/R/library>] [-r] [-c] [-s] [-b] [-w] [-g] [-m] [-d] [-i] </path/to/package>

OPTIONS
-r: roxygenize the package.
-c: R CMD CHECK the package.
-s: build source package
-b: build MAC binary package
-w: build WINDOWS binary package (currently unsupported).
-g: --no-vignettes
-m: --no-manual
-d: --no-docs
-i: R CMD INSTALL the package
EOF
}

# set default values for arguments
PACKAGE_PATH=""
PACKAGE=""
PACKAGE_DIR=""
R_LIB=""
ROXYGENIZE=1
CHECK=1
SOURCE=1
BINARY=1
WINBINARY=1
INSTALL=1
OPTIONS=""

#
# process the arguments
#
if [ $# -eq "0" ]; then
	usage
	exit 1
fi

while getopts "l:rcsbwhgmdi" option; do
	# echo "Processing option $OPTARG $OPTIND $OPTVAL"
	case "${option}" in
		l) R_LIB="${OPTARG}";;
		r) ROXYGENIZE=0;;
		c) CHECK=0;;
		s) SOURCE=0;;
		b) BINARY=0;;
		w) WINBINARY=0;;
		g) OPTIONS="${OPTIONS} --no-vignettes";;
		m) OPTIONS="${OPTIONS} --no-manual";;
		d) OPTIONS="${OPTIONS} --no-docs";;
		i) INSTALL=0;;
		h) usage; exit 1;;
		[?]) usage; exit 1;;
	esac
	# this should make $1 be the </path/to/package> argument
	shift $((OPTIND-1)); OPTIND=1
done

if [ $# -ne 1 ]; then
	echo "Was expecting the final argument to be the /path/to/package"
	usage
	exit 10
fi

if [ $WINBINARY -eq 0 ]; then
	echo "Building windows binary currently unsupported. -w ignored."
	WINBINARY=1
fi

# make sure we have pdflatex, otherwise specify --no-manual
hash pdflatex 2>&- || OPTIONS="${OPTIONS} --no-manual"

# we need to do at least 1 action
if [ $ROXYGENIZE -eq 1 -a $CHECK -eq 1 -a $SOURCE -eq 1 -a $BINARY -eq 1 -a $WINBINARY -eq 1 -a $INSTALL -eq 1 ]; then
	echo "You must specify at least one action: -r, -c, {-b,-s,-w}, -i"
	usage
	exit 10
fi

PACKAGE_PATH="$1"
if [ ! -d "${PACKAGE_PATH}" ]; then
	echo "Package directory not found. Looking for: '${PACKAGE_PATH}'"
	exit 2
fi
PACKAGE=$(basename ${PACKAGE_PATH})
PACKAGE_DIR=$(dirname ${PACKAGE_PATH})

# cat <<EOF
# num remaining args=$#
# args="$@"
# PACKAGE_PATH=$PACKAGE_PATH
# PACKAGE=$PACKAGE
# PACKAGE_DIR= $PACKAGE_DIR
# R_LIB=$R_LIB
# SOURCE=$SOURCE
# BINARY=$BINARY
# WINBINARY=$WINBINARY
# EOF
# exit
######################

#
# Actions based on specific arguments
#

# R_LIB_ARGS=""
# if [ $R_LIB ] && [ ! -d "$R_LIB" ]; then
#	  echo "$R_LIB does not exist. If in doubt, don't supply a -l argument and let R work out where to install the package."
#	  exit 3
# elif [ $R_LIB ]; then
#	  R_LIB_ARGS="-l $R_LIB"
# else
#	  R_LIB_ARGS=""
# fi

# file to capture the stdout
OUT=`mktemp -t updateR.XXXXX`


# backup/reinstate those files that roxygen will perturb
# during an R CMD ROXYGENIZE
function backupRoxygen {
	# $1 = from, $2 = to
	cp -f $1/DESCRIPTION $2/DESCRIPTION
	[ -f $1/NAMESPACE ] && cp -f $1/NAMESPACE $2/NAMESPACE
	[ -d $1/man ] && [ "$(ls -A $1/man/*Rd)" ] && mkdir $2/man && mv -f $1/man/*Rd $2/man
}

# If you document a hidden function, you can end up with a hidden Rd file, which
# a) is missed by R CMD CHECK
# b) complicates all the copy commands below.
# You can avoid this via:
# - specify the Rd filename via @rdname, or 
# - disable Rd creation via @nord
function hiddenRdWarning {
	[ "$(find $1/man -type f -maxdepth 1 -name "\.*.Rd")" ] && echo "WARNING: hidden Rd file(s) detected. You should use @nord, or @rdname tags & delete the offending files"
}

#
# Roygenize the package.
#
RDTMP=`mktemp -d -t updateR.XXXXX`
if [ $ROXYGENIZE -eq 0 ]; then
	echo "Roxygenizing ${PACKAGE}..."

	# is roxygen installed?
	R --vanilla CMD roxygen &> /dev/null
	if [ $? -ne 1 ]; then
		echo "roxygen is not installed."
		exit 198
	fi
	
	hiddenRdWarning ${PACKAGE_PATH}
	
	# backup those files that roxygen will edit
	backupRoxygen ${PACKAGE_PATH} ${RDTMP}
	
	# roxygenize
	R --vanilla CMD roxygen -d ${PACKAGE_PATH} > $OUT 2>&1
	roxOK=$?
	numRd=`ls ${PACKAGE_PATH}/man | wc -l`
	if [ $roxOK -ne 0 ]; then
		cat >&2 $OUT
		rm $OUT
		echo >&2 "roxygenize failed. restoring previous DESCRIPTION, NAMESPACE, Rd files"
		rm -rf ${PACKAGE_PATH}/man/*Rd ${PACKAGE_PATH}/NAMESPACE ${PACKAGE_PATH}/DESCRIPTION
		backupRoxygen ${RDTMP} ${PACKAGE_PATH}
		exit 199
	else
		if [ $numRd -eq 0 ]; then
			cat "roxygen created no Rd files. restoring previous DESCRIPTION, NAMESPACE, Rd files"
			rm -rf ${PACKAGE_PATH}/man/*Rd ${PACKAGE_PATH}/NAMESPACE ${PACKAGE_PATH}/DESCRIPTION
			backupRoxygen ${RDTMP} ${PACKAGE_PATH}
		else
			rm -rf ${RDTMP}/man ${RDTMP}/NAMESPACE ${RDTMP}/DESCRIPTION
			
			# Delete empty inst/doc folder (rmdir won't delete a dir if it's not empty)
			rmdir ${PACKAGE_PATH}/inst/doc 2> /dev/null
			
			# % characters in Rd files need to be escaped (\%).
			# unescaped %'s can sneak in, eg if a default param has a % in it, which ends up in the \usage section of Rd
			perl -pi -e 's/%/\\%/' ${PACKAGE_PATH}/man/*Rd
			perl -pi -e 's/export\((.*<-)\)/export("$1")/' ${PACKAGE_PATH}/NAMESPACE
		fi
	fi
	
	hiddenRdWarning ${PACKAGE_PATH}
fi

#
# CHECK The directory
#
if [ $CHECK -eq 0 ]; then
	echo "Checking ${PACKAGE}..."

	R --vanilla CMD check ${OPTIONS} ${PACKAGE_PATH} > $OUT 2>&1
	if [ $? -ne 0 ]; then
		echo >&2 "R CMD CHECK failed."
		cat >&2 $OUT
		rm $OUT
		exit 205
	else
		rm -rf "${PACKAGE_PATH}.Rcheck"
	fi
fi

#
# Parse the package DESCRIPTION file to determine the package version.
#
PKGVERSION=`grep Version "${PACKAGE_DIR}/${PACKAGE}/DESCRIPTION" | sed 's/Version: //'`
FILE_TO_INSTALL="${PACKAGE}_${PKGVERSION}.tar.gz"

# build the source package
if [ $SOURCE -eq 0 ]; then
	echo "Building *source* ${PACKAGE}..."
	cd "$PACKAGE_DIR"
	R --vanilla CMD build $OPTIONS $PACKAGE > $OUT 2>&1
	if [ $? -ne 0 ]; then
		cat >&2 $OUT
		echo >&2 "Building *source* failed; no installation performed"
		rm $OUT
		exit 200
	fi
	FILE_TO_INSTALL="${PACKAGE}_${PKGVERSION}.tar.gz"
fi

# build the binary package
if [ $BINARY -eq 0 ]; then
	echo "Building *binary* ${PACKAGE}..."
	cd "$PACKAGE_DIR"
	R --vanilla CMD build $OPTIONS --binary $PACKAGE > $OUT 2>&1
	if [ $? -ne 0 ]; then
		cat $OUT
		echo "Building *binary* failed; no installation performed"
		rm $OUT
		exit 201
	fi

	PKGNAME="${PACKAGE}_${PKGVERSION}.tgz"
	
	# At some point, on a mac, running "R CMD build --binary ..." created files named
	# lumidat_1.0_R_i386-apple-darwin9.8.0.tar.gz
	# rename these to lumidat_1.0.tgz
	LONGNAME=`find . -maxdepth 1 -type f -name "${PACKAGE}_${PKGVERSION}_R_i386-apple-darwin*.tgz" -print`
	# LONGNAME="${PACKAGE}_${PKGVERSION}_R_i386-apple-darwin9.8.0.tgz"
	if [ $LONGNAME ]; then
		mv -f "$LONGNAME" "$PKGNAME"
	fi

	# if only -b was set, then point the tgz file to be installed in next step
	if [[ $FILE_TO_INSTALL = "" ]]; then
		FILE_TO_INSTALL=$PKGNAME
	fi
fi

# # build the windows binary package
# if [ $WINBINARY ]; then
#	echo "Building windows binary ${PACKAGE}..."
#	cd "$PACKAGE_DIR"
#	R --vanilla CMD build --binary $PACKAGE > /dev/stdout
# fi

# install the package?
if [ $INSTALL -eq 0 ]; then
	echo "Installing ${FILE_TO_INSTALL}..."
	# install the package: if $R_LIB is undefined, install there, else install to the default
	if [ -n "$R_LIB" ]; then
		R --vanilla CMD INSTALL -l $R_LIB $FILE_TO_INSTALL > $OUT 2>&1
	else
		R --vanilla CMD INSTALL $FILE_TO_INSTALL > $OUT 2>&1
	fi
	if [ $? -ne 0 ]; then
		cat $OUT
		rm $OUT
		exit 202
	fi
fi

exit 0
