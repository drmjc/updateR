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
# 2011-11-17: updated to roxygen2 (v2.1) support
# 2012-02-21: added -x, --no-examples option; escapes only the unescaped '%' chars in Rd files.
#             added unescapedPercentWarning to test for unescaped % characters in Rd files.
# 2012-04-05: added -d deploy flag, originally from mjc's deploy.sh


################################################################################
# FUNCTIONS
################################################################################

# cause execution to terminate, with an optional message,
# and optional error code.
# $1: optional error message
# $2: optional error code. default = 1
die () {
	msg="######### ABORTING: ${1-Unexpected error}"
	exitcode=${2-1}
	echo >&2 "$msg"
	exit $exitcode
}

# deploy an R package in .tar.gz format to a host. this uses scp, ssh and R
# on the host machine.
# $1 = TGZ name
# $2 = host
# $3 = path to R on host
deploypkg () {
	TGZ=$1
	HOST=${2-enzo}
	R=${3-R}
	[ -f "$TGZ" ] || {die "$TGZ does not exist"}
	scp $TGZ ${USER}@${HOST}:/tmp
	ssh ${USER}@${HOST} "cd /tmp && ${R} --vanilla CMD INSTALL $TGZ && rm $TGZ"
}

usage() {
	cat << EOF
usage:
    updateR.sh -h
    updateR.sh [options] </path/to/package>
    updateR.sh [-l </usr/local/R/library>] [-r] [-c] [-x] [-s] [-b] [-w] [-g] [-m] [-d] [-i] [-d] </path/to/package>

OPTIONS
-r: roxygenize the package.
-c: R CMD CHECK the package.
-s: build source package
-b: build MAC binary package
-w: build WINDOWS binary package (currently unsupported).
-g: --no-vignettes
-m: --no-manual
-d: --no-docs
-x: --no-examples (R CMD CHECK)
-i: R CMD INSTALL the package
-d: deploy the source package to the default host
EOF
}

# print the values of the variables after parsing that getopt's
function debug_input () {
	cat <<EOF
	args="$@"
	num remaining args after reading options=$#
	PACKAGE_PATH=$PACKAGE_PATH
	PACKAGE=$PACKAGE
	PACKAGE_DIR= $PACKAGE_DIR
	R_LIB=$R_LIB
	ROXYGENIZE=$ROXYGENIZE
	CHECK=$CHECK
	SOURCE=$SOURCE
	BINARY=$BINARY
	WINBINARY=$WINBINARY
	INSTALL=$INSTALL
	TESTTHAT=$TESTTHAT
	OPTIONS=$OPTVAL
	HOST=$HOST
	DEPLOY=$DEPLOY
	OUT=$OUT
EOF
}

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
# - disable Rd creation via @noRd
function hiddenRdWarning {
	[ "$(find $1/man -type f -maxdepth 1 -name "\.*.Rd")" ] && echo >&2 "WARNING: hidden Rd file(s) detected. You should use @noRd, or @rdname tags & delete the offending files"
}

# look for unescaped '%' characters in Rd files. This should be run
# after roxygenizing the package.
# value: none. it prints warnings
function unescapedPercentWarning {
	matches=`egrep -l '[^\]%' $1/man/*Rd | grep -v ^$`
	if [ ${#matches[*]} -gt 0 ]; then
		for (( i=0; i<${#matches[*]}; i++ )); do
			eval arg=${matches[$i]}
			if [ ! -z "$arg" ]; then
				echo >&2 "WARNING: unescaped % found in: $arg"
			fi
		done
	fi
}

function roxygenize {
	Rscript --vanilla -e "if(require(methods) && require(roxygen2)) roxygenize(\"$1\")"
}

# Update the Date field in the DESCRIPTION file.
# Expects the format "Date: 2012-03-16"
function updatePackageDescriptionDateStamp {
	date=`date +%F`
	perl -pi -e "s/Date: +[0-9].*/Date: $date/" "$1"/DESCRIPTION
}

# run the testthat suite upon this package.
function test_package {
	pkg=`basename "$1"`
	Rscript --vanilla -e "suppressPackageStartupMessages(library(testthat)); suppressPackageStartupMessages(library($pkg)); test_package(\"$pkg\")"
}

################################################################################
################################################################################
################################################################################

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
TESTTHAT=1
OPTIONS=""
HOST=enzo
HOST_R="/misc/ICGCPancreas/bin/R"
DEPLOY=1

#
# process the arguments
#
if [ $# -eq "0" ]; then
	usage
	exit 1
fi

while getopts "l:rcsbwhgmoxtid" option; do
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
		o) OPTIONS="${OPTIONS} --no-docs";;
		x) OPTIONS="${OPTIONS} --no-examples";;
		t) TESTTHAT=0;;
		i) INSTALL=0;;
		d) DEPLOY=0;;
		h) usage; exit 1;;
		[?]) usage; exit 1;;
	esac
	# this should make $1 be the </path/to/package> argument
	shift $((OPTIND-1)); OPTIND=1
done

if [ $# -ne 1 ]; then
	echo "Last & only argument must be the /path/to/package"
	usage
	exit 10
fi

if [ $WINBINARY -eq 0 ]; then
	echo "Building windows binary currently unsupported. -w ignored."
	WINBINARY=1
fi

# make sure we have pdflatex, otherwise specify --no-manual
hash pdflatex 2>&- || OPTIONS="${OPTIONS} --no-manual --no-vignettes"

# we need to do at least 1 action
if [ $ROXYGENIZE -eq 1 -a $CHECK -eq 1 -a $SOURCE -eq 1 -a $BINARY -eq 1 -a $WINBINARY -eq 1 -a $INSTALL -eq 1 -a $TESTTHAT -eq 1 -a $DEPLOY -eq 1 ]; then
	echo "You must specify at least one action: -r, -c, {-b,-s,-w}, -i, -t, -d"
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
# file to capture the stdout
OUT=`mktemp -t updateR.XXXXX`

# debug_input && exit

################################################################################
# Parse the package DESCRIPTION file to determine the package version.
#
PKGVERSION=`grep Version "${PACKAGE_DIR}/${PACKAGE}/DESCRIPTION" | sed 's/Version: //'`
FILE_TO_INSTALL="${PACKAGE}_${PKGVERSION}.tar.gz"

################################################################################
# Update the Date field in the DESCRIPTION file, IF we are performing a modifier
# option.
if [[ $ROXYGENIZE -eq 0 || $SOURCE -eq 0 || $BINARY -eq 0 ]]; then
	updatePackageDescriptionDateStamp "${PACKAGE_PATH}"
fi

################################################################################
# Roygenize the package.
#
if [ $ROXYGENIZE -eq 0 ]; then
	echo "Roxygenizing ${PACKAGE_PATH}..."
	RDTMP=`mktemp -d -t updateR.XXXXX`

	# R --vanilla CMD roxygen2 is not supported <yet>
	# # is roxygen installed?
	# R --vanilla CMD roxygen2 &> /dev/null
	# if [ $? -ne 1 ]; then
	# 	echo "roxygen is not installed."
	# 	exit 198
	# fi
	
	hiddenRdWarning ${PACKAGE_PATH}
	# backup those files that roxygen will edit
	backupRoxygen ${PACKAGE_PATH} ${RDTMP}
	
	# roxygenize
	roxygenize "${PACKAGE_PATH}"  > $OUT 2>&1
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
			# roxygen2 gets this right in the usage section, but not so well in arguments or other
			# section -- should this be up to the author??
			perl -pi -e 's/([^\\])%/$1\\%/' ${PACKAGE_PATH}/man/*Rd
			perl -pi -e 's/^%/\\%/' ${PACKAGE_PATH}/man/*Rd
			perl -pi -e 's/export\((.*<-)\)/export("$1")/' ${PACKAGE_PATH}/NAMESPACE
		fi
	fi
	
	hiddenRdWarning ${PACKAGE_PATH}
	unescapedPercentWarning ${PACKAGE_PATH}
fi

################################################################################
# R CMD CHECK The package folder. 
# @TODO R CMD CHECK on the .tar.gz using --as-cran?
#
if [ $CHECK -eq 0 ]; then
	echo "Checking ${PACKAGE_PATH}..."

	R --vanilla CMD check ${OPTIONS} ${PACKAGE_PATH} > $OUT 2>&1
	if [ $? -ne 0 ]; then
		echo >&2 "R CMD CHECK failed."
		cat >&2 $OUT
		rm $OUT
		exit 205
	else
		rm -rf "${PACKAGE_PATH}.Rcheck"

		hiddenRdWarning ${PACKAGE_PATH}
		unescapedPercentWarning ${PACKAGE_PATH}
	fi
fi

################################################################################
# build the binary package
# note this is b4 SOURCE, since if user specified both BINARY and SOURCE, I want
# the SOURCE tar.gz to be installed/tested/deployed.
if [ $BINARY -eq 0 ]; then
	FILE_TO_INSTALL="${PACKAGE}_${PKGVERSION}.tgz"
	echo "Building *binary* ${PACKAGE} as ${FILE_TO_INSTALL}..."
	
	cd "$PACKAGE_DIR"
	R --vanilla CMD build $OPTIONS --binary $PACKAGE > $OUT 2>&1
	if [ $? -ne 0 ]; then
		cat $OUT
		echo "Building *binary* failed; no installation performed"
		rm $OUT
		exit 201
	fi

	# At some point, on a mac, running "R CMD build --binary ..." created files named
	# lumidat_1.0_R_i386-apple-darwin9.8.0.tar.gz
	# rename these to lumidat_1.0.tgz
	LONGNAME=`find . -maxdepth 1 -type f -name "${PACKAGE}_${PKGVERSION}_R_i386-apple-darwin*.tgz" -print`
	# LONGNAME="${PACKAGE}_${PKGVERSION}_R_i386-apple-darwin9.8.0.tgz"
	if [ $LONGNAME ]; then
		mv -f "$LONGNAME" "$FILE_TO_INSTALL"
	fi
fi

################################################################################
# build the windows binary package
# ---- UNSUPPORTED -----
# if [ $WINBINARY ]; then
#	echo "Building windows binary ${PACKAGE}..."
#	cd "$PACKAGE_DIR"
#	R --vanilla CMD build --binary $PACKAGE > /dev/stdout
# fi

################################################################################
# build the source package
# 
if [ $SOURCE -eq 0 ]; then
	FILE_TO_INSTALL="${PACKAGE}_${PKGVERSION}.tar.gz"
	echo "Building *source* ${PACKAGE} as ${FILE_TO_INSTALL}..."
	cd "$PACKAGE_DIR"
	R --vanilla CMD build $OPTIONS $PACKAGE > $OUT 2>&1
	if [ $? -ne 0 ]; then
		cat >&2 $OUT
		echo >&2 "Building *source* failed; no installation performed"
		rm $OUT
		exit 200
	fi
fi

################################################################################
# install the package?
# 
if [ $INSTALL -eq 0 ]; then
	[[ "$FILE_TO_INSTALL" != "" ]] && [[ -e "$FILE_TO_INSTALL" ]] || die "INSTALL is true, but FILE_TO_INSTALL is not set properly"
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

################################################################################
# run TESTTHAT suite?
# 
if [ $TESTTHAT -eq 0 ]; then
	echo "Testing ${PACKAGE}..."
	test_package ${PACKAGE_PATH} || die "package TEST failed"
fi

################################################################################
# DEPLOY package to HOST?
# 
if [ $DEPLOY -eq 0 ]; then
	echo "Deploying ${FILE_TO_INSTALL}..."
	deploypkg ${FILE_TO_INSTALL} $HOST $HOST_R || die "package DEPLOY failed"
fi

################################################################################

exit 0
