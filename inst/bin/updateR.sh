#!/bin/bash
#
# Generic shell script to build, install, check, test & deploy R packages.
# 
# For the path to the source code directory, you can:
# generate roxygen2 documentation (-r), 
# R CMD CHECK the source code (-c),
# R CMD build (source -s/binary -b/windows -w),
# R CMD CHECK the bundle (-C),
# R CMD INSTALL the bundle (-i),
# run testthat suite (-t)
# deploy (ie remote R CMD INSTALL, -d) the bundle
# 
# Cool features:
# if roxygen2 fails, the old Rd/NAMESPACE/DESCRIPTION files are re-instated
# the Date in the DESCRIPTION file is auto-updated
# you can turn off the creation of manuals & vignettes for speed
# you can turn off exmaple testing in the CHECK's
# R CMD CHECK hides the results of the 'OK' tests & only shows the Notes, Warnings & Errors
# it's svn friendly
# -- ie it doesn't go building in different locations & killing the svn history
# you can build source and binary packages simultaneously
# -- though if you choose both (-b -s), then only the source package will be checked (-C), installed (-i), tested (-t), or deployed (-d).
#    If you want these things to happen for the binary package, then you can't use -s
# it cleans up the build/partial.rds files which are created during BUILD, if \Sexpr's are found
# 
# Notes:
# Currently we don't build windows binary packages, but the options are in place to do so.
# 
# usage:
#	updateR.sh -h
# 
# Mark Cowley
################################################################################

################################################################################
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
# 2012-06-14: added R CMD CHECK --as-cran on the package bundle
# 2012-07-10: v1.1.0
#             determine package name by parsing DESCRIPTION, not from basename $PACKAGE_PATH
#             changed deploy to accept a string argument. tested with hostname, username@hostname
#             updated BINARY package building to work in R >= 2.14
#             only allow BINARY (-b) or SOURCE (-s), not both.
#             simplified roxygenize()
#             delete pkname/build folders which are created at BUILD time, when \Sexpr's are found
# 2012-07-18: allow the R CMD CHECK's to print notes and warnings
# 2013-07-29: update HOST_R to R-2.15.2; Default HOST = gamma00
#             update deploypkg to check if HOST_R is available
#             added -v & $VERBOSE variables (to match the OPTIONS as documented)
# 2013-08-26: enforce R CMD INSTALL --no-multiarch. TODO: allow this to be toggled via cmd line.
# 
################################################################################

################################################################################
# TODO:
# - implement a versioning updater system
# - windows binary package building
# - add a -p password option for deploying to remote hosts
# - force minimum R version to 2.14.0 in DESCRIPTION
################################################################################


################################################################################
# REFERENCES
# building source bundles: http://cran.r-project.org/doc/manuals/R-exts.html#Building-binary-packages
# build/partial.rdb files : http://cran.r-project.org/doc/manuals/R-exts.html#Building-package-tarballs
# 
################################################################################


################################################################################
# FUNCTIONS
################################################################################
usage () {
	cat << EOF
usage:
    updateR.sh -h
    updateR.sh [options] </path/to/package>
    updateR.sh [-l </usr/local/R/library>] [-v] [-r] [-c] [-x] [-s] [-b] [-w] [-g] [-m] [-C] [-i] [-t] [-d username@hostname] </path/to/package>

OPTIONS
-v: verbose
-r: roxygenize the package source code.
-c: R CMD CHECK the package source code.
-s: build source package. -s,-b are mutually exclusive
-b: build MAC binary package. -s,-b are mutually exclusive
-w: build WINDOWS binary package (currently unsupported).
-g: --no-vignettes (R CMD build)
-m: --no-manual    (R CMD build)
-d: --no-docs      (R CMD build)
-x: --no-examples  (R CMD CHECK)
-C: R CMD CHECK --as-cran on the package bundle.
-i: R CMD INSTALL the package
-t: testthat the installed version of the package (runs after -i)
-d: deploy the source package to a remote host. You can specify just the hostname, or username@hostname
EOF
}

# cause execution to terminate, with an optional message,
# and optional error code.
# $1: optional error message
# $2: optional error code. default = 1
die () {
	msg="######### ABORTING: ${1-Unexpected error}"
	exitcode=${2-1}
	echo >&2 "$msg"
	[[ "$3" -ne "1" ]] && usage
	exit $exitcode
}

# print the values of the variables after parsing that getopt's
function debug_input () {
	cat <<EOF
	args="$@"
	num remaining args after reading options=$#
	PACKAGE_NAME=$PACKAGE_NAME
	PACKAGE_PATH=$PACKAGE_PATH
	PACKAGE_DIR=$PACKAGE_DIR
	PACKAGE_VERSION=$PACKAGE_VERSION
	R_LIB=$R_LIB
	ROXYGENIZE=$ROXYGENIZE
	CHECK_SOURCE=$CHECK_SOURCE
	CHECK_PACKAGE=$CHECK_PACKAGE
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
# $1 = package path
# $2 = temp backup path
function backupRoxygen {
	cp -f $1/DESCRIPTION $2/DESCRIPTION
	[[ -f $1/NAMESPACE ]] && cp -f $1/NAMESPACE $2/NAMESPACE
	# note you can't just mv these folders around, as you'll mess with any .svn folders.
	if [[ -d $1/man ]]; then
		if [[ ! -d $2/man ]]; then
			mkdir $2/man
		fi
		find $1/man -type f -iname '*Rd' -exec mv {} $2/man \;
	fi
}

# restore a previously backed up set of Rd,NAMESPACE,DESCRIPTION files
# $1 = package path
# $2 = temp backup path
function restoreRoxygen {
	find $1/man -type f -iname '*Rd' -delete
	backupRoxygen ${2} ${1}
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

# extra default-packages allow this to work under R >= 2.12
function roxygenize {
	Rscript --vanilla --default-packages=roxygen2,utils,stats,methods -e "roxygenize(\"$1\")"
}

# Update the Date field in the DESCRIPTION file.
# Expects the format "Date: 2012-03-16"
function updatePackageDescriptionDateStamp {
	date=`date +%F`
	perl -pi -e "s/Date: +[0-9].*/Date: $date/" "$1"/DESCRIPTION
}

# run the testthat suite upon the installed package.
function test_package {
	# pkg=`basename "$1"`
	pkg="$1"
	# Rscript --vanilla --default-packages=testthat -e "suppressPackageStartupMessages(require($pkg)) || stop(\"$pkg is not installed\"); test_package(\"$pkg\")"
	# TODO: update evaluate to Import the appropriate packages
	Rscript --vanilla --default-packages=grDevices,utils,methods,testthat,$1 -e "test_package(\"$1\")"
}


# deploy an R package in .tar.gz format to a host. this uses scp, ssh and R
# on the host machine.
# $1 = PKG_BUNDLE name (.tar.gz or .tgz if deploying to OSX)
# $2 = [username@]host (default=marcow@gamma00)
# $3 = path to R on host
deploypkg () {
	PKG_BUNDLE=$1
	USER_HOST=${2-${USER}@gamma00}
	R=${3-R}
	[ -f "$PKG_BUNDLE" ] || die "$PKG_BUNDLE does not exist" 101

	cmd="ssh ${USER_HOST} \"which $R\""
	[[ $VERBOSE -eq 0 ]] && echo $cmd
	eval "$cmd" > /dev/null || die "Could not find R on remote host" 104

	cmd="scp $PKG_BUNDLE ${USER_HOST}:/tmp"
	[[ $VERBOSE -eq 0 ]] && echo $cmd
	eval "$cmd" || die "Could not scp bundle across to $HOST" 102

	cmd="ssh ${USER_HOST} \"cd /tmp && ${R} --vanilla CMD INSTALL $PKG_BUNDLE && rm $PKG_BUNDLE\""
	[[ $VERBOSE -eq 0 ]] && echo $cmd
	eval "$cmd" || die "Couldn't install bundle on $HOST" 103
}


################################################################################
################################################################################
################################################################################

# set default values for arguments
PACKAGE_PATH=""
PACKAGE_NAME="" # set by parsing DESCRIPTION
PACKAGE_DIR=""
PACKAGE_VERSION="" # set by parsing DESCRIPTION
R_LIB=""
ROXYGENIZE=1
CHECK_SOURCE=1
CHECK_PACKAGE=1
SOURCE=1
BINARY=1
WINBINARY=1
INSTALL=1
TESTTHAT=1
OPTIONS=""
HOST=""
HOST_R="/share/ClusterShare/software/contrib/marcow/R/2.15.2/bin/R"
DEPLOY=1
VERBOSE=1
MULTIARCH="--no-multiarch"

#
# process the arguments
#
if [ $# -eq "0" ]; then
	die "no arguments found" 1
fi

while getopts "l:rcsbwhgmoxCitvd:" option; do
	# echo "Processing option $OPTARG $OPTIND $OPTVAL"
	case "${option}" in
		l) R_LIB="${OPTARG}";;
		r) ROXYGENIZE=0;;
		c) CHECK_SOURCE=0;;
		s) SOURCE=0;;
		b) BINARY=0;;
		w) WINBINARY=0;;
		g) OPTIONS="${OPTIONS} --no-vignettes";;
		m) OPTIONS="${OPTIONS} --no-manual";;
		o) OPTIONS="${OPTIONS} --no-docs";;
		x) OPTIONS="${OPTIONS} --no-examples";;
		C) CHECK_PACKAGE=0;;
		i) INSTALL=0;;
		t) TESTTHAT=0;;
		v) VERBOSE=0;;
		d) DEPLOY=0;
		   HOST=${OPTARG};;
		h) usage; exit 1;;
		[?]) usage; exit 1;;
	esac
	# this should make $1 be the </path/to/package> argument
	shift $((OPTIND-1)); OPTIND=1
done

if (( $# != 1 )); then
	if [[ $DEPLOY -eq 0 && -d "$HOST" ]]; then
		die "You forgot to specify the hostname after -d" 1
	else
		die "Last & only argument must be the /path/to/package" 2
	fi
fi

if [ $WINBINARY -eq 0 ]; then
	echo "Building windows binary currently unsupported. -w ignored."
	WINBINARY=1
fi

[[ $BINARY -eq 0 && $SOURCE -eq 0 ]] && die "Can only build source (-s) OR binary (-b) packages" 11

# make sure we have pdflatex, otherwise specify --no-manual
hash pdflatex 2>&- || OPTIONS="${OPTIONS} --no-manual --no-vignettes"

# we need to do at least 1 action
if [ $ROXYGENIZE -eq 1 -a $CHECK_SOURCE -eq 1 -a $CHECK_PACKAGE -eq 1 -a $SOURCE -eq 1 -a $BINARY -eq 1 -a $WINBINARY -eq 1 -a $INSTALL -eq 1 -a $TESTTHAT -eq 1 -a $DEPLOY -eq 1 ]; then
	die "You must specify at least one action: -r, -c, {-b,-s,-w}, -C, -i, -t, -d" 12
fi

PACKAGE_PATH="$1"
[[ -d "${PACKAGE_PATH}" ]] || die "Package directory not found. Looking for: '${PACKAGE_PATH}'" 13

################################################################################
# Parse the package DESCRIPTION file to determine the package version.
#
PACKAGE_DIR=$(dirname ${PACKAGE_PATH})
PACKAGE_VERSION=`grep ^Version: "${PACKAGE_PATH}/DESCRIPTION" | sed 's/Version: //'`
PACKAGE_NAME=`grep ^Package: "${PACKAGE_PATH}/DESCRIPTION" | sed 's/Package: //'`
FILE_TO_INSTALL="${PACKAGE_NAME}_${PACKAGE_VERSION}.tar.gz"
# file to capture the stdout
OUT=`mktemp -t updateR.XXXXX`

echo "* this is package '$PACKAGE_NAME' version '$PACKAGE_VERSION'"

# debug_input && exit

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
	echo "Roxygenizing ${PACKAGE_NAME}..."
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
		restoreRoxygen ${PACKAGE_PATH} ${RDTMP}
		die "roxygenize failed. restoring previous DESCRIPTION, NAMESPACE, Rd files" 21
	else
		if [ $numRd -eq 0 ]; then
			cat "roxygen created no Rd files. restoring previous DESCRIPTION, NAMESPACE, Rd files"
			restoreRoxygen ${PACKAGE_PATH} ${RDTMP}
		else
			rm -rf ${RDTMP}/man ${RDTMP}/NAMESPACE ${RDTMP}/DESCRIPTION
			
			# Delete empty inst/doc folder (rmdir won't delete a dir if it's not empty)
			rmdir ${PACKAGE_PATH}/inst/doc 2> /dev/null
			
			# % characters in Rd files need to be escaped (\%).
			# roxygen2 gets this right in the usage section, but not so well in arguments or other
			# section -- should this be up to the author??
			perl -pi -e 's/([^\\])%/$1\\%/g' ${PACKAGE_PATH}/man/*Rd
			perl -pi -e 's/^%/\\%/' ${PACKAGE_PATH}/man/*Rd
			perl -pi -e 's/export\((.*<-)\)/export("$1")/g' ${PACKAGE_PATH}/NAMESPACE
		fi
	fi
	
	hiddenRdWarning ${PACKAGE_PATH}
	unescapedPercentWarning ${PACKAGE_PATH}
fi

################################################################################
# R CMD CHECK The package folder. 
#
if [ $CHECK_SOURCE -eq 0 ]; then
	echo "Checking *source* ${PACKAGE_NAME}..."

	cd "$PACKAGE_DIR"
	R --vanilla CMD check ${OPTIONS} ${PACKAGE_PATH} 2>&1 | tee $OUT | egrep -v "OK$|^\* using|^\* checking extension type ... Package|^\* this is package"
	grep "ERROR" $OUT >/dev/null
	if [ $? -eq 0 ]; then
		cat >&2 $OUT
		rm $OUT
		die "R CMD CHECK failed" 25 1
	else
		rm -rf "${PACKAGE_PATH}.Rcheck"

		hiddenRdWarning ${PACKAGE_PATH}
		unescapedPercentWarning ${PACKAGE_PATH}
	fi
fi

################################################################################
# build the binary package
# in R 2.14 `R CMD build --binary` was deprecated. Instead `R CMD INSTALL --build`
# first installs either a source bundle, or source dir, and then creates a
# tgz binary bundle.
if [ $BINARY -eq 0 ]; then
	FILE_TO_INSTALL="${PACKAGE_NAME}_${PACKAGE_VERSION}.tgz"
	echo "Building *binary* ${PACKAGE_NAME} as ${FILE_TO_INSTALL}..."
	
	cd "$PACKAGE_DIR"
	R --vanilla CMD INSTALL --build $OPTIONS ${PACKAGE_PATH} > $OUT 2>&1
	if [ $? -ne 0 ]; then
		cat >&2 $OUT
		rm $OUT
		die "Building *binary* failed; no installation performed" 31 1
	fi
fi

################################################################################
# build the windows binary package
# ---- UNSUPPORTED -----
# if [ $WINBINARY ]; then
#	echo "Building windows binary ${PACKAGE_NAME}..."
#	cd "$PACKAGE_DIR"
#	R --vanilla CMD build --binary ${PACKAGE_NAME} > /dev/stdout
# fi

################################################################################
# build the source package
# 
if [ $SOURCE -eq 0 ]; then
	FILE_TO_INSTALL="${PACKAGE_NAME}_${PACKAGE_VERSION}.tar.gz"
	echo "Building *source* ${PACKAGE_NAME} as ${FILE_TO_INSTALL}..."
	cd "$PACKAGE_DIR"
	R --vanilla CMD build $OPTIONS ${PACKAGE_PATH} > $OUT 2>&1
	if [ $? -ne 0 ]; then
		cat >&2 $OUT
		rm $OUT
		die "Building *source* failed; no installation performed" 35 1
	fi
	if [[ -d "${PACKAGE_DIR}/build" ]]; then
		rm -rf "${PACKAGE_DIR}/build"
	fi
fi

################################################################################
# R CMD CHECK The package bundle. 
#
if [ $CHECK_PACKAGE -eq 0 ]; then
	echo "Checking *bundle* ${FILE_TO_INSTALL}..."

	R --vanilla CMD check ${OPTIONS} --as-cran ${FILE_TO_INSTALL} 2>&1 | tee $OUT | egrep -v "OK$|^\* using|^\* checking extension type ... Package|^\* this is package"
	grep "ERROR" $OUT >/dev/null
	if [ $? -eq 0 ]; then
		cat >&2 $OUT
		rm $OUT
		die "R CMD CHECK on bundle failed" 39 1
	else
		rm -rf "${PACKAGE_PATH}.Rcheck"
	fi
fi
################################################################################
# install the package?
# 
if [ $INSTALL -eq 0 ]; then
	[[ "$FILE_TO_INSTALL" != "" ]] && [[ -e "$FILE_TO_INSTALL" ]] || die "INSTALL is true, but FILE_TO_INSTALL is not set properly" 41
	echo "Installing ${FILE_TO_INSTALL}..."
	# install the package: if $R_LIB is undefined, install there, else install to the default
	if [ -n "$R_LIB" ]; then
		R --vanilla CMD INSTALL $MULTIARCH -l $R_LIB $FILE_TO_INSTALL > $OUT 2>&1
	else
		R --vanilla CMD INSTALL $MULTIARCH $FILE_TO_INSTALL > $OUT 2>&1
	fi
	if [ $? -ne 0 ]; then
		cat $OUT
		rm $OUT
		die "R CMD INSTALL failed" 43 1
	fi
fi

################################################################################
# run TESTTHAT suite?
# 
if [ $TESTTHAT -eq 0 ]; then
	echo "Testing ${PACKAGE_NAME}..."
	test_package ${PACKAGE_NAME} || die "package TEST failed" 51 1
fi

################################################################################
# DEPLOY package to HOST?
# 
if [ $DEPLOY -eq 0 ]; then
	echo "Deploying ${FILE_TO_INSTALL}..."
	deploypkg ${FILE_TO_INSTALL} $HOST $HOST_R || die "package DEPLOY failed" 61
fi

################################################################################

exit 0
