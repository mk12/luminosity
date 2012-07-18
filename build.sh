#!/bin/bash

usage() {
cat <<EOS
Usage: $0 [-tpw]

This script compiles Luminosity using GHC.

Options:
  -h    Show this message
  -t    Compile with -threaded (parallelized)
  -p    Compile with -prof (for profiling)
  -w    Compile with -Wall (warnings)
EOS
}

# Robust change to current directory
SOURCE="${BASH_SOURCE[0]}"
DIR="$( dirname "$SOURCE" )"
while [ -h "$SOURCE" ]
do 
	SOURCE="$(readlink "$SOURCE")"
	[[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
	DIR="$( cd -P "$( dirname "$SOURCE"  )" && pwd )"
done
cd -P "$( dirname "$SOURCE" )"

# Options
THREADED=
PROF=
WARN=

# Getopts
while getopts ":htpw" OPTION; do
	case $OPTION in
		h)
			usage
			exit
			;;
		t)
			THREADED="-threaded -rtsopts -feager-blackholing -fforce-recomp"
			;;
		p)
			PROF="-prof -auto-all -caf-all -fforce-recomp"
			;;
		w)
			WARN="-Wall"
			;;
		"?")
			usage
			exit 1
			;;
	esac
done

# Build Directory
if [[ ! -d "build" ]]; then
	mkdir build
fi

# Compile
eval "ghc --make -O2 -fexcess-precision -funbox-strict-fields $THREADED $PROF $WARN ./Main.hs -o ./luminosity -odir ./build/ -hidir ./build/"
