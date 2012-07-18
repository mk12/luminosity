#!/bin/bash

usage() {
cat <<EOS
usage: $0 [-tpw]

This script compiles NAME using GHC.

OPTIONS:
  -h    Show this message
  -t    Compile with -threaded (parallelized)
  -p    Compile with -prof (for profiling)
  -w    Compile with -Wall (warnings)
EOS
}

THREADED=
PROF=
WARN=

while getopts ":htpw" OPTION; do
	case $OPTION in
		h)
			usage
			exit 1
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
			exit
			;;
	esac
done

if [[ ! -d "build" ]]; then
	mkdir build
fi

eval "ghc --make -O -funbox-strict-fields $THREADED $PROF $WARN Main.hs -o NAME -odir build/ -hidir build/"
