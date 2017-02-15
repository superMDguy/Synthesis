#!/bin/sh
FILES=""
MLEVEL="0"
LMODE="async"
usage="echo usage: dotty [-V] [-lm (sync|async)] [-el (0|1)] <filename>"

if [ x$DOTTYOPTIONS != x ]
then
    options=$DOTTYOPTIONS
else
    options="$@"
fi

set -- $options

for i in "$@"
do
	case $i in
	-V)
		echo "dotty version 95 (04-18-95)"
		;;
	-lm)
		shift
		LMODE=$1
		if [ x$LMODE != xsync && x$LMODE != xasync ]
		then
			$usage
			exit 1
		fi
		;;
	-el)
		shift
		MLEVEL=$1
		if [ x$MLEVEL != x0 && x$MLEVEL != x1 ]
		then
			$usage
			exit 1
		fi
		;;
	-*)
		$usage
		exit 1
		;;
	*)
		FILES="$FILES '"$1"'"
		shift
		;;
	esac
done

if [ x$LEFTYPATH = x ]
then
	LEFTYPATH="LEFTYLIBDIR"
	export LEFTYPATH
fi

if [ x$DOTTYPATH != x ]
then
    LEFTYPATH="$DOTTYPATH:$LEFTYPATH"
fi

CMDS="dotty.layoutmode = '$LMODE';"
CMDS="$CMDS dotty.mlevel = $MLEVEL; dot.mlevel =  $MLEVEL;"

if [ "x$FILES" = x ]
then
    FILES=null
fi
for i in $FILES
do
	CMDS="$CMDS dotty.createviewandgraph($i,'file',null);"
done

lefty -e "
load ('dotty.lefty');

checkpath = function () {
	if (tablesize(dotty) > 0);	# because tablesize(undef) returns "" not 0
	else {
		echo('You must set LEFTYPATH to the lefty lib directory path name.');
		exit();
	}
};
checkpath ();

dotty.init ();
monitorfile = dotty.monitorfile;
$CMDS
txtview ('off');
"
