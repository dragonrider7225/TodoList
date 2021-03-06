#!/bin/bash

HELP_FLAG="-h"
USAGE="Usage: $0 [$HELP_FLAG] {gui|cmd}"

if [ $# -ne 1 ] || [ "$1" == "$HELP_FLAG" ]
then
    echo "$USAGE"
    if [ $# -eq 1 ]
        then
        exit 0
    else
        exit 1
    fi
fi

if [ "$1" = "gui" ]
    then
    cp src/IOGUI.hs src/IODiscrete.hs
elif [ "$1" = "cmd" ]
    then
    cp src/IOCmd.hs src/IODiscrete.hs
else
    eval "$0 $HELP_FLAG"
    exit 2
fi
