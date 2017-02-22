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
        then
        exit 1
    fi
fi

if [ "$1" = "gui" ]
    then
    cp IOGUI.hs IODiscrete.hs
elif [ "$1" = "cmd" ]
    then
    cp IOCmd.hs IODiscrete.hs
else
    then
    eval "$0 $HELP_FLAG"
    exit 2
fi
