#!/usr/bin/env bash

#  USAGE: ./run_test.sh <APP> <LOG>.
#    <LOG>: if present then it will  output of the tests in LOG.

function report ()
{
  # $1= app, $2=status, $3=output
  msg="FAIL"
  if [ $2 -eq 0 ]; then
    msg="PASS"
  fi
  if [ $2 -eq 124 ]; then
    msg="FAIL: TEST HAS TIMEOUT"
  fi
  if [ -n "$3" ]; then
    echo "$1: $msg. exit code: $2"
    RED='\033[0;31m'
    NC='\033[0m' # No Color
    echo -e "${RED}$1:\n${3}${NC}" 1>&2
  else
    echo "$1: $msg. exit code: $2"
  fi
}

if [ "$#" -lt "1" ]; then
  exit -1
elif [ ! -f "$1" ]; then
  RED='\033[0;31m'
  NC='\033[0m' # No Color
  echo -e "${RED}$1: Test not found${3}${NC}" 1>&2
  exit -1
fi

app=$1
output=`timeout 60s $app 2>&1`
status=$?

if [ -z $2 ]; then
  report $(basename $app) $status
else
  report $(basename $app) $status "$output"
fi

unset report
exit $status
