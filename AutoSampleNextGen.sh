#!/bin/bash
# Sample script to just write to stdout every second

NUMPASS=${1:-120}
i="0"
while [ $i -lt $NUMPASS ]; do
    echo "pass $i"
    sleep 1
    i=$[$i+1]
done

exit 0
