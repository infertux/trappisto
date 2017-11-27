#!/bin/bash -eu

test $UID -eq 0

LOG="/home/infertux/.dcrd/logs/mainnet/dcrd.log"
CACHE="$(dirname $0)/nginx/cache/rpc"

find "$CACHE" -mindepth 1 -delete -print

tail -F -n 0 "$LOG" | \
while read -r line; do
    if echo "$line" | grep -E 'BMGR: Processed [0-9]+ blocks? in the last'; then
        find "$CACHE" -mindepth 1 -delete -print
    fi
done
