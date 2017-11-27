#!/bin/bash -eux

vegeta attack -insecure -http2 -targets=targets.txt -duration=60s -rate=100 -timeout 1s > results.bin
vegeta report -inputs=results.bin -reporter=json > metrics.json
vegeta report -reporter=plot < results.bin > plot.html
vegeta report -reporter="hist[0,1ms,2ms,3ms,4ms,5ms,6ms,7ms,8ms,9ms,10ms]" < results.bin
vegeta report -reporter=text < results.bin
