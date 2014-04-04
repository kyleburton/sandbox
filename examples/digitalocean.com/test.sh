#!/usr/bin/env bash
set -eux
VERBOSE=""
go run pkg/diocean/main.go $VERBOSE droplets new test1 512mb ubuntu-13-10-x64 nyc2 20848 false false 2>&1 | tee new.output

# id      name    image_id        size_id event_id
# 1413427 test1   1505699 66      20770146
EVENT_ID=$(tail -n 1 new.output | cut -f 5)

# grab the event and the droplet_id
go run pkg/diocean/main.go $VERBOSE droplets ls


# wait for the droplet to spin up
go run pkg/diocean/main.go $VERBOSE events wait $EVENT_ID

# destroy the droplet
go run pkg/diocean/main.go $VERBOSE droplets destroy 1390159 false 2>&1 | tee destroy.output

# event_id
# 20770212
EVENT_ID=$(tail -n 1 destroy.output)

# wait for the destroy to complete
go run pkg/diocean/main.go $VERBOSE events wait $EVENT_ID

rm new.output
rm destroy.output
