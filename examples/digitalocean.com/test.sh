#!/usr/bin/env bash
set -eux
VERBOSE=""

# go run pkg/diocean/main.go regions ls
# id      name    slug
# 3       San Francisco 1 sfo1
# 4       New York 2      nyc2
# 5       Amsterdam 2     ams2
# 6       Singapore 1     sgp1

#REGION=nyc2
REGION=sfo1

go run pkg/diocean/main.go $VERBOSE droplets new test1 512mb ubuntu-13-10-x64 $REGION 20848 false false 2>&1 | tee new.output

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

# root:$6$hTe5XKGO$nN8C92LtSGNer7syZQP1yLF0RPO5pXXkM9XyLqLnpbn0qyvtcAMceOX4leU2dK8PsAomEANngAOGVLE2xFAQo1:16165:0:99999:7:::

