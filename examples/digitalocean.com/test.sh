go run pkg/diocean/main.go droplets new test1 512mb ubuntu-13-10-x64 nyc2 20848 false false
# grab the event and the droplet_id
go run pkg/diocean/main.go droplets ls
# wait for the droplet to spin up
echo go run pkg/diocean/main.go events wait 12345
# destroy teh droplet
echo go run pkg/diocean/main.go droplets destroy 1390159 false
