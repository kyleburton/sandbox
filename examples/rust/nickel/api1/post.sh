set -exu
curl -i -XPOST -d '{"firstname": "bobby", "lastname": "tables"}' http://localhost:3000/people

