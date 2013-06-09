export GOPATH=$(pwd)
go test -i src/liquid/liquid_test.go &&
go test src/liquid/liquid_test.go
