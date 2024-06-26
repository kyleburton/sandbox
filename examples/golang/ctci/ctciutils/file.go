package ctciutils

import (
	"io/ioutil"
	"strings"
)

func Slurp(fname string) (string, error) {
	data, err := ioutil.ReadFile(fname)
	if err != nil {
		return "", err
	}

	return string(data), nil
}

func SlurpToLinesChomped(fname string) ([]string, error) {
	data, err := Slurp(fname)
	if err != nil {
		return nil, err
	}

	return strings.Split(data, "\n"), nil
}

func SlurpToLines(fname string) ([]string, error) {
	data, err := Slurp(fname)
	if err != nil {
		return nil, err
	}

	return strings.SplitAfter(data, "\n"), nil
}
