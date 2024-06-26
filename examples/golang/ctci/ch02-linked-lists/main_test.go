package main

import (
	_ "fmt"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestNodeLength(t *testing.T) {
	n1 := Node{Data: "a"}
	assert.Equal(t, 1, n1.Length())
	n2 := Node{Data: "a"}
	n2.Append("b")
	n2.Append("c")
	assert.Equal(t, 3, n2.Length())
}
