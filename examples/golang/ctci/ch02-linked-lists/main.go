package main

import (
	"fmt"
)

type Node struct {
	Data interface{}
	Next *Node
}

func (self *Node) Append(data interface{}) *Node {
	tail := self
	for {
		if tail.Next == nil {
			break
		}
		tail = tail.Next
	}
	tail.Next = &Node{Data: data, Next: nil}

	return self
}

func (self *Node) Length() int {
	tail := self
	length := 1
	for {
		if tail.Next == nil {
			break
		}
		length += 1
		tail = tail.Next
	}

	return length
}

func main() {
	fmt.Printf("here\n")
}
