package main

import (
	"fmt"
	"strings"
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

type TypedNode[T any] struct {
	Data T
	Next *TypedNode[T]
}

func NewTypedNode[T any](val T) *TypedNode[T] {
	return &TypedNode[T]{
		Data: val,
		Next: nil,
	}
}

func (s *TypedNode[T]) Push(val T) *TypedNode[T] {
	return &TypedNode[T]{
		Data: val,
		Next: s,
	}
}

func (s *TypedNode[T]) Pop() (*TypedNode[T], T) {
	return s.Next, s.Data
}

func (s *TypedNode[T]) Tail() *TypedNode[T] {
	for n := s; n != nil; n = n.Next {
		if n.Next == nil {
			return n
		}
	}
	return s
}

func (s *TypedNode[T]) Append(val T) *TypedNode[T] {
	n := s.Tail()
	n.Next = &TypedNode[T]{Data: val}
	return s
}

func (s *TypedNode[T]) Array() []T {
	res := make([]T, 0)

	for n := s; n != nil; n = n.Next {
		res = append(res, n.Data)
	}

	return res
}

func (s *TypedNode[T]) String() string {
	sb := strings.Builder{}
	for n := s; n != nil; n = n.Next {
		sb.WriteString(fmt.Sprintf("%v", n.Data))
		if n.Next != nil {
			sb.WriteString(",")
		}
	}

	return sb.String()
}

func (s *TypedNode[T]) Reverse() *TypedNode[T] {
	var res *TypedNode[T]
	for n := s; n != nil; n = n.Next {
		res = res.Push(n.Data)
	}
	return res
}

func main() {
	n := NewTypedNode(int64(10))
	n = n.Push(9)
	n = n.Push(8)
	n = n.Push(7)
	fmt.Printf("n=%v\n", n.String())

	n, _ = n.Pop()
	fmt.Printf("n=%v\n", n.String())

	n.Append(99)
	fmt.Printf("n=%v\n", n.String())

	m := n.Reverse()
	fmt.Printf("n=%v\n", m.String())
}
