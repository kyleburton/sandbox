package main

import (
	_ "fmt"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestMerge(t *testing.T) {
	left := []string{"this", "that", "other"}
	right := []string{"thing", "stuff", "hence"}
	var merged []string = nil

	merged = merge(nil, nil)
	assert.Equal(t, len(merged), 0, "Expected merge(nil,nil) to be empty")

	merged = merge(left, nil)
	assert.Equal(t, len(left), len(merged), "Expected merge(left,nil) to have the same length as left")
	assert.Equal(t, left, merged, "Expected merge(left,nil) to be the same as left")

	merged = merge(nil, right)
	assert.Equal(t, len(right), len(merged), "Expected merge(nil, right) to have the same length as right")
	assert.Equal(t, right, merged, "Expected merge(nil, right) to be the same as right")

	expected := []string{
		"this", "that", "other",
		"thing", "stuff", "hence",
	}
	merged = merge(left, right)
	assert.Equal(t, len(expected), len(merged), "Expected merge(left, right) to have the combined length of left+right")
	assert.Equal(t, expected, merged, "Expected merge(left, right) to contain all of left and then all of right")
}

func TestStringHasUniqueRunes(t *testing.T) {
	assert.Equal(t, true, stringHasUniqueRunes(""))
	assert.Equal(t, true, stringHasUniqueRunes("a"))
	assert.Equal(t, true, stringHasUniqueRunes("abcdef"))
	assert.Equal(t, false, stringHasUniqueRunes("aa"))
	assert.Equal(t, true, stringHasUniqueRunes("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
	assert.Equal(t, false, stringHasUniqueRunes("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZa"))
}

func TestStringHasUniqueChars(t *testing.T) {
	assert.Equal(t, true, stringHasUniqueChars(""))
	assert.Equal(t, true, stringHasUniqueChars("a"))
	assert.Equal(t, true, stringHasUniqueChars("abcdef"))
	assert.Equal(t, false, stringHasUniqueChars("aa"))
	assert.Equal(t, true, stringHasUniqueChars("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
	assert.Equal(t, false, stringHasUniqueChars("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZa"))
}

func TestStringHasUniqueCharsNoCase(t *testing.T) {
	assert.Equal(t, true, stringHasUniqueCharsNoCase(""))
	assert.Equal(t, true, stringHasUniqueCharsNoCase("a"))
	assert.Equal(t, true, stringHasUniqueCharsNoCase("abcdef"))
	assert.Equal(t, false, stringHasUniqueCharsNoCase("aa"))
	assert.Equal(t, false, stringHasUniqueCharsNoCase("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZa"))
}

// stringHasUniqueCharsNoDataStructures
func TestStringHasUniqueCharsNoDataStructures(t *testing.T) {
	assert.Equal(t, true, stringHasUniqueCharsNoDataStructures(""))
	assert.Equal(t, true, stringHasUniqueCharsNoDataStructures("a"))
	assert.Equal(t, true, stringHasUniqueCharsNoDataStructures("abcdef"))
	assert.Equal(t, false, stringHasUniqueCharsNoDataStructures("aa"))
	assert.Equal(t, true, stringHasUniqueCharsNoDataStructures("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
	assert.Equal(t, false, stringHasUniqueCharsNoDataStructures("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZa"))
}

func TestCheckPermutation(t *testing.T) {
	assert.Equal(t, true, checkPermutation("", ""))
	assert.Equal(t, true, checkPermutation("a", "a"))
	assert.Equal(t, true, checkPermutation("aaa", "aaa"))
	assert.Equal(t, true, checkPermutation("abc", "cba"))
	assert.Equal(t, false, checkPermutation("abcy", "cbax"))
}

func TestURLify(t *testing.T) {
	assert.Equal(t, "", URLify(""))
	assert.Equal(t, "abc", URLify("abc"))
	assert.Equal(t, "%20", URLify(" "))
	assert.Equal(t, "this%20that", URLify("this that"))
}

func TestParseNumberFromStringStart(t *testing.T) {
	count, str, err := parseNumberFromStringStart("33a")
	assert.Equal(t, 33, count)
	assert.Equal(t, "a", str)
	assert.Nil(t, err)
}

func TestStringDecompression(t *testing.T) {
	res, err := StringDecompression(StringCompression(""))
	assert.Equal(t, "", res)
	assert.Nil(t, err)

	res, err = StringDecompression("2a")
	assert.Equal(t, "aa", res)
	assert.Nil(t, err)

	res, err = StringDecompression("3a1b4d8c")
	assert.Equal(t, "aaabddddcccccccc", res)
	assert.Nil(t, err)
}

func TestStringCompression(t *testing.T) {
	// assert.Equal(t, "", StringCompression(""))
	// assert.Equal(t, "1a", StringCompression("a"))
	// assert.Equal(t, "2a", StringCompression("aa"))
	// assert.Equal(t, "3a1b4d8c", StringCompression("aaabddddcccccccc"))

	// res, err := StringDecompression(StringCompression(""))
	// assert.Equal(t, "", res)
	// assert.Nil(t, err)

	// res, err = StringDecompression(StringCompression("aa"))
	// assert.Equal(t, "aa", res)
	// assert.Nil(t, err)

	// tstr := "aaabddddcccccccc"
	// res, err = StringDecompression(StringCompression(tstr))
	// assert.Equal(t, tstr, res)
	// assert.Nil(t, err)
}

func TestZeroMatrix(t *testing.T) {
	m1 := [][]int{}
	ZeroMatrix(m1)
	assert.Equal(t, m1, m1)

	m2 := [][]int{
		[]int{0, 1, 2},
		[]int{3, 4, 5},
	}

	ZeroMatrix(m2)
	assert.Equal(t, m2, [][]int{
		[]int{0, 0, 0},
		[]int{0, 4, 5},
	})
}

func TestEdist(t *testing.T) {
	assert.Equal(t, 0, Edist("", ""))
	assert.Equal(t, 0, Edist("a", "a"))
	assert.Equal(t, 1, Edist("car", "bar"))
	assert.Equal(t, 2, Edist("car", "acr"))
	assert.Equal(t, 2, Edist("c", "car"))
	assert.Equal(t, 3, Edist("car", "bug"))
}

func TestOneAway(t *testing.T) {
	assert.Equal(t, false, OneAway("", ""))
	assert.Equal(t, false, OneAway("other thing", "more stuff"))
	assert.Equal(t, false, OneAway("this", "this"))
	assert.Equal(t, true, OneAway("this", "thin"))
}
