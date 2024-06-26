package main

import (
	"fmt"
	"github.com/kyleburton/ctciutils"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"unicode"
)

func testSlurp() {
	data, err := ctciutils.Slurp("go.mod")
	if err != nil {
		panic(err)
	}

	fmt.Printf("file>>>>\n")
	fmt.Print(data)
	fmt.Printf("<<<<file\n")

	lines, err := ctciutils.SlurpToLines("go.mod")
	if err != nil {
		panic(err)
	}

	for idx, line := range lines {
		fmt.Printf("line[%03d]='%s'", idx, line)
	}
}

func merge(left, right []string) []string {
	res := make([]string, len(left)+len(right))
	for ii := 0; ii < len(left); ii += 1 {
		res[ii] = left[ii]
	}

	llen := len(left)

	for ii := 0; ii < len(right); ii += 1 {
		res[ii+llen] = right[ii]
	}
	return res
}

func stringHasUniqueChars(ss string) bool {
	seen := make(map[string]bool)
	for _, ch := range strings.Split(ss, "") {
		_, ok := seen[ch]
		if ok {
			return false
		}
		seen[ch] = true
	}
	return true
}

func stringHasUniqueCharsNoCase(ss string) bool {
	seen := make(map[string]bool)
	for _, ch := range strings.Split(strings.ToLower(ss), "") {
		_, ok := seen[ch]
		if ok {
			return false
		}
		seen[ch] = true
	}
	return true
}

func stringHasUniqueRunes(ss string) bool {
	seen := make(map[int32]bool)
	for _, ch := range ss {
		_, ok := seen[ch]
		if ok {
			// fmt.Printf("stringHasUniqueRunes: dupe=%c/%d", ch, ch)
			return false
		}
		seen[ch] = true
	}
	return true
}

func stringHasUniqueCharsNoDataStructures(ss string) bool {
	seen := make([]bool, 256)

	for _, ch := range ss {
		if seen[ch] {
			return false
		}
		seen[ch] = true
	}
	return true
}

// NB: golang doens't support this sort
//
//	panic: reflect: call of Swapper on string Value
//
//	func SortString(str string) string {
//		res := strings.Clone(str)
//		sort.Slice(res, func(i, j int) bool {
//			return res[i] < res[j]
//		})
//		return res
//	}
//

// ////////////////////////////////////////////////////////////////////////////
// sort for string
type sortRunes []rune

func (s sortRunes) Less(i, j int) bool {
	return s[i] < s[j]
}

func (s sortRunes) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

func (s sortRunes) Len() int {
	return len(s)
}

func SortString(s string) string {
	r := []rune(s)
	sort.Sort(sortRunes(r))
	return string(r)
}

////////////////////////////////////////////////////////////////////////////////

func checkPermutation(left, right string) bool {
	return SortString(left) == SortString(right)
}

func URLify(ss string) string {
	var sb strings.Builder
	for _, ch := range strings.Split(ss, "") {
		// fmt.Printf("Urlify: ch=%v is space? %v\n", ch, ch == " ")
		if ch == " " {
			sb.WriteString("%20")
		} else {
			sb.WriteString(ch)
		}
	}
	return sb.String()
}

func parseNumberFromStringStart(ss string) (int, string, error) {
	var num int
	slen := 0
	for _, ch := range ss {
		if !unicode.IsDigit(ch) {
			break
		}
		slen += 1
	}

	num, err := strconv.Atoi(ss[0:slen])
	if err != nil {
		return num, "", err
	}

	return num, ss[slen:len(ss)], nil
}

func StringDecompression(ss string) (string, error) {
	var sb strings.Builder
	var count int
	var err error
	for {
		if ss == "" {
			return sb.String(), nil
		}

		count, ss, err = parseNumberFromStringStart(ss)
		if err != nil {
			return "", err
		}

		for _ = range count {
			sb.WriteString(string(ss[0]))
		}
		ss = ss[1:len(ss)]
	}
}

func StringCompression(ss string) string {
	var sb strings.Builder
	if ss == "" {
		return ""
	}

	chars := strings.Split(ss, "")
	ch := chars[0]
	count := 1
	for idx := 1; idx < len(ss); idx += 1 {
		if ch != chars[idx] {
			sb.WriteString(fmt.Sprintf("%d%s", count, ch))
			count = 1
			ch = chars[idx]
			continue
		}
		count += 1
	}
	return sb.String()
}

func main() {
	fmt.Printf("here\n")
	for _, ch := range "thing" {
		fmt.Printf("ch=%+v; type=%+v\n", ch, reflect.TypeOf(ch))
	}
}
