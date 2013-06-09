package liquid

import (
	"testing"
  "liquid"
)

type Test struct {
	tmpl     string
	context  liquid.Context
	expected string
}

var tests = []Test{
	{"{% assign foo = values %}.{{ foo[0] }}.",
		liquid.Context {"values": {"foo", "bar", "qux"}},
		".foo."},
	{"{% assign foo = values %}.{{ foo[1] }}.",
		liquid.Context {"values": {"foo", "bar", "baz"}},
		".bar."},
	{"{% assign foo = values %}.{{ foo[2] }}.",
		liquid.Context {"values": {"foo", "bar", "baz"}},
		".qux."},
	{"{% assign foo = values %}.{{ foo[3] }}.",
		liquid.Context {"values": {"foo", "bar", "baz"}},
		".."},
}

func TestBasic(t *testing.T) {
	for _, test := range tests {
		err, output := liquid.Render(test.tmpl, test.context)
    if err != nil {
      t.Fatalf("%q caused error: %q", test.tmpl, err)
    }
		if output != test.expected {
			t.Fatalf("%q expected %q got %q", test.tmpl, test.expected, output)
		}
	}
}
