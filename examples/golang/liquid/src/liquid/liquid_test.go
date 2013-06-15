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

/*
see: https://github.com/Shopify/liquid/wiki/Liquid-for-Designers

  assign - Assigns some value to a variable
  capture - Block tag that captures text into a variable
  case - Block tag, its the standard case...when block
  comment - Block tag, comments out the text in the block
  cycle - Cycle is usually used within a loop to alternate between values, like colors or DOM classes.
  for - For loop
  if - Standard if/else block
  include - Includes another template; useful for partials
  raw - temporarily disable tag processing to avoid syntax conflicts.
  unless - Mirror of if statement

{% for product in product %}
  {{name}}
  {{product.name}}
  {{product.price | price}}
  {{ product.description | prettyprint | paragraph }}
{% endfor %}


Hello {{name}}
Hello {{user.name}}
Hello {{ 'tobi' }}

Hello {{ 'tobi' | upcase }}
Hello tobi has {{ 'tobi' | size }} letters!
Hello {{ '*tobi*' | textilize | upcase }}
Hello {{ 'now' | date: "%Y %h" }}



*/
var tests = []Test{
	{".{{name}}.",
		liquid.Context {"values": {"name", "a name", ""}},
		".a name."},
/*
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
*/
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
