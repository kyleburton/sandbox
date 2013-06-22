package liquid

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// see: http://cuddle.googlecode.com/hg/talk/lex.html#slide-16

// TODO: change Context to be a map of 
//   string -> interface and use reflection?
type Context map[string][]string

type itemType int
type stateFn func(*lexer) stateFn

/*
type token struct {
  fn        stateFn
  tokenType int
  body      string
}
*/

const debug = true

const (
	itemError itemType = iota
	itemEOF
	itemText
	itemOpenMarkup  // {{
	itemCloseMarkup // }}
	itemOpenTag     // {%
	itemCloseTag    // %}
	itemVariable
	itemEquals
	itemNotEquals
	itemAssign
	itemExpression
	itemIf
)

const eof = -1

// TODO: include the line number and the column number
// in order to produce better error messages
type item struct {
	typ itemType
	val string
}

func (i item) String() string {
	switch i.typ {
	case itemEOF:
		return "EOF"
	case itemEquals:
		return "="
	case itemNotEquals:
		return "!="
	case itemAssign:
		return "=="
	case itemError:
		return i.val
	}

	if len(i.val) > 10 {
		return fmt.Sprintf("%.10q...", i.val)
	}

	return fmt.Sprintf("item{typ=%q, val=%q}", i.typ, i.val)
}

// TODO: include the line number and the column number
// in order to produce better error messages
type lexer struct {
	name  string
	input string
	start int
	pos   int
	width int
	items chan item
}

func lex(name, input string) (*lexer, chan item) {
	l := &lexer{
		name:  name,
		input: input,
		items: make(chan item),
	}

	go l.run()
	return l, l.items
}

func (l *lexer) run() {
	fmt.Printf("lexer.run: starting run, l=%q\n", l)
	for state := lexText; state != nil; {
		state = state(l)
		fmt.Printf("lexer.run: parsed next state: %q\n", state)
	}
	fmt.Printf("lexer.run: exhausted stream, closing l.items channel\n")
	close(l.items)
}

func (l *lexer) emit(t itemType) {
	fmt.Printf("l.emit: itemType=%q\n", t)
	ii := item{t, l.input[l.start:l.pos]}
	l.items <- ii
	fmt.Printf("l.emit setting l.start(%d) = l.pos(%d)\n", l.start, l.pos)
	l.start = l.pos
}

const openOutputMarkup = "{{"
const endOutputMarkup = "}}"

const openTagMarkup = "{%"
const endTagMarkup = "%}"

func lexText(l *lexer) stateFn {
	for {
		fmt.Printf("lexText: checking for (output) %s has?=%q\n", openOutputMarkup, strings.HasPrefix(l.input[l.pos:], openOutputMarkup))
		if strings.HasPrefix(l.input[l.pos:], openOutputMarkup) {
			fmt.Printf("lexText: l.pos=%d > l.start=%d\n", l.pos, l.start)
			if l.pos > l.start {
				fmt.Printf("lexText: emitting itemText=%q text=%s\n", itemText, string(l.input[l.start:l.pos]))
				l.emit(itemText)
				fmt.Printf("lexText: emitted itemText=%q\n", itemText)
			}
			fmt.Printf("lexText: found %s, l.emit(%s) => lexOutputMarkup\n", openOutputMarkup, itemText)
			return lexOutputMarkup
		}

		fmt.Printf("lexText: checking for (tag) %s\n", openTagMarkup)
		if strings.HasPrefix(l.input[l.pos:], openTagMarkup) {
			if l.pos > l.start {
				l.emit(itemText)
			}
			fmt.Printf("lexText: found %s, l.emit(%s) => lexTagMarkup\n", openTagMarkup, itemText)
			return lexTagMarkup
		}

		fmt.Printf("lexText: calling l.next()\n")
		if l.next() == eof {
			break
		}
		fmt.Printf("lexText: was not eof, looping around\n")
	}

	// correctly reached EOF
	if l.pos > l.start {
		l.emit(itemText)
	}

	l.emit(itemEOF)
	return nil
}

func lexOutputMarkup(l *lexer) stateFn {
	l.pos += len(openOutputMarkup)
	l.emit(itemOpenMarkup)
	return lexInsideMarkupExpression
}

func lexTagMarkup(l *lexer) stateFn {
	l.pos += len(openTagMarkup)
	l.emit(itemOpenTag)
	return lexInsideTagExpression
}

func lexCloseMarkupExpression(l *lexer) stateFn {
	l.pos += len(endOutputMarkup)
	l.emit(itemCloseMarkup)
	return lexText
}

func lexCloseTagExpression(l *lexer) stateFn {
	l.pos += len(endTagMarkup)
	l.emit(itemCloseTag)
	return lexText
}

func (l *lexer) next() (r rune) {
	fmt.Printf("l.next: r=%q, pos=%d len(l.input)=%d l=%q\n", r, l.pos, len(l.input), l)
	if l.pos >= len(l.input) {
		l.width = 0
		return eof
	}
	r, l.width = utf8.DecodeRuneInString(l.input[l.pos:])
	l.pos += l.width
	fmt.Printf("l.next: utf8.DecodeRuneInString(\"%s\") r=%q l.width=%d\n", l.input[l.pos:],
		r, l.width)
	return r
}

func lexInsideMarkupExpression(l *lexer) stateFn {
	for {
		if strings.HasPrefix(l.input[l.pos:], endOutputMarkup) {
			return lexCloseMarkupExpression
		}

		// switch this to consume the entire statement
		switch r := l.next(); {
		case r == eof || r == '\n':
			return l.errorf("unclosed expression")
		case unicode.IsSpace(r):
			l.ignore()
		case unicode.IsLetter(r):
			return lexExpression
		default:
			return l.errorf("fell through switch, unrecognized expression")
		}
	}
}

func lexInsideTagExpression(l *lexer) stateFn {
	for {
		if strings.HasPrefix(l.input[l.pos:], endTagMarkup) {
			return lexCloseTagExpression
		}

		r := l.next()
		if r == eof || r == '\n' {
			return l.errorf("unclosed expression")
		}
	}
}

func lexExpression(l *lexer) stateFn {
	for {
		if strings.HasPrefix(l.input[l.pos:], endOutputMarkup) {
			l.emit(itemCloseMarkup)
			return lexCloseMarkupExpression
		}

		/*
		   if strings.HasPrefix(l.input[l.pos:], "if") {
		     l.emit(itemIf)
		     return lexBooleanExpression
		   }
		*/

		r := l.next()
		if r == eof || r == '\n' {
			return l.errorf("unclosed expression")
		}
	}
}

func (l *lexer) ignore() {
	l.start = l.pos
}

func (l *lexer) backup() {
	l.start -= l.width
}

func (l *lexer) peek() rune {
	rune := l.next()
	l.backup()
	return rune
}

func (l *lexer) accept(valid string) bool {
	if strings.IndexRune(valid, l.next()) >= 0 {
		return true
	}
	l.backup()
	return false
}

func (l *lexer) acceptRun(valid string) {
	for strings.IndexRune(valid, l.next()) >= 0 {
	}
	l.backup()
}

func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.items <- item{
		itemError,
		fmt.Sprintf(format, args...),
	}
	return nil
}

func Parse(tmpl string) []item {
	fmt.Printf("Parse: tmpl=%s\n", tmpl)
	l, items := lex("liquid", tmpl)
	fmt.Printf("Parse: lexer=%q items=%q\n", l, items)
	tokens := make([]item, 0)
	for item := range items {
		fmt.Printf("Parse: item=%q\n", item)
		tokens = append(tokens, item)
	}
	return tokens
}

func (self *Context) get(k string) string {
	item, ok := (*self)[k]
	if ok {
		return item[0]
	}
	return ""
}

func (self *Context) has(k string) bool {
	_, ok := (*self)[k]
	return ok
}

func Render(tmpl string, ctx Context) (string, error) {
	tokens := Parse(tmpl)
	res := ""
	tlen := len(tokens)
	ii := 0
	for {
		if ii >= tlen {
			break
		}

		itm := tokens[ii]
		fmt.Printf("Render: [%d] itm=%q\n", ii, itm)
		switch itm.typ {
		case itemText:
			res = fmt.Sprintf("%s%s", res, itm.val)
		// if it's a var access
		case itemOpenMarkup:
			ii += 1 // skp the {{
			itm := tokens[ii]
			if ctx.has(itm.val) {
				res = fmt.Sprintf("%s%s", res, ctx.get(itm.val))
				ii += 1 // skp the item
			}
		case itemEOF:
			break
		default:
			panic(fmt.Sprintf("don't know how render item type: %q\n", itm))
		}
		ii += 1
	}

	return res, nil
}

/*

  openExpression closeExpression
  openExpression variable closeExpression
  openExpression variable (pipe filter)+ closeExpression

*/
