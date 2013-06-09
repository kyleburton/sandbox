package liquid

import (
  "fmt"
  "strings"
  "unicode"
  "unicode/utf8"
)

// see: http://cuddle.googlecode.com/hg/talk/lex.html#slide-16

type Context map[string][]string

type itemType int
type stateFn func(*lexer) stateFn

const (
  itemError itemType = iota
  itemEOF
  itemText
  itemOpenExpression
  itemCloseExpression
  itemContent
  itemEquals
  itemAssign
  itemVar
  itemValue
)

const eof = -1

type item struct {
  typ itemType
  val string
}

func (i item) String () string {
  switch i.typ {
  case itemEOF:
    return "EOF"
  case itemEquals:
    return "="
  case itemError:
    return i.val
  }

  if len(i.val) > 10 {
    return fmt.Sprintf("%.10q...", i.val)
  }

  return fmt.Sprintf("%q", i.val)
}

type lexer struct {
  name string
  input string
  start int
  pos int
  width int
  items chan item
}

func lex(name, input string) (*lexer, chan item) {
  l := &lexer{
    name: name,
    input: input,
    items: make(chan item),
  }

  go l.run()
  return l, l.items
}

func (l *lexer) run() {
  for state := lexText; state != nil; {
    state = state(l)
  }
  close(l.items)
}

func (l *lexer) emit(t itemType) {
  l.items <- item{t, l.input[l.start:l.pos]}
  l.start = l.pos
}

const openExpression = "{%"
const closeExpression = "%}"

func lexText(l *lexer) stateFn {
  for {
    if strings.HasPrefix(l.input[l.pos:], openExpression) {
      if l.pos > l.start {
        l.emit(itemText)
      }
      return lexOpenExpression
    }

    if l.next() == eof { break }
  }

  // correctly reached EOF
  if l.pos > l.start {
    l.emit(itemText)
  }

  l.emit(itemEOF)
  return nil
}

func lexOpenExpression(l *lexer) stateFn {
  l.pos += len(openExpression)
  l.emit(itemOpenExpression)
  return lexInsideExpression
}

func lexCloseExpression(l *lexer) stateFn {
  l.pos += len(closeExpression)
  l.emit(itemCloseExpression)
  return lexText
}

func (l *lexer) next() (r rune) {
  if l.pos >= len(l.input) {
    l.width = 0
    return eof
  }
  r, l.width = utf8.DecodeRuneInString(l.input[l.pos:])
  l.pos += l.width
  return r
}

func lexInsideExpression(l *lexer) stateFn {
  // Any of: 'assign'
  for {
    if strings.HasPrefix(l.input[l.pos:], closeExpression) {
      return lexCloseExpression
    }

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

func lexExpression(l *lexer) stateFn {
  panic("Not implemented");
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
  l.items <- item {
    itemError,
    fmt.Sprintf(format, args...),
  }
  return nil
}

func Parse (tmpl string) {
  l, items := lex("liquid", tmpl)
  fmt.Printf("l=%q items=%q\n", l, items)
}


func Render (tmpl string, ctx Context) (error, string) {
  Parse(tmpl)
  return nil, "nothing"
}
