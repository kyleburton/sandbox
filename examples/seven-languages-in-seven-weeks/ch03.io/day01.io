// To run this: 
//   $ io ch01.io
// or from the io repl:
//   doFile("ch01.io")


// receiver on the left, message on the right
"this is a string\n" print

// both := and = are used for assignment
// := will create the slot if it doesn't exist, while
// = will throw an exception if the slot doesn't exist
// Eg: contrast the working line below with executing this one (in its stead, i.e. first)
//   Vehicle = Object clone
Vehicle := Object clone
Vehicle description := "Something to take you places"

// NB: once we've created the description slot, we can now assign to it with just =
Vehicle description = "Something to take you places"

Vehicle description println

// now for some reflection
Vehicle slotNames println

// that printed list(type, description) ... is that valid syntax?
// NO:  list(this, that)
// YES: list("this", "that")
//   ... and it prints w/o the quotes:
//      => list(this, that)


Object type println
Vehicle type println



Car := Vehicle clone
ferrari := Car clone
"ferrari type => " print
ferrari type println
"ferrari slotNames => " print
ferrari slotNames println


Ferrari := Car clone
"Ferrari type => " print
Ferrari type println
"Ferrari slotNames => " print
Ferrari slotNames println

method("So, you've come for an argument" println)
method() type println
// NB: how do you 'call' this? -- you can't, this doens't define a method, it constructs one, 
// you have to capture/save it
M1 := method("So, you've come for an argument" println)
// call it:
M1

// call it on another 'object'?
"foo" M1

// this works too?
ferrari M1
// yep(!), though I'm not sure what it means at this point

Car drive := method("Vroom" println)
ferrari drive
ferrari getSlot("drive") println
ferrari getSlot("type") println
ferrari proto println

"Checkout 'Lobby' (a namespace/object)" println


// p68 lists and maps
toDos := list("find my car", "find Continuum Transfunctioner")
toDos println
toDos size println
toDos append("Find a present")
toDos println
toDos size println

nums := list(1,2,3,34)
"average is " print
nums average println

"sum is " print
nums sum println

"at(1) " print
nums at(1) println

nums append(4)
nums println
nums pop
nums println

nums println
nums prepend(0)
nums println

"nums isEmpty " print
nums isEmpty println
"list() isEmpty " print
list() isEmpty println

"\"\" isEmpty " print
"" isEmpty println

elvis := Map clone
elvis println
elvis atPut("home", "Graceland") 
elvis println
elvis at("home") println
elvis atPut("style", "rock and roll")

"elvis asObject " print
elvis asObject println

"elvis asList " print
elvis asList println

"elvis keys " print
elvis keys println

"elvis size " print
elvis size println

"4 < 5 : " print
(4 < 5) println

"4 <= 3 : " print
(4 <= 3) println

"true and false : " print
(true and false) println

"true and true : " print
(true and true) println

"true or true : " print
(true or true) println

"true or false : " print
(true or false) println

"true and 6 : " print
(true and 6) println

"true and 0 : " print
(true and 0) println

true proto println


Highlander := Object clone
Highlander clone := Highlander
fred := Highlander clone
mike := Highlander clone
"fred=" print
fred println
"mike=" print
mike println
"fred == mike => " print
(fred == mike) println

// don't do this:
// Object clone := "banana"
// kinda funny, but it hoses the io runtime


//  a ::= 1  ==> newSlot("a", 1)
//  a := 1   ==> setSlot("a", 1)
//  a = 1    ==> updateSlot("a", 1)

"end of file"
