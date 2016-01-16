ii := 1
while(ii <= 11, ii println; ii = ii + 1); "this one goes up to 11" println

for(ii, 1, 11,  ii println); "this one goes up to 11 too" println

for(ii, 1, 11, 2,  ii println); "inc'd by 2" println

// "If that line of code is buried deeply into a complex package, Io just puked in your car"
// "Sometimes, freedom hurts."
//  ...indeed the price of freedom is eternal vigilance 
//     (paraphrased from john philpot curran's "the condition upon which god
//     hath given liberty to man is eternal vigilance.")



if(true, "It is true." println, "It is false." println)

if(true) then("it is true" println) else ("it is false" println)
// whoah! this means if returns something...
thatIfThing := if(true)
thatIfThing then("ah, yep" println) else("oh noes" println)
// indeed, it's the boolean thing
true then("ah, yep" println) else("oh noes" println)
false then("ah, yep" println) else("oh noes" println)

// :( I wish if's then() and else() were expressions, but they return 'nil)

// This is strange behavior...
thatIfThing := if(true)
"thatIfThing := if(true)" println

"thatIfThing then(33) => " print
thatIfThing then(33) println

"thatIfThing else(33) => " print
thatIfThing else(33) println

thatIfThing := if(false)
"thatIfThing := if(false)" println

"thatIfThing then(33) => " print
thatIfThing then(33) println

"thatIfThing else(33) => " print
thatIfThing else(33) println

// this is distinct from the if 'expression' itself:
"straight up if statements are expressions though" println
if(true, 1, 2) println


OperatorTable println
OperatorTable addOperator("xor", 11)
true  xor := method(bool, if(bool, false, true))
false xor := method(bool, if(bool, true, false))

"true  xor true  => " print
true xor true println

"true  xor false => " print
true xor false println

"false xor true  => " print
false xor true println

"false xor false => " print
false xor false println



postOffice := Object clone
postOffice packageSender := method(call sender)

mailer := Object clone
mailer deliver := method(postOffice packageSender)

mailer deliver
postOffice messageTarget := method(call target)
postOffice messageTarget


postOffice messageArgs := method(call message arguments)
postOffice messageName := method(call message name)


unless := method(
  (call sender doMessage(call message atArg(0))) ifFalse(
   call sender doMessage(call message atArg(1))) ifTrue(
   call sender doMessage(call message atArg(2)))
)
