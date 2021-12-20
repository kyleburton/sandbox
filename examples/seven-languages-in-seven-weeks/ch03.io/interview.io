# TODO: add birthDate and change age to be a function of the curent time
Person  := Object clone
Address := Object clone
people  := list() // List clone

addPerson := method( name, age, address, 
  p          := Person clone 
  p name     := name
  p age      := age
  p address  := address
  people append(p)
)

makeAddress := method(line1, city, state, zip,
  a         := Address clone
  a line1   := line1
  a city    := city
  a state   := state
  a zip     := zip
)
people append("Kyle",    44, makeAddress("12333 W. Olympic Blvd", "Santa Monica", "CA", "90064"))
people append("Kristin", 43, makeAddress("345 S. Devon Ave",      "Wayne", "PA", "19087"))
people append("Sydney",  12, makeAddress("345 S. Devon Ave",      "Wayne", "PA", "19087"))
people append("Madison",  8, makeAddress("345 S. Devon Ave",      "Wayne", "PA", "19087"))

people println
people at(0) println
// adults := people select(age > 20) 
adults := people select(person, person age > 20) 
adults map(address) println
