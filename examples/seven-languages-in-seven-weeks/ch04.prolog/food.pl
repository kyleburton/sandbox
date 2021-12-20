food_type(velveeta, cheese).
food_type(cheddar, cheese).
food_type(stilton, cheese).
food_type(gouda, cheese).
food_type(munster, cheese).
food_type(provolone, cheese).
food_type(swiss, cheese).
food_type(eadam, cheese).
food_type(ritz, cracker).
food_type(spam, meat).
food_type(sausage, meat).
food_type(bacon, meat).
food_type(steak, meat).
food_type(shrimp, seafood).
food_type(jolt, soda).
food_type(sprite, soda).
food_type(coke, soda).
food_type(pepsi, soda).
food_type(orange_fanta, soda).
food_type(twinkie, dessert).
food_type(kandykake, dessert).
food_type(moonpie, dessert).

flavor(sweet, dessert).
flavor(savory, meat).
flavor(savory, cheese).
flavor(savory, seafood).
flavor(sweet, soda).

food_flavor(X, Y) :- food_type(X, Z), flavor(Y, Z).

/*

type in:

food_type(What, meat).

then type semi-colon to get additional answers,
if you press enter, that becomes the last answer and is accepted as the answer
(hence the response of 'yes', opposed to running out of answers by typing ';'
and you get 'no')

You can get them all at once by typing 'a' instead of ';' or enter.

 */
