-module(temp).
-export([f2c/1,c2f/1]).

f2c({farenheight,Temp}) -> {celcius, (Temp - 32) * (5/9)}.

c2f({celcius,Temp}) -> {farenheight, (Temp * (9/5)) + 32}.
