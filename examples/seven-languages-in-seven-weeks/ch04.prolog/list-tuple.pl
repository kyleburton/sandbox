[a, b, c] = [a|Tail].
%% Tail = [b,c] 

[a, b, c] = [a|[Head|Tail]].
%% => 
%%  Head = a
%%  Tail = [c]


%% Grab the 3rd element
[a, b, c, d, e, f] = [_, _|[Head|_]].
