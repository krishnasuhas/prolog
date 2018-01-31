/*******************************************/
/**    Your solution goes in this file    **/ 
/*******************************************/

/*1st question*/
fc_course(X):-course(X,_,3);course(X,_,4).

/*2nd question*/
prereq_110(Y):-course(Y,X,_),member(ecs110,X).

/*3rd question*/
ecs140a_students(X):-student(X,Y),member(ecs140a,Y).

/*4th question*/
instructor_names(A):-student(john,Y),
instructor(A,Z),
findall(X, ( nth0(N, Y, X), member(X, Z) ) ,Final),
length(Final,L),L>0.

/*5th question*/
students(A):-instructor(jim,Y),
student(A,Z),
findall(X, ( nth0(N, Y, X), member(X, Z) ) ,Final),
length(Final,L),L>0.

/*6th question*/

allprereq(A,Temp):-course(A,B,_),ap(B,Temp),reverse(Temp,Temp).
ap([],[]).
ap([H|T],[H|Temp]):-allprereq(H,Temp),ap(T,X).


/*part2*/

/*1st question*/

all_length([H|T],X):-is_list(H),
length(H,0),all_length(T,Q),
X is 1+Q,!.
all_length([],0).
all_length([H|T],X):-is_list(H),
all_length(H,P),all_length(T,Q),
X is P+Q.
all_length([H|T],X):-atom(H),
all_length(T,Y),X is 1 +Y.


/*2nd question*/
equal_a_b(A):-equal_a_b2(A,0).
equal_a_b2([],0).
equal_a_b2([H|T],Temp):-H \= a,H \= b,equal_a_b2(T,Temp).
equal_a_b2([a|T],Temp):-equal_a_b2(T,Y),Temp is 1+ Y.
equal_a_b2([b|T],Temp):-equal_a_b2(T,Y),Temp is -1+ Y.

/*3rd question*/
swap_prefix_suffix(K,L,R):-
append(L1,L23,L),
append(L2,L3,L23),
L2==K,
append(L3,L2,SR),
append(SR,L1,R).

/*4th question*/

palin(A):-revinsert(A,A,[]).
revinsert([],A,A).
revinsert([H|T],A,B):-revinsert(T,A,[H|B]).


/*5th question*/

good([]).
good([0]).
good([1|T]):-length(T,L),L > 1,good(T).
good([0|T]):-T \= [],good(T).

/*part3*/

state(A,_,_,_):-A= left;A=right.
state(_,A,_,_):-A= left;A=right.
state(_,_,A,_):-A= left;A=right.
state(_,_,_,A):-A= left;A=right.

farmer(F):-state(F,_,_,_).
wolf(W):-state(_,W,_,_).
cabbage(C):-state(_,_,C,_).
goat(G):-state(_,_,_,G).

safe(state(F,W,C,G)):-W \=G,G \=C.
safe(state(F,W,C,G)):-F == W,F == G,W == G.
safe(state(F,W,C,G)):-F == C,F == G,C == G,!.

unsafe(S):- (\+ safe(S)).
opposite(A,B):-A \= B.

take(X,A,B):-format('take(~w ',[X,","]),write(','),write(A),
write(' ,'),write(B),write(')'),nl.

arc(N,state(left,W,C,G),state(right,W,C,G)):-safe(state(right,W,C,G)).
arc(N,state(right,W,C,G),state(left,W,C,G)):-safe(state(left,W,C,G)).
arc(N,state(left,left,C,G),state(right,right,C,G)):-safe(state(right,right,C,G)).
arc(N,state(right,right,C,G),state(left,left,C,G)):-safe(state(left,left,C,G)).
arc(N,state(left,W,left,G),state(right,W,right,G)):-safe(state(right,W,right,G)).
arc(N,state(right,W,right,G),state(left,W,left,G)):-safe(state(left,W,left,G)).
arc(N,state(left,W,C,left),state(right,W,C,right)):-safe(state(right,W,C,right)).
arc(N,state(right,W,C,right),state(left,W,C,left)):-safe(state(left,W,C,left)).

out(state(A,W,C,G),state(B,W,C,G)):-take('none',A,B).
out(state(A,A,C,G),state(B,B,C,G)):-take('wolf',A,B).
out(state(A,W,A,G),state(B,W,B,G)):-take('cabbage',A,B).
out(state(A,W,C,A),state(B,W,C,B)):-take('goat',A,B).


/* solved recursively but not working
solve:-sol(A,_),arc(_,A,state(right,right,right,right)),out(A,state(right,right,right,right)).
sol(A,_):-arc(_,B,A),sol(B,_),out(B,A).
sol(arc(_,state(left,left,left,left),A),_):-out(state(left,left,left,left),A).
*/

ss(A,B,C,D,E,F):-arc(_,A,state(right,right,right,right)),
arc(_,B,A),arc(_,C,B),arc(_,D,C),arc(_,E,D),arc(_,F,E),
arc(_,state(left,left,left,left),F).

solve:-ss(A,B,C,D,E,F),out(state(left,left,left,left),F),
out(F,E),out(E,D),out(D,C),out(C,B),out(B,A),
out(A,state(right,right,right,right)),!.


finalsol:-arc(_,state(left,left,left,left),B),checkloop(B),out(state(left,left,left,left),B).
checkloop(state(left,left,left,left)).
checkloop(B):-arc(_,B,C),out(B,C),checkloop(C).














