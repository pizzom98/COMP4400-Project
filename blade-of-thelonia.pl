% Blade of Thelonia 
% Created by: Matthew Pizzo
% Date: 11-21-2020
% A simple adventure game written in prolog

%Initalize Dynamic Relations
:- dynamic(here/1).
:- dynamic(have/1).
:- dynamic(turned_off/1).
:- dynamic(turned_on/1).
:- dynamic(location/2).
:- dynamic(door/2).
%Initalize static Relations
%room - holds a room name
room(outside).
room('war room').
room(hall).
room(armory).
room(barracks).
room('mess hall').
room(storage).
room(treasury).
room('hidden hall').
room(temple).
room('hidden room').

%location - holds something and the room it is in 
location(flowers, outside).

location(lantern, hall).

location(bed, barracks).

location(table, 'war room').
location(chair, 'war room').
location(banner, 'war room').
location(map, table).
location(dagger, table).

location(weapons, armory).
location(armor, armory).
location(key, armor).
location(oil, armory).

location(tapestry, temple).
location(statue, temple).

location('blade of thelonia', 'hidden room').

%Doors and connections
door(outside, hall).

door(hall, armory).
door(hall, 'war room').
door(hall, barracks).
door(hall, 'mess hall').
door(hall, treasury).
door(treasury, 'hidden hall').
door('hidden hall', temple).

door('mess hall', storage).

door(barracks, 'war room').

%lantern is off by default  
turned_off(lantern).

%statue order for third puzzle
statue1(stone).
statue2(blade).
statue3(king).

%Set starting room
here(outside).

%Set Dynamic Relations 
dynamic(here).
dynamic(have).
dynamic(location).
dynamic(door).
dynamic(turned_off).
dynamic(turned_on).

begin:-

  write('Blade of Thelonia'), nl,
  nl,
  write('The game has you playing as a young knight in the Thelonian army.'),nl,
  write('It has been years since Thelonia was a united land, since the fall of the'),nl,
  write('Mad King Vaeril the country has been shattered into pieces. It has been'),nl,
  write('prophesied by the Wizard Folmon that a King will rise from the ruins of'),nl,
  write('the past to lead Thelonia to prosperity once more. They will arrive'),nl,
  write('with world at their command and the blade of Thelonia at their side....'),nl,
  write('so here you arrive at the last known location of the legendary blade.'),nl,
  nl,
  write('You play the game through the use of simple English sentences'),nl,
  write('expressing an action wish to do. You can move to'),nl,
  write('rooms, look around, look in things, take things,'),nl,
  write('drop things, view inventory,'),nl,
  write('and turn things off & on'),nl,
  nl,
  write('Type "help" if you need help on commands.'),nl,
  write('Type "quit" if you wish to end the game.'),nl,
  nl,
  write('Hit any key to start the game.'),get0(_),
  write('Good luck my Lord!'),
  nl,
  nl,
  start.


%Create start loop
start:-
  look,
  repeat,
  write('>> '),
  get_command(X),
  first_puzzle(X),
  second_puzzle(X),
  third_puzzle(X),
  do(X), nl,
  end_condition(X).

%End Contitions
end_condition(end).
end_condition(_) :-
  have('blade of thelonia'),
  write('Congratulations my King').

%%Natural Language Parsing

get_command(C) :-
  input_list(L),
  command(CL,L),
  C =..  CL, !.
get_command(_) :-
  write('Command not recognized'), nl, fail.

%command - breaks down s sentence and parses the command from it 
command([V], InList):- 
  verbPart(V, InList-[]).

command([V,O], InList) :-
  verbPart(Object_Type, V, InList-S1),
  object(Object_Type, O, S1-[]).

%Object - a thing, or place related to the verb found prior (go to[verb] the[determiner] hall[place])
object(Type, N, S1-S3) :-
  determiner(S1-S2),
  nounPart(Type, N, S2-S3).
object(Type, N, S1-S2) :-
  nounPart(Type, N, S1-S2).

%Determiner - a, the, an, etc
determiner([the|X]- X).
determiner([a|X]-X).
determiner([an|X]-X).

%verbs
verbPart(look, [look|X]-X).
verbPart(look, [look,around|X]-X).
verbPart(inventory, [inventory|X]-X).
verbPart(help, [help|X]-X).
verbPart(end, [end|X]-X).
verbPart(end, [quit|X]-X).
verbPart(end, [good,bye|X]-X).

verbPart(place, goto, [go,to|X]-X).
verbPart(place, goto, [go|X]-X).
verbPart(place, goto, [move,to|X]-X).
verbPart(place, goto, [X|Y]-[X|Y]):- room(X).
verbPart(place, goto, [mess,hall|Y]-[mess,hall|Y]).
verbPart(place, goto, [war,room|Y]-[war,room|Y]).
verbPart(place, goto, [hidden,hall|Y]-[hidden,hall|Y]).

verbPart(thing, look_in, [look,in|X]-X).
verbPart(thing, look_in, [look,at|X]-X).
verbPart(thing, take, [take|X]-X).
verbPart(thing, drop, [drop|X]-X).
verbPart(thing, drop, [put|X]-X).
verbPart(thing, turn_on, [turn,on|X]-X).
verbPart(thing, turn_off, [turn,pff|X]-X).

%noun
nounPart(place, R, [R|X]-X):- room(R).
nounPart(place, 'mess hall', [mess,hall|X]-X).
nounPart(place, 'war room', [war,room|X]-X).
nounPart(place, 'hidden hall', [hidden,hall|X]-X).
nounPart(place, 'hidden room', [hidden,room|X]-X).

nounPart(thing, T, [T|X]-X):- location(T,_).
nounPart(thing, T, [T|X]-X):- have(T).
nounPart(thing, 'blade of thelonia', [blade|X]-X).
nounPart(thing, 'blade of thelonia', [blade,of,thelonia|X]-X).
nounPart(thing, lantern, [light|X], X):- have(lantern).

% read a line of words from the user

input_list(L) :-
  read_input(CL),
  list(L,CL,[]), !.

read_input(L) :-
  get0(C),
  createList(C,L).

createList(13,[]) :- !. 
createList(10,[]) :- !.
createList(C,[C|X]) :-
  get0(C2),
  createList(C2,X).
 
list([X|Y]) --> word(X), space, list(Y).
list([X]) --> space, list(X).
list([X]) --> word(X).
list([X]) --> word(X), space.

word(W) --> charList(X), {name(W,X)}.

charList([X|Y]) --> chr(X), charList(Y).
charList([X]) --> chr(X).

chr(X) --> [X],{X>=48}.

space --> wspace, space.
space --> wspace.

wspace --> [X], {X<48}.

%DO
do(goto(X)):-goto(X),!.
do(go(X)):-goto(X),!.
do(inventory):-inventory,!.
do(look):-look,!.
do(take(X)) :- take(X), !.
do(turn_on(X)) :- turn_on(X), !.
do(turn_off(X)) :- turn_off(X), !.
do(look_in(X)) :- look_in(X), !.
do(help):-help,!.
do(end).
do(_) :-
  write('Invalid command').

%help
help:-
  write('Type basic sentences to enter commands'),nl,
  write('Commands can be:'),nl,
  nl,
  write('   go to a location '),nl,
  write('   look around      '),nl,
  write('   look in place    '),nl,
  write('   take thing       '),nl,
  write('   drop thing       '),nl,
  write('   turn on thing    '),nl,
  write('   inventory        '),nl,
  nl,
  write('Click any key to continue.'),nl,
  get0(_),
  look.

%Check if rooms are connected 
connect(X,Y) :- door(X,Y).

connect(X,Y) :- door(Y,X).

% List Items in a Place
list_things(Place) :-
    location(X,Place),
    tab(2),
    write(X),
    nl,
    fail.
list_things(AnyPlace).

%List connections
list_connections(Place) :-
    connect(Place,X),
    tab(2),
    write(X),
    nl,
    fail.
list_connections(_).

%look - Looks at the current room and displays what you can do
look :-
    here(Place),
    write('Your are in the '),write(Place), nl,
    write('You can see:'), nl,
    list_things(Place),
    write('You can go to'), nl,
    list_connections(Place).

%look_in - Look in a room and view the things in it 
look_in(tapestry) :-
    write('Looking at the tapestry you see a sword in a stone,'),nl,
    write('a man holding a shining blade aloft as people gather around,'),nl,
    write('and a king sitting upon his throne as kingdom under him prospers,'),nl.

look_in(Place) :-
    write('Looking at '),write(Place),write(' you see:'), 
    nl,
    list_things(Place).


% GO functions
%goto - Moves Player to a room
goto(Place) :-
    can_go(Place),
    move(Place),
    look.

%can go - Check if user can go to a room
can_go(Place):- 
  here(X),
  connect(X, Place).
can_go(Place):-
  write('You can''t get there from here.'), nl,
  fail.

%move - move the player into a new room 
move(Place) :-
    retract(here(X)),
    assertz(here(Place)).

%TAKE functions
%take - take an item
take(X) :-
    can_take(X),
    take_object(X).

can_take(Thing) :-
    here(Place),
    is_contained_in(Thing,Place).
can_take(Thing) :-
    write('There is no '), write(Thing), 
    write(' here.'), 
    nl,
    fail.

take_object(Thing) :-
    retract(location(Thing,_)),
    assertz(have(Thing)),
    write('taken '),write(Thing), 
    nl.

%PUT functions
%put - put an item somewhere
put_down(Thing):-
  have(Thing),
  here(Place),
  retract(have(Thing)),
  assertz(location(Thing,Place)).
put_down(Thing):-
    write('You have no '), write(Thing), 
    nl,
    fail.

%INVENTORY functions
%inventory - lists held items
inventory:-
  have(X), 
  write('You have: '),nl,
  list_items.
inventory:-
  write('You have nothing'),nl.

list_items:-
  have(X),
  tab(2),write(X),nl,
  fail.
list_items.

%TURN ON/OFF
%turn_on - turns on an item
turn_on(Thing):-
  turned_on(Thing),
  write(Thing),write(' already turned on'),
  nl,
  fail.
turn_on(Thing):-
  have(oil),
  turned_off(Thing),
  retract(turned_off(Thing)),
  assertz(turned_on(Thing)),
  write(Thing),write(' turned on').
turn_on(Thing):-
  write('You have no oil for the '),write(Thing),
  nl.

%turn_off - turns off an item
turn_off(Thing):-
  turned_off(Thing),
  write(Thing),write(' already turned off'),
  nl,
  fail.
turn_off(Thing):-
  turned_on(Thing),
  retract(turned_on(Thing)),
  assertz(turned_off(Thing)),
  write(Thing),write(' turned off').

%IS CONTAINED
%is_contained_in - finds items contained in other items 

is_contained_in(T1,T2) :-  
  location(T1,T2).

is_contained_in(T1,T2) :-
  location(X,T2),
  is_contained_in(T1,X).

%PUZZLES
%first_puzzle
first_puzzle(goto(treasury)):-
  have(key),
  !.

first_puzzle(goto(treasury)):-
  write('The door is locked. Maybe there is a key...'),nl,
  !, fail.

first_puzzle(_).

%second_puzzle
second_puzzle(goto(treasury)):-
  have(lantern),
  turned_on(lantern),
  !.

second_puzzle(goto(treasury)):-
  write('All the lanterns have run out. It''s to dark to see down here...'),nl,
  !, fail.

second_puzzle(_).

%third_puzzle
third_puzzle(look_in(statue)):-  
  write('There are three odd statues here:'),nl, 
  write('One is a man holding the blade aloft (blade)'),nl,
  write('Another is a sword in a stone (stone)'),nl,
  write('The last is a king on his throne (king)'),nl,
  nl,
  write('It seems the statues can be moved around (put a . after each response)'),nl,
  write('What statue will you place first? '),read(X),
  write('What statue will you place second? '),read(Y),
  write('What statue will you place third? '),read(Z),
  statue1(X),
  statue2(Y),
  statue3(Z),
  write('A click echoes in the room as the statue moves forward revealing a stairway..'),nl,
  assertz(door(temple,'hidden room')),
  !.

third_puzzle(look_in(statue)):-
  write('Nothing seems to happen..'),nl,
  !, fail.

third_puzzle(_).