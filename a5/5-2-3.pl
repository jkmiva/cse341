% Name: Huang Jiaming
% NSID: jih211
% stuID:11207964

% 2.
% female(person) % person is female
female(elizabeth).
female(diana).
female(camilla).
female(catherine).
female(charlotte).

% male(person) % person is male
male(philip).
male(charles).
male(william).
male(harry).
male(george).

% married_to(person1, person2) % person1 is/was married to person2
married_to(elizabeth, philip).
married_to(diana, charles).
married_to(catherine, william).
married_to(camilla, charles).

% child_of(person1, person2) % person1 is a child of person2
child_of(charles, elizabeth).
child_of(charles, philip).
child_of(william, diana).
child_of(william, charles).
child_of(harry, diana).
child_of(harry, charles).
child_of(george, william).
child_of(george, catherine).
child_of(charlotte, william).
child_of(charlotte, catherine).

% 3.
% a) aunt_of(person1, person2) % person1 is aunt of person2
aunt_of(X, Y) :-
    female(X),
    child_of(Y, A),
    child_of(A, B),
    child_of(X, B).

% b) grandchild_of(person1, person2) % person1 is grandchild of person2
grandchild_of(X, Y) :-
    child_of(X, A),
    child_of(A, Y).

% c) mother_of(person1, person2) % person1 is mother of person2
mother_of(X, Y) :-
    female(X),
    child_of(Y, X).

% d) stepmother_of(person1, person2) % person1 is stepmother of person2
stepmother_of(X, Y) :-
    female(X),
    not(child_of(Y, X)),
    child_of(Y, A),
    married_to(X, A).

% e) nephew_of(person1, person2) % person1 is nephew of person2
nephew_of(X, Y) :-
    male(X),
    not(child_of(X, Y)),
    child_of(X, A),
    child_of(A, B),
    child_of(Y, B).

% f) mother_in_law_of(person1, person2) % person1 is mother-in-law of person2
mother_in_law_of(X, Y) :-
    female(X),
    female(Y),
    married_to(Y, A),
    child_of(A, X).
mother_in_law_of(X, Y) :-
    female(X),
    male(Y),
    married_to(A, Y),
    child_of(A, X).

% g) brother_in_law_of(person1, person2) % person1 is brother-in-law of person2
brother_in_law_of(X, Y) :-
    male(X),
    female(Y),
    married_to(Y, A),
    child_of(A, B),
    child_of(X, B).
brother_in_law_of(X, Y) :-
    male(X),
    female(Y),
    married_to(Y, A),
    child_of(A, B),
    child_of(C, B),
    female(C),
    married_to(C, X).
brother_in_law_of(X, Y) :-
    male(X),
    male(Y),
    married_to(A, Y),
    child_of(A, B),
    child_of(X, B).
brother_in_law_of(X, Y) :-
    male(X),
    male(Y),
    married_to(A, Y),
    child_of(A, B),
    child_of(C, B),
    female(C),
    married_to(C, X).

% h) ancestor_of(person1, person2) % person1 is ancestor of person2
ancestor_of(X, Y) :-
    child_of(Y, X).
ancestor_of(X, Y) :-
    child_of(Y, A),
    ancestor_of(X, A).
