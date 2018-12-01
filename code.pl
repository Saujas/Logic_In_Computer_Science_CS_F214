:- consult(db).

packet(X, Y, Z) :-
    allow(A, B, C),
    validate_adapter(X, A), 
    validate_ethernet(Y, B),
    testIP(Z, C).


testIP([A|[B|_]], [E|[F|_]]) :-
    A=E,
    B=F.

testIP([A, B], [E, F]) :-
    rangeIP(E, A),  
    B=F.

testIP([A, B], [E, F]) :-
    mask(E, A),
    B=F.

testIP([A, B], [E, F]) :-
    rangeIP(F, B),  
    A=E.

testIP([A, B], [E, F]) :-
    mask(F, B),
    A=E.

testIP([A, B], [E, F]) :-
    rangeIP(E, A),
    rangeIP(F, B).

testIP([A, B], [E, F]) :-
    rangeIP(E, A), 
    mask(F, B).

testIP([A, B], [E, F]) :-
    mask(E, A),
    mask(F, B).

testIP([A, B], [E, F]) :-
    mask(E, A),
    rangeIP(F, B).

testIP([A, B], [E, F]) :-
    A=E,
    commaIP(F, B).

testIP([A, B], [E, F]) :-
    rangeIP(E, A), 
    commaIP(F, B).

testIP([A, B], [E, F]) :-
    mask(E, A),
    commaIP(F, B).

testIP([A, B], [E, F]) :-
    B=F,
    commaIP(E, A).

testIP([A, B], [E, F]) :-
    rangeIP(F, B),
    commaIP(E, A).

testIP([A, B], [E, F]) :-
    mask(F, B),
    commaIP(E, A).

testIP([A, B], [E, F]) :-
    commaIP(E, A),
    commaIP(F, B).

commaIP(X, A) :-
    split_string(X, ",", "", L),
    member(C, L),
    atom_string(A, C).


mask(X, B) :-
    split_string(B, "/", "", L),
    X=B,
    lastEl(C, L), 
    C=<32,
    C>=1.


check([L1, L2, L3 | _], [M1, M2, M3 | _]) :-
    L1=M1,
    L2=M2, 
    L3=M3.

rangeIP(X, I) :-
    split_string(X, "-", "", L), 
    splitNo(L, I).

splitNo([A, B], I) :-
    split_string(A, ".", "", L),
    split_string(B, ".", "", M),
    split_string(I, ".", "", N), 
    lastEl(Q, L),
    lastEl(R, M),
    lastEl(S, N), 
    memberofNumberAsString(Q, R, S, L, M).  

lastEl(X,[X]).

lastEl(X,[_|Z]) :- 
    lastEl(X,Z).

memberofNumberAsString(Q, R, S, [L1, L2, L3 | _], [M1, M2, M3 | _]) :-
    L1=M1,
    L2=M2,
    L3=M3,
    number_codes(X, Q), 
    number_codes(Y, R), 
    number_codes(Z, S), 
    Z>=X, 
    Z=<Y.



validate_adapter(X, L) :-
    \+X='any',
    %allow(L, _, _),
    split_string(L, "-", "", T),
    length(T, I),
    I=2,
    memberOfRange(X,T).
    
validate_adapter(X, L) :-
    \+X='any',
    %allow(L, _, _),
    split_string(L, ",", "", T),
    length(T, I),
    I >= 2,
    memberOfList(X, T).

validate_adapter(X, P) :-
    \+X='any',
    %allow(P, _, _),
    string_length(P, I),
    I=1,
    char_code(X, XC),
    char_code(P, PC),
    XC=PC.

validate_adapter(X, A) :-
    X=A,
    A='any'.

validate_ethernet([V|[P|_]], [X, Y]) :-
    validate_vid(V, X),
    validate_proto(P, Y).

validate_vid(X, E) :-
    %allow(_, [E|_], _),
    split_string(E, "-", "", T),
    length(T, I),
    I=2,
    memberOfNumberRange(X,T).

validate_vid(X, E) :-
    %allow(_, [E|_], _),
    split_string(E, ",", "", T),
    length(T, I),
    I >= 2,
    memberOfNumberList(X, T).

validate_vid(X, E) :-
    %allow(_, [E|_], _),
    string_to_list(E, S),
    checkValidNumber(S),
    atom_number(E, EN),
    X=EN.

validate_vid(X, E) :-
    X=E,
    E='null'.


validate_proto(X, E) :-
    %allow(_, [_|[E|_]], _),
    split_string(E, "-", "", T),
    length(T, I),
    I=2,
    memberOfNumberRange(X,T).

validate_proto(X, E) :-
    %allow(_, [_|[E|_]], _),
    split_string(E, ",", "", T),
    length(T, I),
    I >= 2,
    memberOfNumberList(X, T).

validate_proto(X, E) :-
    %allow(_, [_|[E|_]], _),
    string_to_list(E, S),
    checkValidNumber(S),
    atom_number(E, EN),
    X=EN.

validate_proto(X, E) :-
    X=E,
    E='null'.



memberOfList(_, []) :-
    false.

memberOfList(X, [H|T]) :-
    char_code(H, HC),
    char_code(X, XC),
    HC = XC;
    memberOfList(X, T).

memberOfRange(X, [S|[E|_]]) :-
    char_code(S, SC),
    char_code(E, EC),
    char_code(X, XC),
    XC >= SC,
    XC =< EC.

memberOfNumberRange(X, [S|[E|_]]) :-
    atom_number(S, SN),
    atom_number(E, EN),
    X >= SN,
    X =< EN.

memberOfNumberList(_, []) :-
    false.

memberOfNumberList(X, [H|T]) :-
    atom_number(H, HE),
    X = HE;
    memberOfNumberList(X, T).

checkValidNumber([]) :-
    true.

checkValidNumber([H|T]) :-
    between(48,57, H),
    checkValidNumber(T);
    false.