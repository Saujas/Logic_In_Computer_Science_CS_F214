:- consult(db).

packet(X, Y, Z, W, T) :-
    allow(A, B, C, D, E),
    testAdapter(X, A), 
    testEthernet(Y, B),
    testIP(Z, C),
    testICMP(W, D),
    testTcpUdp(T, E),
    write("Packet accepted!").

packet(X, Y, Z, W, T) :-
    reject(A, B, C, D, E),
    testAdapter(X, A), 
    testEthernet(Y, B),
    testIP(Z, C),
    testICMP(W, D),
    testTcpUdp(T, E),
    write("Packet rejected!"), false.

packet(X, Y, Z, W, T) :-
    drop(A, B, C, D, E),
    testAdapter(X, A), 
    testEthernet(Y, B),
    testIP(Z, C),
    testICMP(W, D),
    testTcpUdp(T, E),
    false.

testAdapter(X, L) :-
    \+X='any',
    split_string(L, "-", "", T),
    length(T, I),
    I=2,
    memberOfRange(X,T).
    
testAdapter(X, L) :-
    \+X='any',
    split_string(L, ",", "", T),
    length(T, I),
    I >= 2,
    memberOfList(X, T).

testAdapter(X, P) :-
    \+X='any',
    string_length(P, I),
    I=1,
    char_code(X, XC),
    char_code(P, PC),
    XC=PC.

testAdapter(X, A) :-
    X=A,
    A='any'.

testEthernet([V|[P|_]], [X, Y]) :-
    testVid(V, X),
    testProto(P, Y).

testICMP([V|[P|_]], [X, Y]) :-
    testVid(V, X),
    testProto(P, Y).

testTcpUdp([X, Y, Z], [A, B, C]) :-
    testSrcDst(X, A),
    testSrcDst(Y, B),
    testName(Z, C).

testName(A, B) :-
    A=B,
    B='udp'.

testName(A, B) :-
    A=B,
    B='tdp'.

testSrcDst(X, E) :-
    split_string(E, "-", "", T),
    length(T, I),
    I=2,
    memberOfNumberRange(X,T),
    X=<65535,
    X>=0.

testSrcDst(X, E) :-
    split_string(E, ",", "", T),
    length(T, I),
    I >= 2,
    memberOfNumberList(X, T),
    X=<65535,
    X>=0.


testSrcDst(X, E) :-
    string_to_list(E, S),
    checkValidNumber(S),
    atom_number(E, EN),
    X=EN,
    X=<65535,
    X>=0.

testSrcDst(X, E) :-
    X=E,
    E='null'.

testVid(X, E) :-
    split_string(E, "-", "", T),
    length(T, I),
    I=2,
    memberOfNumberRange(X,T).

testVid(X, E) :-
    split_string(E, ",", "", T),
    length(T, I),
    I >= 2,
    memberOfNumberList(X, T).

testVid(X, E) :-
    string_to_list(E, S),
    checkValidNumber(S),
    atom_number(E, EN),
    X=EN.

testVid(X, E) :-
    X=E,
    E='null'.


testProto(X, E) :-
    split_string(E, "-", "", T),
    length(T, I),
    I=2,
    memberOfNumberRange(X,T).

testProto(X, E) :-
    split_string(E, ",", "", T),
    length(T, I),
    I >= 2,
    memberOfNumberList(X, T).

testProto(X, E) :-
    string_to_list(E, S),
    checkValidNumber(S),
    atom_number(E, EN),
    X=EN.

testProto(X, E) :-
    X=E,
    E='null'.



testIP([A|[B|C]], [E|[F|G]]) :-
    srcIP(A, E),
    dstIP(B, F),
    protoIP(C, G).

protoIP(C, G) :-
    C=G. 


srcIP(A, E) :-
    A=E.

srcIP(A, E) :-
    mask(E, A).

srcIP(A, E) :-
    rangeIP(E, A).

srcIP(A, E) :-
    commaIP(E, A).


dstIP(B, F) :-
    B=F.


dstIP(B, F) :-
    rangeIP(F, B).

dstIP(B, F) :-
    mask(F, B).

dstIP(B, F) :-
    commaIP(F, B).


commaIP(X, A) :-
    split_string(X, ",", "", L),
    member(C, L),
    atom_string(A, C).


mask(X, B) :-
    X=B,
    \+B='null',
    split_string(B, "/", "", L),
    lastEl(C, L),
    length(L, I),
    I=2,
    C=<32,
    C>=1.


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
    memberofNumberAsString(Q, R, S, L, N).  

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
