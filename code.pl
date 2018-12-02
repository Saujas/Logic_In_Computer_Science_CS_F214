:- consult(db).

%!!---------MULTIPLE TYPES OF RESPONSES FOR THE PACKET THROUGH THE FIREWALL---------!!

%The packet is allow to go through when it matches with an allow rule in the database. An acceptance message is printed.
packet(X, Y, Z, W, T) :-
    allow(A, B, C, D, E),
    testAdapter(X, A), 
    testEthernet(Y, B),
    testIP(Z, C),
    testICMP(W, D),
    testTcpUdp(T, E),
    write("Packet accepted!").

%The packet is rejected when it matches with a reject rule in the database, printing a rejection message.
packet(X, Y, Z, W, T) :-
    reject(A, B, C, D, E),
    testAdapter(X, A), 
    testEthernet(Y, B),
    testIP(Z, C),
    testICMP(W, D),
    testTcpUdp(T, E),
    write("Packet rejected!"), false.

%The packet is dropped silently when a corresponding drop rule exists in the database. Nothing is printed in this case.
packet(X, Y, Z, W, T) :-
    drop(A, B, C, D, E),
    testAdapter(X, A), 
    testEthernet(Y, B),
    testIP(Z, C),
    testICMP(W, D),
    testTcpUdp(T, E),
    false.


%!!---------TESTING THE FIRST CLAUSE OF THE PACKET, THE ADAPTER CLAUSE---------!!

%The adapter matches when it is found in adapter clause's 'hyphen' separated rule in the database.
%Examples - 'A-C', 'B-F'
testAdapter(X, L) :-
    \+X='any',
    split_string(L, "-", "", T),
    length(T, I),
    I=2,
    memberOfRange(X,T).
    
%The adapter matches when it is found in adapter clause's'comma' separated rule in the database.
%Examples - 'L,M', 'S,T,Y'
testAdapter(X, L) :-
    \+X='any',
    split_string(L, ",", "", T),
    length(T, I),
    I >= 2,
    memberOfList(X, T).

%The adapter matches when it is found in adapter clause's single character rule in the database.
%Examples - 'A-C', 'B-F'
testAdapter(X, P) :-
    \+X='any',
    string_length(P, I),
    I=1,
    char_code(X, XC),
    char_code(P, PC),
    XC=PC.

%The adapter matches when it is not meant to have a clause. That happens when 'any' is given as an input to the adapter clause in the packet.
testAdapter(X, A) :-
    X=A,
    A='any'.


%!!---------TESTING THE SECOND CLAUSE OF THE PACKET, THE ETHERNET CLAUSE---------!!

%The clause is satisfied only when both the elements of the list (vid and proto) satisfy corresponding predicates - testVid and testProto writing afterwards.
testEthernet([V|[P|_]], [X, Y]) :-
    testVid(V, X),
    testProto(P, Y).

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


%!!---------TESTING THE FOURTH CLAUSE OF THE PACKET, THE ICMP CLAUSE---------!!

%The clause is satisfied only when both the elements of the list (test and code) satisfy corresponding predicates.
%Since they work on the same principles as testVid and testProto, they must be satisfied here as well.
testICMP([V|[P|_]], [X, Y]) :-
    testVid(V, X),
    testProto(P, Y).


%!!---------TESTING THE FIFTH CLAUSE OF THE PACKET, THE TCP/UDP CLAUSE---------!!

%The clause is satisfied when source and destination addresses (first and second elements of the input list) match
%AND
%The third elements corresponds to either 'tcp' or 'udp'. 
testTcpUdp([X, Y, Z], [A, B, C]) :-
    testSrcDst(X, A),
    testSrcDst(Y, B),
    testName(Z, C).

%Tests for the third element of TCP/UDP clause to be 'udp'
testName(A, B) :-
    A=B,
    B='udp'.

%Tests for the third element of TCP/UDP clause to be 'tcp'
testName(A, B) :-
    A=B,
    B='tcp'.

%Tests the source and destination ports of TCP/UDP when any (or both) of them match(es) a 'hyphen' separated rule in the database.
%Tests if the input is in the correct range as per the Internet Assigned Numbers Authority.
%Examples - 4000-5000
testSrcDst(X, E) :-
    split_string(E, "-", "", T),
    length(T, I),
    I=2,
    memberOfNumberRange(X,T),
    X=<65535,
    X>=0.

%Tests the source and destination ports of TCP/UDP when any (or both) of them match(es) a 'comma' separated rule in the database.
%Tests if the input is in the correct range as per the Internet Assigned Numbers Authority.
%Examples - 4000,4001
testSrcDst(X, E) :-
    split_string(E, ",", "", T),
    length(T, I),
    I >= 2,
    memberOfNumberList(X, T),
    X=<65535,
    X>=0.

%Tests the source and destination ports of TCP/UDP when any (or both) of them match(es) a simple rule - without 'hyphen'/'comma' in the database.
%Tests if the input is in the correct range as per the Internet Assigned Numbers Authority.
testSrcDst(X, E) :-
    string_to_list(E, S),
    checkValidNumber(S),
    atom_number(E, EN),
    X=EN,
    X=<65535,
    X>=0.

%Tests the source and destination addresses of TCP/UDP when any (or both) of them are not meant to have an input.
%In our rulebook, this happens when the keyword 'null' is used in the place of the address.
%This checks the input for that 'null' case.
testSrcDst(X, E) :-
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
