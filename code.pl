
packet(X, Y, Z) :-
    %split_adapter(X),
    allow(A, B, C),
    validate_adapter(X, A), 
    validate_ethernet(Y, B),
    testIP(Z, C).


testIP([A|[B|_]], [E|[F|_]]) :-
    A=E,
    B=F.

% split_adapter(X) :-
%     split_string(X, " ", "", L),
%     length(L, I), 
%     I=2,
%     write(L),
%     adapterListSplit(K, L),
%     write(K).

% adapterListSplit(_, []) :-
%     false.

% adapterListSplit(K, [H|T]) :-
%     string_length(H, I),
%     I=1,
%     char_code(H, HC),
%     char_code(K, KC),
%     HC = KC;
%     adapterListSplit(K, T).

validate_adapter(X, L) :-
    %allow(L, _, _),
    split_string(L, "-", "", T),
    length(T, I),
    I=2,
    memberOfRange(X,T).
    
validate_adapter(X, L) :-
    %allow(L, _, _),
    split_string(L, ",", "", T),
    length(T, I),
    I >= 2,
    memberOfList(X, T).

validate_adapter(X, P) :-
    %allow(P, _, _),
    string_length(P, I),
    I=1,
    char_code(X, XC),
    char_code(P, PC),
    XC=PC.

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