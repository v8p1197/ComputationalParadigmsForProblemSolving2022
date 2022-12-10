%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The scripts converts a valid boolean expression is its equivalent Disjunctive
% Normal Form (DNF).
%
% Author: Vincenzo Petrone
%
% Documentation:
% dnf(E, F) is true if and only if F is the DNF of expression E.
% isdnf(E) is true if and only if E is a DNF.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------- %
% Known boolean rules %
% ------------------- %

% Distributive Property
dnf(A*(B+C), X + Y) :-
	dnf(A, SA), dnf(B, SB), dnf(C, SC),
	dnf(SA*SB, X), dnf(SA*SC, Y).

% De Morgan's OR Property
dnf(-(A+B), X) :-
	dnf(A, SA), dnf(B, SB),
	dnf(-SA, NSA), dnf(-SB, NSB),
	dnf(NSA * NSB, X).

% De Morgan's AND Property
dnf(-(A*B), NSA + NSB) :-
	dnf(A, SA), dnf(B, SB),
	dnf(-SA, NSA), dnf(-SB, NSB).

% Double negation Property
dnf(- - A, SA) :- dnf(A, SA).

% ----------- %
% Basic rules %
% ----------- %

% An expression is already a DNF if it is a literal
dnf(A, A) :- isliteral(A).

dnf(A+B, SA+SB) :-
	dnf(A,SA), dnf(B,SB).
dnf(A*B, SA*SB) :-
	dnf(A,SA), dnf(B,SB).

% An expression X is a literal if and only if it is either a variable...
isliteral(X) :- atom(X).
% ...or a negated variable
isliteral(-X) :- atom(X).

% --------- %
% Interface %
% --------- %

% An expression is in DNF if and only if its DNF is the expression itself
isdnf(E) :- dnf(E, E).
