%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The scripts converts a valid boolean expression is its equivalent Disjunctive
% Normal Form (DNF).
%
% Author: Vincenzo Petrone
%
% Documentation:
% dnf(E, F) is true if and only if F is the DNF of expression E.
% 
% Examples:
% * dnf(-(a+b), E).
%   gives
%   E = -a * -b
% * dnf(x*(a+b), E).
%   gives
%   E = x*a + x*b
% * dnf(- -x * (- -a + -b * (c + -d)), E).
%   gives
%   E = x*a + (x * (-b * c) + x* (-b * -d))
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------- %
% Known boolean rules %
% ------------------- %

% Distributive Property
dnf(A*(B+C), X + Y) :-
	dnf(A, SA), dnf(B, SB), dnf(C, SC),
	dnf(SA*SB, X), dnf(SA*SC, Y).
dnf((A+B)*C, X + Y) :-
	dnf(A, SA), dnf(B, SB), dnf(C, SC),
	dnf(SA*SC, X), dnf(SB*SC, Y).
dnf((A+B)*(C+D), W + X + Y + Z) :-
	dnf(A, SA), dnf(B, SB), dnf(C, SC), dnf(D, SD),
	dnf(SA*SC, W), dnf(SA*SD, X), dnf(SB*SC, Y), dnf(SB*SD, Z).

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

% Simplify OR and AND with the DNFs
dnf(A+B, SA+SB) :- dnf(A,SA), dnf(B,SB).
dnf(A*B, SA*SB) :- dnf(A,SA), dnf(B,SB).

% An expression X is a literal if and only if it is either a variable...
isliteral(X) :- atom(X).
% ...or a negated variable
isliteral(-X) :- atom(X).
