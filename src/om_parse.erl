-module(om_parse).
-description('Om Parser').
-compile(export_all).

%    Om/Henk/Morte Core Specification
%
%    EXPR :=                     EXPR             EXPR => {APP,[           I:EXPR, O:EXPR]}
%          | "λ"   "(" LABEL ":" EXPR ")" "arrow" EXPR => {LAM,[{ARG:LABEL,I:EXPR, O;EXPR]}
%          | "π"   "(" LABEL ":" EXPR ")" "arrow" EXPR => {PI, [{ATH:LABEL,I:EXPR, O;EXPR]}
%          |                     EXPR     "arrow" EXPR => {PI, [{"_",      I:EXPR, O;EXPR]}
%          |           LABEL                           => {VAR,LABEL}
%          | "*"                                       => {Star}
%          | "[]"                                      => {Box}
%          |       "("           EXPR ")"              => EXPR

% During forward pass we stack applications (except typevars), then
% on reaching close paren ")" we perform backward pass and stack arrows,
% until neaarest unstacked open paren "(" appeared (then we just return
% control to the forward pass).

% We need to preserve applies to typevars as they should
% be processes lately on rewind pass, so we have just typevars bypassing rule.
% On the rewind pass we stack lambdas by matching arrow/apply signatures
% where typevar(x) is introduction an variable "x" to the Gamma context.
%
%                   apply: (A->B) x A -> B
%                  lambda: arrow(app(typevar(x),A),B)
%

expr([],           Acc) -> rewind(Acc,[],[]);
expr([{N,X}|T],[{typevar,Y}|Acc])  -> expr(T,[{N,X},{typevar,Y}|Acc]);
expr([{N,X}|T],[{C,Y}|Acc])        -> expr(T,[{app,{{C,Y},{N,X}}}|Acc]);
expr([open    |T], Acc) -> expr(T,[{open}|Acc]);
expr([close   |T], Acc) -> {T1,Acc1}=rewind(Acc,T,[]), expr(T1,Acc1);
expr([star    |T], Acc) -> expr(T,[{const,star}|Acc]);
expr([arrow   |T], Acc) -> expr(T,[{arrow}|Acc]);
expr([lambda  |T], Acc) -> expr(T,[{lambda}|Acc]);
expr([pi      |T], Acc) -> expr(T,[{pi}|Acc]);
expr([colon   |T], Acc) -> expr(T,[{colon}|Acc]);
expr([{var,L},colon|T],Acc) -> expr(T,[{typevar,L}|Acc]);
expr([{var,L}|T],      Acc) -> expr(T,[{var,L}|Acc]).

rewind([{F}|Acc],         T, [{arrow,{{app,{{typevar,{L,_}},{A,X}}},{B,Y}}}|R]) when F == lambda; F== pi
                                        -> rewind(Acc,T,[{F,{{arg,L},{A,X},{B,Y}}}|R]);
rewind([{A,X}|Acc],       T, [{B,Y}|R]) -> rewind(Acc,T,[{app,{{A,X},{B,Y}}}|R]);
rewind([{A,X}|Acc],       T, R)         -> rewind(Acc,T,[{A,X}|R]);
rewind([{arrow},Y|Acc],   T, [X|R])     -> rewind(Acc,T,[{arrow,{Y,X}}|R]);
rewind([{open},{A,X}|Acc],T, [{B,Y}|R]) -> {T,om:flat([{app,{{A,X},{B,Y}}}|[R|Acc]])};
rewind([{open}|Acc],      T, R)         -> {T,om:flat([R|Acc])};
rewind([{colon}|Acc],     T, R)         -> {T,om:flat([R|Acc])};
rewind([],                T, R)         -> {T,R}.
