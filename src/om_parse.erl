-module(om_parse).
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

expr([],           Acc) -> rewind(Acc,[],[]);
expr([{N,X}|T],[{typevar,Y}|Acc])  -> expr(T,[{N,X},{typevar,Y}|Acc]);
expr([{N,X}|T],[{C,Y}|Acc])        -> expr(T,[{app,{{C,Y},{N,X}}}|Acc]);
expr([close   |T], Acc) -> {T1,Acc1}=rewind(Acc,T,[]), expr(T1,Acc1);
expr([star    |T], Acc) -> expr(T,[{const,star}|Acc]);
expr([open    |T], Acc) -> expr(T,[{open}|Acc]);
expr([arrow   |T], Acc) -> expr(T,[{arrow}|Acc]);
expr([lambda  |T], Acc) -> expr(T,[{lambda}|Acc]);
expr([pi      |T], Acc) -> expr(T,[{pi}|Acc]);
expr([colon   |T], Acc) -> expr(T,[{colon}|Acc]);
expr([{var,L},colon|T],Acc) -> expr(T,[{typevar,L}|Acc]);
expr([{var,L}|T],      Acc) -> expr(T,[{var,L}|Acc]).

rewind([],                 T,         Rest) -> {T,Rest};
rewind([{arrow},{C,Y}|Acc],T, [{N,X}|Rest]) -> rewind(Acc,T,[{arrow,{{C,Y},{N,X}}}|Rest]);
rewind([{Fun}|Acc],T, [{arrow,{{app,{{typevar,Label},X}},Y}}|Rest]) when Fun==lambda;Fun==pi -> rewind(Acc,T,[{Fun,{{arg,Label},X,Y}}|Rest]);
rewind([{N,X}|Acc],T, [{C,Y}|Rest]) -> rewind(Acc,T,[{app,{{N,X},{C,Y}}}|Rest]);
rewind([{N,X}|Acc],T, Rest) -> rewind(Acc,T,[{N,X}|Rest]);
rewind([{open},{N,X}|Acc],T,[{C,Y}|Rest]) -> rewind(Acc,T,[{app,{{N,X},{C,Y}}}|Rest]);
rewind([{open}|Acc],T, Rest) -> {T,lists:flatten([Rest|Acc])}.
