-module(om_type).
-description('Type Checker').
-compile(export_all).

getStar({star,N}) -> N;
getStar(_) -> erlang:error("*").

assertFunc({{"∀",ArgName},{ArgType,OutType}}) -> true;
assertFunc(T) -> erlang:error(["∀",T]).

assertVar(Name,Bind)       -> assertVar(Name,Bind,proplists:is_defined(Name,Bind)).
assertVar(Name,Bind,true)  -> true;
assertVar(Name,Bind,false) -> erlang:error(["free var", Name, Bind]).

hierarchy(Arg,Out) -> Out. % impredicative
%hierarchy(Arg,Out) -> max(Arg,Out). % predicative

substVar(Term,Name,Value) -> substVar(Term,Name,Value,0).
substVar({"→",{ArgType,OutType}},Name,Value,L)           -> {"→",{substVar(ArgType,Name,Value,L),substVar(OutType,Name,Value,L)}};
substVar({{"∀",{Name,0}},{ArgType,OutType}},Name,Value,L)    -> {{"∀",{Name,0}},{substVar(ArgType,Name,Value,L),substVar(OutType,Name,Value,L+1)}};
substVar({{"∀",{ArgName,N}},{ArgType,OutType}},Name,Value,L) -> {{"∀",{ArgName,N}},{substVar(ArgType,Name,Value,L),substVar(OutType,Name,Value,L)}};
substVar({{"λ",{Name,0}},{ArgType,OutTerm}},Name,Value,L)    -> {{"λ",{Name,0}},{substVar(ArgType,Name,Value,L),substVar(OutTerm,Name,Value,L+1)}};
substVar({{"λ",{ArgName,N}},{ArgType,OutType}},Name,Value,L) -> {{"λ",{ArgName,N}},{substVar(ArgType,Name,Value,L),substVar(OutType,Name,Value,L)}};
substVar({app,{Func,Arg}},Name,Value,L)                  -> {app,{substVar(Func,Name,Value,L),substVar(Arg,Name,Value,L)}};
substVar({var,{Name,L}},Name,Value,L)                    -> Value; % index match
substVar({var,{VarName,I}},Name,Value,L)                 -> {var,{VarName,I}}; % no match
substVar({star,N},Name,Value,L)                       -> {star,N}.

assertEqual(T,T) -> true;
assertEqual({{"∀",{"_",0}},X},{"→",Y}) -> assertEqual(X,Y);
%assertEqual({"→",{I1,O1}},{{"∀",{"_",0}},{I2,O2}}) -> assertEqual(O2,substVar(O1,I2,{var,{I1,0}},0));
%assertEqual({"→",{I1,O1}},{{"∀",{_,0}},{I2,O2}}) -> assertEqual(O2,substVar(O1,I2,{var,{I1,0}},0));
assertEqual({{"∀",{ArgName1,0}},{ArgType1,OutType1}},{{"∀",{ArgName2,0}},{ArgType2,OutType2}}) ->
    assertEqual(ArgType1,ArgType2), assertEqual(OutType1,substVar(OutType2,ArgName2,{var,{ArgName1,0}},0));
assertEqual({{"λ",{ArgName1,0}},{ArgType1,OutType1}},{{"λ",{ArgName2,0}},{ArgType2,OutType2}}) ->
    assertEqual(ArgType1,ArgType2), assertEqual(OutType1,substVar(OutType2,ArgName2,{var,{ArgName1,0}},0));
assertEqual({app,{Func1,Arg1}},{app,{Func2,Arg2}}) -> assertEqual(Func1,Func2), assertEqual(Arg1,Arg2);
assertEqual({var,{Name,I}},{var,{Name,I}}) -> true;
assertEqual({star,N},{star,N}) -> true;
assertEqual(A,B) -> erlang:error(["==", A, B]).

getType(Term) -> getType(Term, []). % closed term (w/o free vars)

getType({"→",{ArgType,OutType}},Bind) -> ArgLevel = getStar(getType(ArgType,Bind)), OutLevel = getStar(getType(OutType,Bind)), {star,hierarchy(ArgLevel,OutLevel)};
getType({{"∀",{ArgName,0}},{ArgType,OutType}},Bind) -> ArgLevel  = getStar(getType(ArgType,Bind)), NormArgType = normalize(ArgType), OutLevel = getStar(getType(OutType,[{ArgName,NormArgType}|Bind])), {star,hierarchy(ArgLevel,OutLevel)};
getType({{"λ",{ArgName,0}},{ArgType,OutTerm}},Bind) -> TArg  = getType(ArgType,Bind), ArgLevel = getStar(TArg), NormArgType = normalize(ArgType), TOut = getType(OutTerm,[{ArgName,NormArgType}|Bind]), {{"∀",{ArgName,0}},{NormArgType,TOut}};
getType({app,{Func,Arg}},Bind)                  -> TFunc = getType(Func,Bind),    assertFunc(TFunc),  {{"∀",{ArgName,0}},{ArgType,OutType}} = TFunc, TArg = getType(Arg,Bind), assertEqual(ArgType,TArg), normalize(substVar(OutType,ArgName,normalize(Arg)));
getType({var,{Name,I}},Bind)                    -> assertVar(Name,Bind), proplists:get_value(Name,Bind); % TODO respect index of var
getType({star,N},Bind) -> {star,N+1}.

normalize({"→",{ArgType,OutType}}) -> {{"∀",{"_",0}},{normalize(ArgType),normalize(OutType)}};
normalize({{"∀",{ArgName,0}},{ArgType,OutType}}) -> {{"∀",{ArgName,0}},{normalize(ArgType),normalize(OutType)}};
normalize({{"λ",{ArgName,0}},{ArgType,OutTerm}}) -> {{"λ",{ArgName,0}},{normalize(ArgType),normalize(OutTerm)}};
normalize({app,{{{"λ",{ArgName,0}},{ArgType,OutTerm}},ArgValue}}) -> normalize(substVar(OutTerm,ArgName,normalize(ArgValue)));
normalize({app,{Func,Arg}}) -> {app,{normalize(Func),normalize(Arg)}};
normalize({var,{Name,I}}) -> {var,{Name,I}};
normalize({star,N}) -> {star,N}.

toString(Term) -> unicode:characters_to_binary(toString(Term,"")).

toString({"→",{ArgType,OutType}},Str) -> "("++toString(ArgType," → "++toString(OutType,")"++Str));
toString({{"∀",{ArgName,0}},{ArgType,OutType}},Str) -> "(∀("++atom_to_list(ArgName)++":"++toString(ArgType,") → "++toString(OutType,")"++Str));
toString({{"λ",{ArgName,0}},{ArgType,OutTerm}},Str) -> "(λ("++atom_to_list(ArgName)++":"++toString(ArgType,") → "++toString(OutTerm,")"++Str));
toString({app,{Func,Arg}},Str) -> "("++toString(Func," "++toString(Arg,")"++Str));
toString({var,{Name,I}},Str) -> atom_to_list(Name)++Str;
toString({star,N},Str) -> "*"++integer_to_list(N)++Str.
