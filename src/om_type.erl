-module(om_type).
-description('Type Checker').
-compile(export_all).

getStar({star,N}) -> N;
getStar(_) -> erlang:error("*").

assertFunc({{"∀",ArgName},{ArgType,OutType}}) -> true;
assertFunc(_) -> erlang:error("∀").

assertEqual(T,T) -> true;
assertEqual(_,_) -> erlang:error("must be equal").

assertVar(Name,Bind)       -> assertVar(Name,Bind,proplists:is_defined(Name,Bind)).
assertVar(Name,Bind,true)  -> true;
assertVar(Name,Bind,false) -> erlang:error("free var").

%hierarchy(Arg,Out) -> Out. % impredicative
hierarchy(Arg,Out) -> max(Arg,Out). % predicative

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

getType(Term) -> getType(Term, []). % closed term (w/o free vars)

getType({"→",{ArgType,OutType}},Bind) -> ArgLevel = getStar(getType(ArgType,Bind)), OutLevel = getStar(getType(OutType,Bind)), {star,hierarchy(ArgLevel,OutLevel)};
getType({{"∀",{ArgName,0}},{ArgType,OutType}},Bind) -> ArgLevel  = getStar(getType(ArgType,Bind)), OutLevel = getStar(getType(OutType,[{ArgName,normalize(ArgType)}|Bind])), {star,hierarchy(ArgLevel,OutLevel)};
getType({{"λ",{ArgName,0}},{ArgType,OutTerm}},Bind) -> TArg  = getType(ArgType), ArgLevel = getStar(TArg),   TOut = getType(OutTerm,[{ArgName,normalize(ArgType)}|Bind]), OutType = getStar(TOut), {{"∀",{ArgName,0}},{ArgType,TOut}};
getType({app,{Func,Arg}},Bind)                  -> TFunc = getType(Func,Bind),    assertFunc(TFunc),  {{"∀",{ArgName,0}},{ArgType,OutType}} = TFunc, TArg = getType(Arg,Bind), assertEqual(ArgType,TArg), normalize(substVar(OutType,ArgName,normalize(Arg)));
getType({var,{Name,I}},Bind)                    -> assertVar(Name,Bind), proplists:get_value(Name,Bind); % TODO respect index of var
getType({star,N},Bind)                       -> {star,N+1}.

normalize({"→",{ArgType,OutType}}) -> {"→",{normalize(ArgType),normalize(OutType)}};
normalize({{"∀",{ArgName,0}},{ArgType,OutType}}) -> {{"∀",{ArgName,0}},{normalize(ArgType),normalize(OutType)}};
normalize({{"λ",{ArgName,0}},{ArgType,OutTerm}}) -> {{"λ",{ArgName,0}},{normalize(ArgType),normalize(OutTerm)}};
normalize({app,{{{"λ",{ArgName,0}},{ArgType,OutTerm}},ArgValue}}) -> normalize(substVar(OutTerm,ArgName,normalize(ArgValue)));
normalize({app,{Func,Arg}}) -> {app,{normalize(Func),normalize(Arg)}};
normalize({var,{Name,I}}) -> {var,{Name,I}};
normalize({star,N}) -> {star,N}.
