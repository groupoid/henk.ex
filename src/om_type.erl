-module(om_type).
-description('Type Checker').
-compile(export_all).

assertStar({const,"*"}) -> true;
assertStar(_) -> erlang:error("*").

assertFunc({{"∀",ArgName},{ArgType,OutType}}) -> true;
assertFunc(_) -> erlang:error("∀").

assertEqual(T,T) -> true;
assertEqual(_,_) -> erlang:error("must be equal").

assertVar(Name,Bind)       -> assertVar(Name,Bind,proplists:is_defined(Name,Bind)).
assertVar(Name,Bind,true)  -> true;
assertVar(Name,Bind,false) -> erlang:error("free var").

substVar({"→",{ArgType,OutType}},Name,Value)           -> {"→",{substVar(ArgType,Name,Value),substVar(OutType,Name,Value)}};
substVar({{"∀",Name},{ArgType,OutType}},Name,Value)    -> {{"∀",Name},{ArgType,OutType}};
substVar({{"∀",ArgName},{ArgType,OutType}},Name,Value) -> {{"∀",ArgName},{substVar(ArgType,Name,Value),substVar(OutType,Name,Value)}};
substVar({{"λ",Name},{ArgType,OutTerm}},Name,Value)    -> {{"λ",Name},{ArgType,OutTerm}};
substVar({{"λ",ArgName},{ArgType,OutType}},Name,Value) -> {{"λ",ArgName},{substVar(ArgType,Name,Value),substVar(OutType,Name,Value)}};
substVar({app,{Func,Arg}},Name,Value)                  -> {app,{substVar(Func,Name,Value),substVar(Arg,Name,Value)}};
substVar({var,{Name,I}},Name,Value)                    -> Value; %FIXME index
substVar({var,{VarName,I}},Name,Value)                 -> {var,{VarName,I}};
substVar({const,"*"},Name,Value)                       -> {const,"*"}.

getType(Term) -> getType(Term, []). %TODO BindVal=[{Name,{Term,Type}}]

getType({"→",{ArgType,OutType}},Bind) -> assertStar(getType(ArgType,Bind)), assertStar(getType(OutType,Bind));
getType({{"∀",ArgName},{ArgType,OutType}},Bind) -> TArg  = getType(ArgType,Bind), assertStar(TArg),   TRes = getType(OutType,[{ArgName,normalize(ArgType,TArg)}|Bind]), assertStar(TRes), TRes;
getType({{"λ",ArgName},{ArgType,OutTerm}},Bind) -> TArg  = getType(ArgType),      assertStar(TArg),   TRes = getType(OutTerm,[{ArgName,ArgType}|Bind]), assertStar(TRes), {{"∀",ArgName},{ArgType,TRes}};
getType({app,{Func,Arg}},Bind)                  -> TFunc = getType(Func,Bind),    assertFunc(TFunc),  {{"∀",ArgName},{ArgType,OutType}} = TFunc, TArg = getType(Arg,Bind), assertEqual(ArgType,TArg), substVar(OutType,ArgName,Arg);
getType({var,{Name,I}},Bind)                    -> assertVar(Name,Bind), proplists:get_value(Name,Bind);
getType({const,"*"},Bind)                       -> {const,"*"}. %FIXME levels

normalize(ArgType,TArg) -> TArg.

%erasure(Term,Type)
