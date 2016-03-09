-module(om_SUITE).
-compile(export_all).

suite()  -> [{timetrap,   {seconds,30}}].
all()    -> [{group,all}, {group, parser}].
groups() -> [{all,    [], [generic]},
             {parser, [], [parser_errors]}].

init_per_suite(Config) ->  Config.
end_per_suite(_Config) -> ok.

generic(Config) ->
    om:mode("hurkens"),
    mad_repl:ets_created(),
    application:set_env(om,priv,mad_repl:cwd()++"/../../priv"),
    All = om:all(),
    ct:log("~tp~n",[All]),
    true = lists:all(fun({Mode,Status,Tests}) -> Status /= om:pass(false) end, All),
    ok.

parser_errors(Config) ->
    ct:log("~tp~n",[om_parse:test()]),
    ok.
