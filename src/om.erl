-module(om).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, main/1]).
-compile(export_all).

main(A)    -> mad:main(A).
start()    -> start(normal,[]).
start(_,_) -> supervisor:start_link({local,om},om,[]).
stop(_)    -> ok.
init([]) -> {ok, {{one_for_one, 5, 10}, []}}.

read() -> Bin = file(),
          io:format("~ts~n",[unicode:characters_to_list(Bin)]), om_tok:tokens(Bin,0,{1,[]},[]).
          main() -> io:fwrite("~120p~n",[om_parse:expr(read(),[])]).


file() ->  {ok,Bin2} = file:read_file(%code:priv_dir(om)++
                                      "priv/fun.txt"),
          %<<"( ( a → (b x) → x) → ( c → d ) ) ( a b ) a b"/utf8>>.
          %<<"(c → (a → c) → v)"/utf8>>.
%          <<"(λ (a: c → (a (x a)) → v) → λ (a:*) → *)"/utf8>>.
          Bin2.

