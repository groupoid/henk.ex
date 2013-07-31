Definitions.

D       = [0-9]
U       = [A-Z]
L       = [a-z]
A       = ({U}|{L}|{D}|_)
WS      = ([\000-\s]|%.*)

Rules.

[]()[}{|!?/;:,.*+#<>=-] : {token,{list_to_atom(TokenChars),TokenLine}}.
-> : {token,{'->',TokenLine}}.
({U}|_){A}* : {token,{var,TokenLine,list_to_atom(TokenChars)}}.
{L}{A}* : Atom = list_to_atom(TokenChars),
          {token,case reserved_word(Atom) of 
                      true -> {Atom,TokenLine};
                      false -> {atom,TokenLine,Atom} end}.
{WS}+ : skip_token.

Erlang code.

reserved_word(E) -> lists:member(E,['fun','sum','product','cat','end']).
