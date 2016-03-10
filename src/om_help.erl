-module(om_help).
-compile(export_all).

help() -> [{a,[expr],"to parse. Returns {_,_}=Term or {error,_}."},
           {type,[term],"typechecks and returns Type term."},
           {erase,[term],"to untyped term. Returns {Term,Type}."},
           {file,[name],"load file as binary."},
           {str,[binary],"lexical tokenizer."},
           {parse,[tokens],"parse given tokens into {_,_} term."},
           {fst,[{x,y}],"returns first element of a pair."},
           {snd,[{x,y}],"returns second element of a pair."},
           {debug,[bool],"enable/disable debug output."},
           {mode,[name],"select metaverse folder."},
           {modes,[],"list all metaverses."}
          ].
