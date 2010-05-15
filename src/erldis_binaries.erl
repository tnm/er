-module(erldis_binaries).

-export([to_binary/1]).

to_binary(X) when is_binary(X)  -> X;
to_binary(X) when is_list(X)    -> list_to_binary(X);
to_binary(X) when is_atom(X)    -> list_to_binary(atom_to_list(X));
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X)).
